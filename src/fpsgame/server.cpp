#include "game.h"

namespace game
{
    void parseoptions(vector<const char *> &args)
    {
        loopv(args)
#ifndef STANDALONE
            if(!game::clientoption(args[i]))
#endif
            if(!server::serveroption(args[i]))
                conoutf(CON_ERROR, "unknown command-line option: %s", args[i]);
    }

    const char *gameident() { return "fps"; }
}

extern ENetAddress masteraddress;
extern int nonlocalclients;
extern const char *getclienthostname(int);

namespace server
{
    struct server_entity            // server side version of "entity" type
    {
        int type;
        int spawntime;
        char spawned;
    };

    static const int DEATHMILLIS = 300;

    struct clientinfo;

    struct gameevent
    {
        virtual ~gameevent() {}

        virtual bool flush(clientinfo *ci, int fmillis);
        virtual void process(clientinfo *ci) {}

        virtual bool keepable() const { return false; }
    };

    struct timedevent : gameevent
    {
        int millis;

        bool flush(clientinfo *ci, int fmillis);
    };

    struct hitinfo
    {
        int target;
        int lifesequence;
        int rays;
        float dist;
        vec dir;
    };

    struct shotevent : timedevent
    {
        int id, gun;
        vec from, to;
        vector<hitinfo> hits;

        void process(clientinfo *ci);
    };

    struct explodeevent : timedevent
    {
        int id, gun;
        vector<hitinfo> hits;

        bool keepable() const { return true; }

        void process(clientinfo *ci);
    };

    struct suicideevent : gameevent
    {
        void process(clientinfo *ci);
    };

    struct pickupevent : gameevent
    {
        int ent;

        void process(clientinfo *ci);
    };

    template <int N>
    struct projectilestate
    {
        int projs[N];
        int numprojs;

        projectilestate() : numprojs(0) {}

        void reset() { numprojs = 0; }

        void add(int val)
        {
            if(numprojs>=N) numprojs = 0;
            projs[numprojs++] = val;
        }

        bool remove(int val)
        {
            loopi(numprojs) if(projs[i]==val)
            {
                projs[i] = projs[--numprojs];
                return true;
            }
            return false;
        }
    };

    bool racemode = false;

    struct gamestate : fpsstate
    {
        vec o;
        int state, editstate;
        int lastdeath, deadflush, lastspawn, lifesequence;
        int lastshot;
        projectilestate<8> rockets, grenades;
        int frags, flags, deaths, teamkills, shotdamage, damage, tokens, returned, stolen;
        int lasttimeplayed, timeplayed;
        float effectiveness;

        gamestate() : state(CS_DEAD), editstate(CS_DEAD), lifesequence(0) {}

        bool isalive(int gamemillis)
        {
            return state==CS_ALIVE || (state==CS_DEAD && gamemillis - lastdeath <= DEATHMILLIS);
        }

        bool waitexpired(int gamemillis)
        {
            return gamemillis - lastshot >= gunwait;
        }

        void reset()
        {
            if(state!=CS_SPECTATOR) state = editstate = CS_DEAD;
            maxhealth = 100;
            rockets.reset();
            grenades.reset();

            timeplayed = 0;
            effectiveness = 0;
            frags = flags = deaths = teamkills = shotdamage = damage = tokens = 0;

            lastdeath = 0;

            respawn();
        }

        void respawn()
        {
            fpsstate::respawn();
            if(!racemode) o = vec(-1e10f, -1e10f, -1e10f);
            deadflush = 0;
            lastspawn = -1;
            lastshot = 0;
            tokens = 0;
        }

        void reassign()
        {
            respawn();
            rockets.reset();
            grenades.reset();
        }
    };

    struct savedscore
    {
        uint ip;
        string name;
        int maxhealth, frags, flags, deaths, teamkills, shotdamage, damage;
        int timeplayed;
        float effectiveness;

        void save(gamestate &gs)
        {
            maxhealth = gs.maxhealth;
            frags = gs.frags;
            flags = gs.flags;
            deaths = gs.deaths;
            teamkills = gs.teamkills;
            shotdamage = gs.shotdamage;
            damage = gs.damage;
            timeplayed = gs.timeplayed;
            effectiveness = gs.effectiveness;
        }

        void restore(gamestate &gs)
        {
            if(gs.health==gs.maxhealth) gs.health = maxhealth;
            gs.maxhealth = maxhealth;
            gs.frags = frags;
            gs.flags = flags;
            gs.deaths = deaths;
            gs.teamkills = teamkills;
            gs.shotdamage = shotdamage;
            gs.damage = damage;
            gs.timeplayed = timeplayed;
            gs.effectiveness = effectiveness;
        }
    };

    extern int gamemillis, nextexceeded;

    struct clientinfo
    {
        int clientnum, ownernum, connectmillis, sessionid, overflow;
        string name, team, mapvote;
        int playermodel;
        int modevote;
        int privilege;
        bool connected, local, timesync;
        int gameoffset, lastevent, pushed, exceeded;
        gamestate state;
        vector<gameevent *> events;
        vector<uchar> position, messages;
        uchar *wsdata;
        int wslen;
        vector<clientinfo *> bots;
        int ping, aireinit;
        string clientmap;
        int mapcrc;
        bool warned, gameclip;
        ENetPacket *getdemo, *getmap, *clipboard;
        int lastclipboard, needclipboard;
        int connectauth;
        uint authreq;
        string authname, authdesc;
        void *authchallenge;
        int authkickvictim;
        char *authkickreason;
        bool forcespec, mute, emute, nmute;
        bool isspy, islooser;
        int lasttakeflag;
        int spectimes;

        clientinfo() : getdemo(NULL), getmap(NULL), clipboard(NULL), authchallenge(NULL), authkickreason(NULL) { reset(); }
        ~clientinfo() { events.deletecontents(); cleanclipboard(); cleanauth(); }

        void addevent(gameevent *e)
        {
            if(state.state==CS_SPECTATOR || events.length()>100) delete e;
            else events.add(e);
        }

        enum
        {
            PUSHMILLIS = 3000
        };

        int calcpushrange()
        {
            ENetPeer *peer = getclientpeer(ownernum);
            return PUSHMILLIS + (peer ? peer->roundTripTime + peer->roundTripTimeVariance : ENET_PEER_DEFAULT_ROUND_TRIP_TIME);
        }

        bool checkpushed(int millis, int range)
        {
            return millis >= pushed - range && millis <= pushed + range;
        }

        void scheduleexceeded()
        {
            if(state.state!=CS_ALIVE || !exceeded) return;
            int range = calcpushrange();
            if(!nextexceeded || exceeded + range < nextexceeded) nextexceeded = exceeded + range;
        }

        void setexceeded()
        {
            if(state.state==CS_ALIVE && !exceeded && !checkpushed(gamemillis, calcpushrange())) exceeded = gamemillis;
            scheduleexceeded(); 
        }
            
        void setpushed()
        {
            pushed = max(pushed, gamemillis);
            if(exceeded && checkpushed(exceeded, calcpushrange())) exceeded = 0;
        }
        
        bool checkexceeded()
        {
            return state.state==CS_ALIVE && exceeded && gamemillis > exceeded + calcpushrange();
        }

        void mapchange()
        {
            mapvote[0] = 0;
            modevote = INT_MAX;
            state.reset();
            events.deletecontents();
            overflow = 0;
            timesync = false;
            lastevent = 0;
            exceeded = 0;
            pushed = 0;
            clientmap[0] = '\0';
            mapcrc = 0;
            warned = false;
            gameclip = false;
            state.stolen = 0;
            state.returned = 0;
        }

        void reassign()
        {
            state.reassign();
            events.deletecontents();
            timesync = false;
            lastevent = 0;
        }

        void cleanclipboard(bool fullclean = true)
        {
            if(clipboard) { if(--clipboard->referenceCount <= 0) enet_packet_destroy(clipboard); clipboard = NULL; }
            if(fullclean) lastclipboard = 0;
        }

        void cleanauthkick()
        {
            authkickvictim = -1;
            DELETEA(authkickreason);
        }

        void cleanauth(bool full = true)
        {
            authreq = 0;
            if(authchallenge) { freechallenge(authchallenge); authchallenge = NULL; }
            if(full) cleanauthkick();
        }

        void reset()
        {
            name[0] = team[0] = 0;
            playermodel = -1;
            privilege = PRIV_NONE;
            connected = local = false;
            connectauth = 0;
            position.setsize(0);
            messages.setsize(0);
            ping = 0;
            aireinit = 0;
            needclipboard = 0;
            cleanclipboard();
            cleanauth();
            mapchange();
        }

        int geteventmillis(int servmillis, int clientmillis)
        {
            if(!timesync || (events.empty() && state.waitexpired(servmillis)))
            {
                timesync = true;
                gameoffset = servmillis - clientmillis;
                return servmillis;
            }
            else return gameoffset + clientmillis;
        }
    };

    struct ban
    {
        int time, expire;
        uint ip;
    };

    namespace aiman
    {
        extern void removeai(clientinfo *ci);
        extern void clearai();
        extern void checkai();
        extern void reqadd(clientinfo *ci, int skill);
        extern void reqdel(clientinfo *ci);
        extern void setbotlimit(clientinfo *ci, int limit);
        extern void setbotbalance(clientinfo *ci, bool balance);
        extern void changemap();
        extern void addclient(clientinfo *ci);
        extern void changeteam(clientinfo *ci);
    }

    #define MM_MODE 0xF
    #define MM_AUTOAPPROVE 0x1000
    #define MM_PRIVSERV (MM_MODE | MM_AUTOAPPROVE)
    #define MM_PUBSERV ((1<<MM_OPEN) | (1<<MM_VETO))
    #define MM_COOPSERV (MM_AUTOAPPROVE | MM_PUBSERV | (1<<MM_LOCKED))

    bool notgotitems = true;        // true when map has changed and waiting for clients to send item
    int gamemode = 0;
    int gamemillis = 0, gamelimit = 0, nextexceeded = 0, gamespeed = 100;
    bool gamepaused = false, shouldstep = true;

    string smapname = "";
    int interm = 0;
    enet_uint32 lastsend = 0;
    int mastermode = MM_OPEN, mastermask = MM_PRIVSERV;
    stream *mapdata = NULL;

    vector<uint> allowedips;
    vector<ban> bannedips;

    void addban(uint ip, int expire)
    {
        allowedips.removeobj(ip);
        ban b;
        b.time = totalmillis;
        b.expire = totalmillis + expire;
        b.ip = ip;
        loopv(bannedips) if(b.expire < bannedips[i].expire) { bannedips.insert(i, b); return; }
        bannedips.add(b);
    }

    vector<clientinfo *> connects, clients, bots;

    void kickclients(uint ip, clientinfo *actor = NULL, int priv = PRIV_NONE)
    {
        loopvrev(clients)
        {
            clientinfo &c = *clients[i];
            if(c.state.aitype != AI_NONE || c.privilege >= PRIV_ADMIN || c.local) continue;
            if(actor && ((c.privilege > priv && !actor->local) || c.clientnum == actor->clientnum)) continue;
            if(getclientip(c.clientnum) == ip) disconnect_client(c.clientnum, DISC_KICK);
        }
    }
 
    struct maprotation
    {
        static int exclude;
        int modes;
        string map;
        
        int calcmodemask() const { return modes&(1<<NUMGAMEMODES) ? modes & ~exclude : modes; }
        bool hasmode(int mode, int offset = STARTGAMEMODE) const { return (calcmodemask() & (1 << (mode-offset))) != 0; }

        int findmode(int mode) const
        {
            if(!hasmode(mode)) loopi(NUMGAMEMODES) if(hasmode(i, 0)) return i+STARTGAMEMODE;
            return mode;
        }

        bool match(int reqmode, const char *reqmap) const
        {
            return hasmode(reqmode) && (!map[0] || !reqmap[0] || !strcmp(map, reqmap));
        }

        bool includes(const maprotation &rot) const
        {
            return rot.modes == modes ? rot.map[0] && !map[0] : (rot.modes & modes) == rot.modes;
        }
    };
    int maprotation::exclude = 0;
    vector<maprotation> maprotations;
    int curmaprotation = 0;

    VAR(lockmaprotation, 0, 0, 2);

    void maprotationreset()
    {
        maprotations.setsize(0);
        curmaprotation = 0;
        maprotation::exclude = 0;
    }

    void nextmaprotation()
    {
        curmaprotation++;
        if(maprotations.inrange(curmaprotation) && maprotations[curmaprotation].modes) return;
        do curmaprotation--;
        while(maprotations.inrange(curmaprotation) && maprotations[curmaprotation].modes);
        curmaprotation++;
    }

    int findmaprotation(int mode, const char *map)
    {
        for(int i = max(curmaprotation, 0); i < maprotations.length(); i++)
        {
            maprotation &rot = maprotations[i];
            if(!rot.modes) break;
            if(rot.match(mode, map)) return i;
        }
        int start;
        for(start = max(curmaprotation, 0) - 1; start >= 0; start--) if(!maprotations[start].modes) break;
        start++;
        for(int i = start; i < curmaprotation; i++)
        {
            maprotation &rot = maprotations[i];
            if(!rot.modes) break;
            if(rot.match(mode, map)) return i;
        }
        int best = -1;
        loopv(maprotations)
        {
            maprotation &rot = maprotations[i];
            if(rot.match(mode, map) && (best < 0 || maprotations[best].includes(rot))) best = i;
        }
        return best;
    }

    bool searchmodename(const char *haystack, const char *needle)
    {
        if(!needle[0]) return true;
        do
        {
            if(needle[0] != '.')
            {
                haystack = strchr(haystack, needle[0]);
                if(!haystack) break;
                haystack++;
            }
            const char *h = haystack, *n = needle+1;
            for(; *h && *n; h++)
            {
                if(*h == *n) n++;
                else if(*h != ' ') break; 
            }
            if(!*n) return true;
            if(*n == '.') return !*h;
        } while(needle[0] != '.');
        return false;
    }

    int genmodemask(vector<char *> &modes)
    {
        int modemask = 0;
        loopv(modes)
        {
            const char *mode = modes[i];
            int op = mode[0];
            switch(mode[0])
            {
                case '*':
                    modemask |= 1<<NUMGAMEMODES;
                    loopk(NUMGAMEMODES) if(m_checknot(k+STARTGAMEMODE, M_DEMO|M_EDIT|M_LOCAL)) modemask |= 1<<k;
                    continue;
                case '!':
                    mode++;
                    if(mode[0] != '?') break;
                case '?':
                    mode++;
                    loopk(NUMGAMEMODES) if(searchmodename(gamemodes[k].name, mode))
                    {
                        if(op == '!') modemask &= ~(1<<k);
                        else modemask |= 1<<k;
                    }
                    continue;
            }
            int modenum = INT_MAX;
            if(isdigit(mode[0])) modenum = atoi(mode);
            else loopk(NUMGAMEMODES) if(searchmodename(gamemodes[k].name, mode)) { modenum = k+STARTGAMEMODE; break; }
            if(!m_valid(modenum)) continue;
            switch(op)
            {
                case '!': modemask &= ~(1 << (modenum - STARTGAMEMODE)); break;
                default: modemask |= 1 << (modenum - STARTGAMEMODE); break;
            }
        }
        return modemask;
    }
         
    bool addmaprotation(int modemask, const char *map)
    {
        if(!map[0]) loopk(NUMGAMEMODES) if(modemask&(1<<k) && !m_check(k+STARTGAMEMODE, M_EDIT)) modemask &= ~(1<<k);
        if(!modemask) return false;
        if(!(modemask&(1<<NUMGAMEMODES))) maprotation::exclude |= modemask;
        maprotation &rot = maprotations.add();
        rot.modes = modemask;
        copystring(rot.map, map);
        return true;
    }
        
    void addmaprotations(tagval *args, int numargs)
    {
        vector<char *> modes, maps;
        for(int i = 0; i + 1 < numargs; i += 2)
        {
            explodelist(args[i].getstr(), modes);
            explodelist(args[i+1].getstr(), maps);
            int modemask = genmodemask(modes);
            if(maps.length()) loopvj(maps) addmaprotation(modemask, maps[j]);
            else addmaprotation(modemask, "");
            modes.deletearrays();
            maps.deletearrays();
        }
        if(maprotations.length() && maprotations.last().modes)
        {
            maprotation &rot = maprotations.add();
            rot.modes = 0;
            rot.map[0] = '\0';
        }
    }
    
    COMMAND(maprotationreset, "");
    COMMANDN(maprotation, addmaprotations, "ss2V");

    struct demofile
    {
        string info;
        uchar *data;
        int len;
    };

    vector<demofile> demos;

    bool demonextmatch = false;
    stream *demotmp = NULL, *demorecord = NULL, *demoplayback = NULL;
    int nextplayback = 0, demomillis = 0;

    VAR(maxdemos, 0, 5, 25);
    VAR(maxdemosize, 0, 16, 64);
    VAR(restrictdemos, 0, 1, 1);

    VAR(restrictpausegame, 0, 1, 1);
    VAR(restrictgamespeed, 0, 1, 1);

    SVAR(serverdesc, "");
    SVAR(serverpass, "");
    SVAR(adminpass, "");
    VARF(publicserver, 0, 0, 2, {
        switch(publicserver)
        {
            case 0: default: mastermask = MM_PRIVSERV; break;
            case 1: mastermask = MM_PUBSERV; break;
            case 2: mastermask = MM_COOPSERV; break;
        }
    });
    SVAR(servermotd, "");

    struct teamkillkick
    {
        int modes, limit, ban;

        bool match(int mode) const
        {
            return (modes&(1<<(mode-STARTGAMEMODE)))!=0;
        }

        bool includes(const teamkillkick &tk) const
        {
            return tk.modes != modes && (tk.modes & modes) == tk.modes;
        }
    };
    vector<teamkillkick> teamkillkicks;

    void teamkillkickreset()
    {
        teamkillkicks.setsize(0);
    }

    void addteamkillkick(char *modestr, int *limit, int *ban)
    {
        vector<char *> modes;
        explodelist(modestr, modes);
        teamkillkick &kick = teamkillkicks.add();
        kick.modes = genmodemask(modes);
        kick.limit = *limit;
        kick.ban = *ban > 0 ? *ban*60000 : (*ban < 0 ? 0 : 30*60000); 
        modes.deletearrays();
    }

    COMMAND(teamkillkickreset, "");
    COMMANDN(teamkillkick, addteamkillkick, "sii");

    struct teamkillinfo
    {
        uint ip;
        int teamkills;
    };
    vector<teamkillinfo> teamkills;
    bool shouldcheckteamkills = false;

    void addteamkill(clientinfo *actor, clientinfo *victim, int n)
    {
        if(!m_timed || actor->state.aitype != AI_NONE || actor->local || actor->privilege || (victim && victim->state.aitype != AI_NONE)) return;
        shouldcheckteamkills = true;
        uint ip = getclientip(actor->clientnum);
        loopv(teamkills) if(teamkills[i].ip == ip) 
        { 
            teamkills[i].teamkills += n;
            return;
        }
        teamkillinfo &tk = teamkills.add();
        tk.ip = ip;
        tk.teamkills = n;
    }

    void checkteamkills()
    {
        teamkillkick *kick = NULL;
        if(m_timed) loopv(teamkillkicks) if(teamkillkicks[i].match(gamemode) && (!kick || kick->includes(teamkillkicks[i])))
            kick = &teamkillkicks[i];
        if(kick) loopvrev(teamkills)
        {
            teamkillinfo &tk = teamkills[i];
            if(tk.teamkills >= kick->limit)
            {
                if(kick->ban > 0) addban(tk.ip, kick->ban);
                kickclients(tk.ip);
                teamkills.removeunordered(i);
            }
        }
        shouldcheckteamkills = false;
    }

    void *newclientinfo() { return new clientinfo; }
    void deleteclientinfo(void *ci) { delete (clientinfo *)ci; }

    clientinfo *getinfo(int n)
    {
        if(n < MAXCLIENTS) return (clientinfo *)getclientinfo(n);
        n -= MAXCLIENTS;
        return bots.inrange(n) ? bots[n] : NULL;
    }

    uint mcrc = 0;
    vector<entity> ments;
    vector<server_entity> sents;
    vector<savedscore> scores;

    int msgsizelookup(int msg)
    {
        static int sizetable[NUMMSG] = { -1 };
        if(sizetable[0] < 0)
        {
            memset(sizetable, -1, sizeof(sizetable));
            for(const int *p = msgsizes; *p >= 0; p += 2) sizetable[p[0]] = p[1];
        }
        return msg >= 0 && msg < NUMMSG ? sizetable[msg] : -1;
    }

    const char *modename(int n, const char *unknown)
    {
        if(m_valid(n)) return gamemodes[n - STARTGAMEMODE].name;
        return unknown;
    }

    const char *mastermodename(int n, const char *unknown)
    {
        return (n>=MM_START && size_t(n-MM_START)<sizeof(mastermodenames)/sizeof(mastermodenames[0])) ? mastermodenames[n-MM_START] : unknown;
    }

    const char *privname(int type)
    {
        switch(type)
        {
            case PRIV_OWNER: return "owner";
            case PRIV_ADMIN: return "admin";
            case PRIV_AUTH: return "auth";
            case PRIV_MASTER: return "master";
            default: return "unknown";
        }
    }

    void sendservmsg(const char *s) { sendf(-1, 1, "ris", N_SERVMSG, s); }
    void sendservmsgf(const char *fmt, ...)
    {
         defvformatstring(s, fmt, fmt);
         sendf(-1, 1, "ris", N_SERVMSG, s);
    }

    void resetitems()
    {
        mcrc = 0;
        ments.setsize(0);
        sents.setsize(0);
        //cps.reset();
    }

    bool serveroption(const char *arg)
    {
        if(arg[0]=='-') switch(arg[1])
        {
            case 'n': setsvar("serverdesc", &arg[2]); return true;
            case 'y': setsvar("serverpass", &arg[2]); return true;
            case 'p': setsvar("adminpass", &arg[2]); return true;
            case 'o': setvar("publicserver", atoi(&arg[2])); return true;
        }
        return false;
    }

    VARF(defaultgamemode, 0, 0, NUMGAMEMODES+STARTGAMEMODE, { if(!m_mp(defaultgamemode)) defaultgamemode = 0; });

    void serverinit()
    {
        smapname[0] = '\0';
        gamemode = defaultgamemode;
        resetitems();
    }

    int numclients(int exclude = -1, bool nospec = true, bool noai = true, bool priv = false)
    {
        int n = 0;
        loopv(clients) 
        {
            clientinfo *ci = clients[i];
            if(ci->isspy) continue;
            if(ci->clientnum!=exclude && (!nospec || ci->state.state!=CS_SPECTATOR || (priv && (ci->privilege || ci->local))) && (!noai || ci->state.aitype == AI_NONE)) n++;
        }
        return n;
    }

    bool duplicatename(clientinfo *ci, char *name)
    {
        if(!name) name = ci->name;
        loopv(clients) if(clients[i]!=ci && !clients[i]->isspy && !strcmp(name, clients[i]->name)) return true;
        return false;
    }

    const char *colorname(clientinfo *ci, char *name = NULL)
    {
        if(!name) name = ci->name;
        if(name[0] && !duplicatename(ci, name) && ci->state.aitype == AI_NONE) return name;
        static string cname[3];
        static int cidx = 0;
        cidx = (cidx+1)%3;
        formatstring(cname[cidx])(ci->state.aitype == AI_NONE ? "%s \fs\f5(%d)\fr" : "%s \fs\f5[%d]\fr", name, ci->clientnum);
        return cname[cidx];
    }

    struct servmode
    {
        virtual ~servmode() {}

        virtual void entergame(clientinfo *ci) {}
        virtual void leavegame(clientinfo *ci, bool disconnecting = false) {}

        virtual void moved(clientinfo *ci, const vec &oldpos, bool oldclip, const vec &newpos, bool newclip) {}
        virtual bool canspawn(clientinfo *ci, bool connecting = false) { return true; }
        virtual void spawned(clientinfo *ci) {}
        virtual int fragvalue(clientinfo *victim, clientinfo *actor)
        {
            if(victim==actor || isteam(victim->team, actor->team)) return -1;
            return 1;
        }
        virtual void died(clientinfo *victim, clientinfo *actor) {}
        virtual bool canchangeteam(clientinfo *ci, const char *oldteam, const char *newteam) { return true; }
        virtual void changeteam(clientinfo *ci, const char *oldteam, const char *newteam) {}
        virtual void initclient(clientinfo *ci, packetbuf &p, bool connecting) {}
        virtual void update() {}
        virtual void cleanup() {}
        virtual void setup() {}
        virtual void newmap() {}
        virtual void intermission() {}
        virtual bool hidefrags() { return false; }
        virtual int getteamscore(const char *team) { return 0; }
        virtual void getteamscores(vector<teamscore> &scores) {}
        virtual bool extinfoteam(const char *team, ucharbuf &p) { return false; }
    };

    // Generic notifics to clients

    void sendmsg(int cn, const char * msg) { sendf(cn, 1, "ris", N_SERVMSG, msg); }
    void sendmsgf(int cn, const char * msg, ...) { defvformatstring(s, msg, msg); sendmsg(cn, s); }
    void sendmsg(clientinfo * ci, const char * msg) { sendmsg(ci ? ci->ownernum : -1, msg); }
    void sendmsgf(clientinfo * ci, const char * msg, ...) { defvformatstring(s, msg, msg); sendmsg(ci ? ci->ownernum : -1, s); }

    // Generic notifics to all clients who have at least a specified privilege level

    void sendprivmsg(int priv, const char * msg) { loopv(clients) if(clients[i]->privilege >= priv) sendmsg(clients[i], msg); }
    void sendprivmsgf(int priv, const char * msg, ...) { defvformatstring(s, msg, msg); sendprivmsg(priv, s); }
    void sendprivmsg(clientinfo * ci, const char * msg) { sendprivmsg(ci ? ci->privilege : 0, msg); }
    void sendprivmsgf(clientinfo * ci, const char * msg, ...) { defvformatstring(s, msg, msg); sendprivmsg(ci ? ci->privilege : 0, s); }

    struct pban
    {
        string IP;
        string Reason;
        bool hide;
    };
    vector<pban> pbans;

    struct rpban
    {
        string Range;
        string Reason;
        int type;
    };
    vector<rpban> rpbans;

    void addrpban24(const char *range, const char *reason)
    {
        loopv(rpbans) if(!strcmp(rpbans[i].Range, range)) return;
        rpban &_rpban = rpbans.add();
        copystring(_rpban.Range, range);
        copystring(_rpban.Reason, reason);
        _rpban.type = 24;
        for(int i = 1; i <= 255; i++)
        {
            pban &_pban = pbans.add();
            defformatstring(ip)("%s.%i", range, i);
            copystring(_pban.IP, (const char *)ip);
            copystring(_pban.Reason, reason);
            _pban.hide = true;
        }
    }

    void addrpban16(const char *range, const char *reason)
    {
        loopv(rpbans) if(!strcmp(rpbans[i].Range, range)) return;
        rpban &_rpban = rpbans.add();
        copystring(_rpban.Range, range);
        copystring(_rpban.Reason, reason);
        _rpban.type = 16;
        for(int i = 1; i <= 255; i++)
        {
            for(int j = 1; j <= 255; j++)
            {
                pban &_pban = pbans.add();
                defformatstring(ip)("%s.%i.%i", range, i, j);
                copystring(_pban.IP, (const char *)ip);
                copystring(_pban.Reason, reason);
                _pban.hide = true;
            }
        }
    }

    void addrpban8(const char *range, const char *reason)
    {
        loopv(rpbans) if(!strcmp(rpbans[i].Range, range)) return;
        rpban &_rpban = rpbans.add();
        copystring(_rpban.Range, range);
        copystring(_rpban.Reason, reason);
        _rpban.type = 8;
        for(int i = 1; i <= 255; i++)
        {
            for(int j = 1; j <= 255; j++)
            {
                for(int z = 1; z <= 255; z++)
                {
                    pban &_pban = pbans.add();
                    defformatstring(ip)("%s.%i.%i.%i", range, i, j, z);
                    copystring(_pban.IP, (const char *)ip);
                    copystring(_pban.Reason, reason);
                    _pban.hide = true;
                }
            }
        }
    }

    void addpban(const char *ip, const char *reason)
    {
        loopv(pbans) if(!strcmp(pbans[i].IP, ip)) return;
        pban &_pban = pbans.add();
        copystring(_pban.IP, ip);
        copystring(_pban.Reason, reason);
        _pban.hide = false;
    }

    inline void addpbanf(const char *ip, const char *reason, ...)
        { defvformatstring(s, reason, reason); addpban(ip, s); }

    inline void addpban(char *ip, char *reason)
        { addpban((const char *)ip, (const char *)reason); }

    inline void addpbanf(char *ip, char *reason, ...)
        { defvformatstring(s, reason, reason); addpban(ip, s); }

    inline bool ispban(const char *ip)
        { loopv(pbans) if(!strcmp(pbans[i].IP, ip)) return true; return false; }

    inline bool ispban(char *ip)
        { return ispban((const char *)ip); }

    enum cheat_id_t
    {
        FLAGHACK = 0,
        EDITMODE, EDITMSG, EDITENT,
        GENERICGUNHACK, GUNHACKRELOAD, GUNHACKRANGE, GUNHACKRAYS, GUNHACKNOAMMO, GUNHACKNOTALIVE, NONINSTAGUNININSTA, UNKNOWNGUN,
//        MNOITEMS, MNOAMMO,
        MESSAGESIZE//, WRONGMESSAGE,
//        SOUNDHACK
    };

    VAR(anticheat, 0, 1, 1);
    VAR(ac_pban_clients, 0, 1, 1);
    VAR(ac_public_message, 0, 0, 1);
    VAR(flagrun_min_millis, 0, 500, 1000);

    void ac(clientinfo * ci, cheat_id_t cheat_id, int eint = 0, const char *echar = "")
    {
        return;
        if(!anticheat) return;
        if(cheat_id < FLAGHACK || cheat_id > /*SOUNDHACK*/MESSAGESIZE) return;
        string msg;
        switch(cheat_id)
        {
            case FLAGHACK:
            {
                formatstring(msg)("Flaghack (taken millis = %i, minimal millis = %i).", eint, flagrun_min_millis);
                break;
            }
            case EDITMODE:
            case EDITMSG:
            case EDITENT:
            {
                formatstring(msg)("Edit-packets in non coop-edit gamemode (%s).", 
                    cheat_id == EDITMODE ? "edit-toggle" : (
                        cheat_id == EDITMSG ? "generic edit-message" : "entity editing"
                    )
                );
                break;
            }
            case GENERICGUNHACK:
            {
                copystring(msg, "Generic gun-hack");
                break;
            }
            case GUNHACKRELOAD:
            case GUNHACKRANGE:
            case GUNHACKRAYS:
            case NONINSTAGUNININSTA:
            case UNKNOWNGUN:
            {
                formatstring(msg)("Gun-hack (%s (%i))",
                    cheat_id == GUNHACKRELOAD ? "Modified weapon reload time" : (
                        cheat_id == GUNHACKRANGE ? "Modified weapon range" : (
                            cheat_id == GUNHACKRAYS ? "Modified weapon rays" : (
                                cheat_id == NONINSTAGUNININSTA ? "Non instagib-gun in instagib-gamemode." : "Unknown gun."
                            )
                        )
                    ),
                    eint
                );
                break;
            }
            case GUNHACKNOAMMO: // Tollerate this.
            case GUNHACKNOTALIVE:
            {
                return;
            }
/*            case MNOITEMS:
            case MNOAMMO:
            {
                formatstring(msg)("Trying to pick up %s in a m_no%s gamemode.", 
                    cheat_id == MNOITEMS ? "items" : "ammunition",
                    cheat_id == MNOITEMS ? "items" : "ammo"
                );
                break;
            }*/
            case MESSAGESIZE:
            //case WRONGMESSAGE:
            {
                formatstring(msg)(/*"Message error (%s (%i))"*/"Message error (wrong message size (%i))",
//                    cheat_id == MESSAGESIZE ? "wrong message size" : "unknown message type",
                    eint
                );
                break;
            }
/*            case SOUNDHACK:
            {
                formatstring(msg)("Unknown sound %i", eint);
                break;
            }*/
            default:
            {
                return;
            }
        }
        if(ac_public_message) sendservmsgf("Anti-Cheat - Cheater detected: %s - Cheat: %s", colorname(ci), msg);
        else sendprivmsgf(PRIV_ADMIN, "Anti-Cheat - Cheater detected: %s - Cheat: %s", colorname(ci), msg);
        if(ac_pban_clients) addpbanf(getclienthostname(ci->ownernum), "Banned by Anti-Cheat for cheating - Cheat: %s", msg);
        disconnect_client(ci->ownernum, ac_pban_clients ? DISC_IPBAN : DISC_MSGERR);
    }

    struct fr
    {
        string scorer;
        string map;
        int scoremillis;
        int mode;
    };
    vector<fr> frs;

    void addflagrun(const char *name, const char *frmap, int frmillis, int frmode)
    {
        loopv(frs)
        {
            if(!strcmp(frs[i].map, smapname) && frs[i].mode == gamemode) frs.remove(i);
        }
        int id = frs.length();
        frs.add();
        copystring(frs[id].scorer, name);
        copystring(frs[id].map, frmap);
        frs[id].scoremillis = frmillis;
        frs[id].mode = frmode;
    }

    ICOMMAND(flagrun, "ssii", (const char *n, const char *m, int *frmi, int *frmo), {
        addflagrun(n, m, *frmi, *frmo);
    });

    void flagrun(clientinfo *ci, int frmillis)
    {
        if(gamespeed!=100) return;
        if(frmillis<flagrun_min_millis || !frmillis)
        {
            ac(ci, FLAGHACK, frmillis);
            return;
        }
        fr *cur = 0;
        loopv(frs) if(!strcmp(frs[i].map, smapname) && frs[i].mode == gamemode) cur = &frs[i];
        bool isbest = false;
        if(!cur)
        {
            addflagrun(ci->name, smapname, frmillis, gamemode);
            isbest = true;
        }
        else
        {
            if(cur->scoremillis > frmillis)
            {
                copystring(cur->scorer, ci->name);
                cur->scoremillis = frmillis;
                isbest = true;
            }
        }
        string message;
        double frseconds = (double)frmillis/1000.0;
        if(isbest)
        {
            formatstring(message)("\f0[FLAGRUN]\f7: \f1%s \f7has scored the \f2flag \f7in \f0%.3f \f6seconds \f7(\f3best\f7).",
                colorname(ci),
                frseconds
            );
        }
        else
        {
            double bestfrseconds = (double)cur->scoremillis/1000;
            formatstring(message)("\f0[FLAGRUN]\f7: \f1%s \f7has scored the \f2flag \f7in \f0%.3f \f6seconds \f7(\f3best\f7: \f1%s\f7, \f0%.3f \f6seconds\f7).",
                colorname(ci),
                frseconds,
                cur->scorer,
                bestfrseconds
            );
        }
        sendservmsg(message);
    }

    #define SERVMODE 1
    #include "capture.h"
    #include "ctf.h"
    #include "collect.h"

    captureservmode capturemode;
    ctfservmode ctfmode;
    collectservmode collectmode;
    servmode *smode = NULL;

    bool canspawnitem(int type) { return !m_noitems && (type>=I_SHELLS && type<=I_QUAD && (!m_noammo || type<I_SHELLS || type>I_CARTRIDGES)); }

    int spawntime(int type)
    {
        if(m_classicsp) return INT_MAX;
        int np = numclients(-1, true, false);
        np = np<3 ? 4 : (np>4 ? 2 : 3);         // spawn times are dependent on number of players
        int sec = 0;
        switch(type)
        {
            case I_SHELLS:
            case I_BULLETS:
            case I_ROCKETS:
            case I_ROUNDS:
            case I_GRENADES:
            case I_CARTRIDGES: sec = np*4; break;
            case I_HEALTH: sec = np*5; break;
            case I_GREENARMOUR: sec = 20; break;
            case I_YELLOWARMOUR: sec = 30; break;
            case I_BOOST: sec = 60; break;
            case I_QUAD: sec = 70; break;
        }
        return sec*1000;
    }

    bool delayspawn(int type)
    {
        switch(type)
        {
            case I_GREENARMOUR:
            case I_YELLOWARMOUR:
                return !m_classicsp;
            case I_BOOST:
            case I_QUAD:
                return true;
            default:
                return false;
        }
    }
 
    bool pickup(int i, int sender)         // server side item pickup, acknowledge first client that gets it
    {
        if((m_timed && gamemillis>=gamelimit) || !sents.inrange(i) || !sents[i].spawned) return false;
        clientinfo *ci = getinfo(sender);
        if(!ci || (!ci->local && !ci->state.canpickup(sents[i].type))) return false;
        sents[i].spawned = false;
        sents[i].spawntime = spawntime(sents[i].type);
        sendf(-1, 1, "ri3", N_ITEMACC, i, sender);
        ci->state.pickup(sents[i].type);
        return true;
    }

    static hashset<teaminfo> teaminfos;

    void clearteaminfo()
    {
        teaminfos.clear();
    }

    bool teamhasplayers(const char *team) { loopv(clients) if(!strcmp(clients[i]->team, team)) return true; return false; }

    bool pruneteaminfo()
    {
        int oldteams = teaminfos.numelems;
        enumerates(teaminfos, teaminfo, old,
            if(!old.frags && !teamhasplayers(old.team)) teaminfos.remove(old.team);
        );
        return teaminfos.numelems < oldteams;
    }

    teaminfo *addteaminfo(const char *team)
    {
        teaminfo *t = teaminfos.access(team);
        if(!t)
        {
            if(teaminfos.numelems >= MAXTEAMS && !pruneteaminfo()) return NULL;
            t = &teaminfos[team];
            copystring(t->team, team, sizeof(t->team));
            t->frags = 0;
        }
        return t;
    }

    clientinfo *choosebestclient(float &bestrank)
    {
        clientinfo *best = NULL;
        bestrank = -1;
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.timeplayed<0) continue;
            float rank = ci->state.state!=CS_SPECTATOR ? ci->state.effectiveness/max(ci->state.timeplayed, 1) : -1;
            if(!best || rank > bestrank) { best = ci; bestrank = rank; }
        }
        return best;
    }

    void autoteam()
    {
        static const char * const teamnames[2] = {"good", "evil"};
        vector<clientinfo *> team[2];
        float teamrank[2] = {0, 0};
        for(int round = 0, remaining = clients.length(); remaining>=0; round++)
        {
            int first = round&1, second = (round+1)&1, selected = 0;
            while(teamrank[first] <= teamrank[second])
            {
                float rank;
                clientinfo *ci = choosebestclient(rank);
                if(!ci) break;
                if(smode && smode->hidefrags()) rank = 1;
                else if(selected && rank<=0) break;
                ci->state.timeplayed = -1;
                team[first].add(ci);
                if(rank>0) teamrank[first] += rank;
                selected++;
                if(rank<=0) break;
            }
            if(!selected) break;
            remaining -= selected;
        }
        loopi(sizeof(team)/sizeof(team[0]))
        {
            addteaminfo(teamnames[i]);
            loopvj(team[i])
            {
                clientinfo *ci = team[i][j];
                if(!strcmp(ci->team, teamnames[i])) continue;
                copystring(ci->team, teamnames[i], MAXTEAMLEN+1);
                sendf(ci->isspy ? ci->ownernum : -1, 1, "riisi", N_SETTEAM, ci->clientnum, teamnames[i], -1);
            }
        }
    }

    struct teamrank
    {
        const char *name;
        float rank;
        int clients;

        teamrank(const char *name) : name(name), rank(0), clients(0) {}
    };

    const char *chooseworstteam(const char *suggest = NULL, clientinfo *exclude = NULL)
    {
        teamrank teamranks[2] = { teamrank("good"), teamrank("evil") };
        const int numteams = sizeof(teamranks)/sizeof(teamranks[0]);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci==exclude || ci->state.aitype!=AI_NONE || ci->state.state==CS_SPECTATOR || !ci->team[0]) continue;
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            ci->state.lasttimeplayed = lastmillis;

            loopj(numteams) if(!strcmp(ci->team, teamranks[j].name))
            {
                teamrank &ts = teamranks[j];
                ts.rank += ci->state.effectiveness/max(ci->state.timeplayed, 1);
                ts.clients++;
                break;
            }
        }
        teamrank *worst = &teamranks[numteams-1];
        loopi(numteams-1)
        {
            teamrank &ts = teamranks[i];
            if(smode && smode->hidefrags())
            {
                if(ts.clients < worst->clients || (ts.clients == worst->clients && ts.rank < worst->rank)) worst = &ts;
            }
            else if(ts.rank < worst->rank || (ts.rank == worst->rank && ts.clients < worst->clients)) worst = &ts;
        }
        return worst->name;
    }

    void prunedemos(int extra = 0)
    {
        int n = clamp(demos.length() + extra - maxdemos, 0, demos.length());
        if(n <= 0) return;
        loopi(n) delete[] demos[i].data;
        demos.remove(0, n);
    }
 
    void adddemo()
    {
        if(!demotmp) return;
        int len = (int)min(demotmp->size(), stream::offset((maxdemosize<<20) + 0x10000));
        demofile &d = demos.add();
        time_t t = time(NULL);
        char *timestr = ctime(&t), *trim = timestr + strlen(timestr);
        while(trim>timestr && iscubespace(*--trim)) *trim = '\0';
        formatstring(d.info)("%s: %s, %s, %.2f%s", timestr, modename(gamemode), smapname, len > 1024*1024 ? len/(1024*1024.f) : len/1024.0f, len > 1024*1024 ? "MB" : "kB");
        sendservmsgf("demo \"%s\" recorded", d.info);
        d.data = new uchar[len];
        d.len = len;
        demotmp->seek(0, SEEK_SET);
        demotmp->read(d.data, len);
        DELETEP(demotmp);
    }
        
    void enddemorecord()
    {
        if(!demorecord) return;

        DELETEP(demorecord);

        if(!demotmp) return;
        if(!maxdemos || !maxdemosize) { DELETEP(demotmp); return; }

        prunedemos(1);
        adddemo();
    }

    void writedemo(int chan, void *data, int len)
    {
        if(!demorecord) return;
        int stamp[3] = { gamemillis, chan, len };
        lilswap(stamp, 3);
        demorecord->write(stamp, sizeof(stamp));
        demorecord->write(data, len);
        if(demorecord->rawtell() >= (maxdemosize<<20)) enddemorecord();
    }

    void recordpacket(int chan, void *data, int len)
    {
        writedemo(chan, data, len);
    }

    int welcomepacket(packetbuf &p, clientinfo *ci);
    void sendwelcome(clientinfo *ci);

    void setupdemorecord()
    {
        if(!m_mp(gamemode) || m_edit) return;

        demotmp = opentempfile("demorecord", "w+b");
        if(!demotmp) return;

        stream *f = opengzfile(NULL, "wb", demotmp);
        if(!f) { DELETEP(demotmp); return; }

        sendservmsg("recording demo");

        demorecord = f;

        demoheader hdr;
        memcpy(hdr.magic, DEMO_MAGIC, sizeof(hdr.magic));
        hdr.version = DEMO_VERSION;
        hdr.protocol = PROTOCOL_VERSION;
        lilswap(&hdr.version, 2);
        demorecord->write(&hdr, sizeof(demoheader));

        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        welcomepacket(p, NULL);
        writedemo(1, p.buf, p.len);
    }

    void listdemos(int cn)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        putint(p, N_SENDDEMOLIST);
        putint(p, demos.length());
        loopv(demos) sendstring(demos[i].info, p);
        sendpacket(cn, 1, p.finalize());
    }

    void cleardemos(int n)
    {
        if(!n)
        {
            loopv(demos) delete[] demos[i].data;
            demos.shrink(0);
            sendservmsg("cleared all demos");
        }
        else if(demos.inrange(n-1))
        {
            delete[] demos[n-1].data;
            demos.remove(n-1);
            sendservmsgf("cleared demo %d", n);
        }
    }

    static void freegetmap(ENetPacket *packet)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->getmap == packet) ci->getmap = NULL;
        }
    }

    static void freegetdemo(ENetPacket *packet)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->getdemo == packet) ci->getdemo = NULL;
        }
    }

    void senddemo(clientinfo *ci, int num)
    {
        if(ci->getdemo) return;
        if(!num) num = demos.length();
        if(!demos.inrange(num-1)) return;
        demofile &d = demos[num-1];
        if((ci->getdemo = sendf(ci->clientnum, 2, "rim", N_SENDDEMO, d.len, d.data)))
            ci->getdemo->freeCallback = freegetdemo;
    }

    void enddemoplayback()
    {
        if(!demoplayback) return;
        DELETEP(demoplayback);

        loopv(clients) sendf(clients[i]->clientnum, 1, "ri3", N_DEMOPLAYBACK, 0, clients[i]->clientnum);

        sendservmsg("demo playback finished");

        loopv(clients) sendwelcome(clients[i]);
    }

    void setupdemoplayback()
    {
        if(demoplayback) return;
        demoheader hdr;
        string msg;
        msg[0] = '\0';
        defformatstring(file)("%s.dmo", smapname);
        demoplayback = opengzfile(file, "rb");
        if(!demoplayback) formatstring(msg)("could not read demo \"%s\"", file);
        else if(demoplayback->read(&hdr, sizeof(demoheader))!=sizeof(demoheader) || memcmp(hdr.magic, DEMO_MAGIC, sizeof(hdr.magic)))
            formatstring(msg)("\"%s\" is not a demo file", file);
        else
        {
            lilswap(&hdr.version, 2);
            if(hdr.version!=DEMO_VERSION) formatstring(msg)("demo \"%s\" requires an %s version of Cube 2: Sauerbraten", file, hdr.version<DEMO_VERSION ? "older" : "newer");
            else if(hdr.protocol!=PROTOCOL_VERSION) formatstring(msg)("demo \"%s\" requires an %s version of Cube 2: Sauerbraten", file, hdr.protocol<PROTOCOL_VERSION ? "older" : "newer");
        }
        if(msg[0])
        {
            DELETEP(demoplayback);
            sendservmsg(msg);
            return;
        }

        sendservmsgf("playing demo \"%s\"", file);

        demomillis = 0;
        sendf(-1, 1, "ri3", N_DEMOPLAYBACK, 1, -1);

        if(demoplayback->read(&nextplayback, sizeof(nextplayback))!=sizeof(nextplayback))
        {
            enddemoplayback();
            return;
        }
        lilswap(&nextplayback, 1);
    }

    void readdemo()
    {
        if(!demoplayback) return;
        demomillis += curtime;
        while(demomillis>=nextplayback)
        {
            int chan, len;
            if(demoplayback->read(&chan, sizeof(chan))!=sizeof(chan) ||
               demoplayback->read(&len, sizeof(len))!=sizeof(len))
            {
                enddemoplayback();
                return;
            }
            lilswap(&chan, 1);
            lilswap(&len, 1);
            ENetPacket *packet = enet_packet_create(NULL, len+1, 0);
            if(!packet || demoplayback->read(packet->data+1, len)!=len)
            {
                if(packet) enet_packet_destroy(packet);
                enddemoplayback();
                return;
            }
            packet->data[0] = N_DEMOPACKET;
            sendpacket(-1, chan, packet);
            if(!packet->referenceCount) enet_packet_destroy(packet);
            if(!demoplayback) break;
            if(demoplayback->read(&nextplayback, sizeof(nextplayback))!=sizeof(nextplayback))
            {
                enddemoplayback();
                return;
            }
            lilswap(&nextplayback, 1);
        }
    }

    void stopdemo()
    {
        if(m_demo) enddemoplayback();
        else enddemorecord();
    }

    void pausegame(bool val, clientinfo *ci = NULL)
    {
        if(gamepaused==val) return;
        gamepaused = val;
        sendf(-1, 1, "riii", N_PAUSEGAME, gamepaused ? 1 : 0, ci ? ci->clientnum : -1);
    }

    void checkpausegame()
    {
        if(!gamepaused) return;
        int admins = 0;
        loopv(clients) if(clients[i]->privilege >= (restrictpausegame ? PRIV_ADMIN : PRIV_MASTER) || clients[i]->local) admins++;
        if(!admins) pausegame(false);
    }

    void forcepaused(bool paused)
    {
        pausegame(paused);
    }

    bool ispaused() { return gamepaused; }

    void changegamespeed(int val, clientinfo *ci = NULL)
    {
        val = clamp(val, 10, 1000);
        if(gamespeed==val) return;
        gamespeed = val;
        sendf(-1, 1, "riii", N_GAMESPEED, gamespeed, ci ? ci->clientnum : -1);
    }

    void forcegamespeed(int speed)
    {
        changegamespeed(speed);
    }

    int scaletime(int t) { return t*gamespeed; }

    SVAR(serverauth, "");

    struct userkey
    {
        char *name;
        char *desc;
        
        userkey() : name(NULL), desc(NULL) {}
        userkey(char *name, char *desc) : name(name), desc(desc) {}
    };

    static inline uint hthash(const userkey &k) { return ::hthash(k.name); }
    static inline bool htcmp(const userkey &x, const userkey &y) { return !strcmp(x.name, y.name) && !strcmp(x.desc, y.desc); }

    struct userinfo : userkey
    {
        void *pubkey;
        int privilege;

        userinfo() : pubkey(NULL), privilege(PRIV_NONE) {}
        ~userinfo() { delete[] name; delete[] desc; if(pubkey) freepubkey(pubkey); }
    };
    hashset<userinfo> users;

    void adduser(char *name, char *desc, char *pubkey, char *priv)
    {
        userkey key(name, desc);
        userinfo &u = users[key];
        if(u.pubkey) { freepubkey(u.pubkey); u.pubkey = NULL; }
        if(!u.name) u.name = newstring(name);
        if(!u.desc) u.desc = newstring(desc);
        u.pubkey = parsepubkey(pubkey);
        switch(priv[0])
        {
            case 'o': case 'O': u.privilege = PRIV_OWNER; break;
            case 'a': case 'A': u.privilege = PRIV_ADMIN; break;
            case 'm': case 'M': default: u.privilege = PRIV_AUTH; break;
        }
    }
    COMMAND(adduser, "ssss");

    void clearusers()
    {
        users.clear();
    }
    COMMAND(clearusers, "");

    void hashpassword(int cn, int sessionid, const char *pwd, char *result, int maxlen)
    {
        char buf[2*sizeof(string)];
        formatstring(buf)("%d %d ", cn, sessionid);
        copystring(&buf[strlen(buf)], pwd);
        if(!hashstring(buf, result, maxlen)) *result = '\0';
    }

    bool checkpassword(clientinfo *ci, const char *wanted, const char *given)
    {
        string hash;
        hashpassword(ci->clientnum, ci->sessionid, wanted, hash, sizeof(hash));
        return !strcmp(hash, given);
    }

    void revokemaster(clientinfo *ci)
    {
        ci->privilege = PRIV_NONE;
        if(ci->state.state==CS_SPECTATOR && !ci->local) aiman::removeai(ci);
    }

    extern void connected(clientinfo *ci);

    VAR(hidepriv, 0, 0, 1);

    const char * privcolor(int priv)
    {
        switch (priv)
        {
            case PRIV_MASTER:
                return "\f0";
            case PRIV_AUTH:
                return "\f1";
            case PRIV_ADMIN:
                return "\f6";
            case PRIV_OWNER:
                return "\f3";
            case PRIV_NONE:
            default:
                return "\f7";
        }
    }

    const char * privcolor(const char *name)
    {
        if(!strcmp(name, "master"))
            return "\f0";
        else if(!strcmp(name, "auth"))
            return "\f1";
        else if(!strcmp(name, "admin"))
            return "\f6";
        else if(!strcmp(name, "owner"))
            return "\f3";
        else
            return "\f7"; 
    }

    SVAR(masterpass, "");

    bool setmaster(clientinfo *ci, bool val, const char *pass = "", const char *authname = NULL, const char *authdesc = NULL, int authpriv = PRIV_MASTER, bool force = false, bool trial = false)
    {
        if(authname && !val) return false;
        const char *name = "";
        int oldpriv = ci->privilege;
        if(val)
        {
            bool haspass = adminpass[0] && checkpassword(ci, adminpass, pass);
            bool hasmpass = masterpass[0] && checkpassword(ci, masterpass, pass);
            int wantpriv = ci->local || haspass ? PRIV_ADMIN : authpriv;
            if(ci->privilege)
            {
                if(wantpriv <= ci->privilege) return true;
            }
            else if(wantpriv <= PRIV_MASTER && !force && !hasmpass)
            {
                if(ci->state.state==CS_SPECTATOR) 
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "Spectators may not claim master.");
                    return false;
                }
                loopv(clients) if(ci!=clients[i] && clients[i]->privilege && !clients[i]->isspy && (!hidepriv || clients[i]->privilege <= PRIV_AUTH))
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "Master is already claimed.");
                    return false;
                }
                if(!authname && !(mastermask&MM_AUTOAPPROVE) && !ci->privilege && !ci->local)
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "This server requires you to use the \"/auth\" command to claim master.");
                    return false;
                }
            }
            if(trial) return true;
            ci->privilege = wantpriv;
            name = privname(ci->privilege);
        }
        else
        {
            if(!ci->privilege) return false;
            if(trial) return true;
            name = privname(ci->privilege);
            revokemaster(ci);
        }
        bool hasmaster = false;
        loopv(clients) if(clients[i]->local || clients[i]->privilege >= PRIV_MASTER) hasmaster = true;
        if(!hasmaster)
        {
            mastermode = MM_OPEN;
            allowedips.shrink(0);
        }
        string msg;
        bool needshide = true;
        if(val && authname) 
        {
            if(authdesc && authdesc[0]) formatstring(msg)("\f0[INFO]\f7: Player \f1%s \f5(%i)\f7 claimed %s%s%s\f7 as \f5'\f2%s\f5' \f5{\f2%s\f5}",
                ci->name,
                ci->clientnum,
                hidepriv ? "\f4invisible " : "",
                privcolor(ci->privilege),
                name,
                authname,
                authdesc
            );
            else
            {
                formatstring(msg)("\f0[INFO]\f7: Player \f1%s \f5(%i)\f7 claimed %s%s\f7 as \f5'\f2%s\f5'",
                    ci->name,
                    ci->clientnum,
                    privcolor(ci->privilege),
                    name,
                    authname
                );
                needshide = false;
            }
        } 
        else formatstring(msg)("\f0[INFO]\f7: Player \f1%s \f5(%i) \f7%s %s%s%s",
            ci->name,
            ci->clientnum,
            val ? "claimed" : "relinquished",
            ((val && ci->privilege > PRIV_AUTH) || oldpriv > PRIV_AUTH) && hidepriv ? "\f4invisible " : "",
            privcolor(name),
            name
        );
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        if((hidepriv && needshide) || ci->isspy)
        {
            sendmsg(ci, msg);
            if(!ci->isspy)
            {
                loopv(clients) if(((val && clients[i]->privilege >= ci->privilege) || (!val && clients[i]->privilege >= PRIV_ADMIN)) && clients[i] != ci) sendmsg(clients[i], msg);
            }
        }
        else
        {
            putint(p, N_SERVMSG);
            sendstring(msg, p);
        }
        putint(p, N_CURRENTMASTER);
        putint(p, mastermode);
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && !clients[i]->isspy)
        {
            if((clients[i]->privilege > PRIV_AUTH && hidepriv) || clients[i]->clientnum == ci->clientnum) continue;
            putint(p, clients[i]->clientnum);
            putint(p, clients[i]->privilege);
        }
        putint(p, -1);
        sendpacket(-1, 1, p.finalize());
        if(hidepriv)
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege == PRIV_MASTER || clients[j]->privilege == PRIV_AUTH)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
            sendpacket(-1, 1, z.finalize());
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
            {
                clientinfo *cx = clients[j];
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CURRENTMASTER);
                putint(q, mastermode);
                loopv(clients) if((clients[i]->privilege >= PRIV_MASTER && clients[i]->privilege <= cx->privilege && !clients[i]->isspy) || clients[i]->clientnum == cx->clientnum)
                {
                    putint(q, clients[i]->clientnum);
                    putint(q, clients[i]->privilege);
                }
                putint(q, -1);
                sendpacket(cx->clientnum, 1, q.finalize());
            }
        }
        else
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
            sendpacket(-1, 1, z.finalize());
        }
        checkpausegame();
        return true;
    }

    bool trykick(clientinfo *ci, int victim, const char *reason = NULL, const char *authname = NULL, const char *authdesc = NULL, int authpriv = PRIV_NONE, bool trial = false)
    {
        int priv = ci->privilege;
        if(authname)
        {
            if(priv >= authpriv || ci->local) authname = authdesc = NULL;
            else priv = authpriv;
        }
        if((priv || ci->local) && ci->clientnum!=victim)
        {
            clientinfo *vinfo = (clientinfo *)getclientinfo(victim);
            if(vinfo && (priv >= vinfo->privilege || ci->local) && vinfo->privilege < PRIV_ADMIN && !vinfo->local)
            {
                if(trial) return true;
                string kicker;
                if(authname)
                {
                    if(authdesc && authdesc[0]) formatstring(kicker)("%s \f4as \f5'\f2%s\f5' \f0{\f6%s\f0}", colorname(ci), authname, authdesc);
                    else formatstring(kicker)("%s \f4as \f5'\f2%s\f5'", colorname(ci), authname);
                }
                else copystring(kicker, colorname(ci));
                string msg;
                if(!ci->isspy || !hidepriv || ci->privilege < PRIV_AUTH)
                {
                    if(reason && reason[0]) formatstring(msg)("\f0[INFO]\f7: \f2Player \f6%s \f5(%i) \f7has been \f3kicked \f7by %s \f4because: \f0%s\f7.", vinfo->name, vinfo->clientnum, kicker, reason);
                    else formatstring(msg)("\f0[INFO]\f7: \f2Player \f6%s \f5(%i) \f7has been \f3kicked \f7by %s\f7.", vinfo->name, vinfo->clientnum, kicker);
                }
                else
                {
                    if(reason && reason[0]) formatstring(msg)("\f0[INFO]\f7: \f2Player \f6%s \f5(%i) \f7has been \f3kicked \f4because: \f0%s\f7.", vinfo->name, vinfo->clientnum, reason);
                    else formatstring(msg)("\f0[INFO]\f7: \f2Player \f6%s \f5(%i) \f7has been\f7.", vinfo->name, vinfo->clientnum);
                }
                sendservmsg(msg);
                uint ip = getclientip(victim);
                addban(ip, 4*60*60000);
                kickclients(ip, ci, priv);
            }
        }
        return false;
    }

    savedscore *findscore(clientinfo *ci, bool insert)
    {
        uint ip = getclientip(ci->clientnum);
        if(!ip && !ci->local) return 0;
        if(!insert)
        {
            loopv(clients)
            {
                clientinfo *oi = clients[i];
                if(oi->clientnum != ci->clientnum && getclientip(oi->clientnum) == ip && !strcmp(oi->name, ci->name))
                {
                    oi->state.timeplayed += lastmillis - oi->state.lasttimeplayed;
                    oi->state.lasttimeplayed = lastmillis;
                    static savedscore curscore;
                    curscore.save(oi->state);
                    return &curscore;
                }
            }
        }
        loopv(scores)
        {
            savedscore &sc = scores[i];
            if(sc.ip == ip && !strcmp(sc.name, ci->name)) return &sc;
        }
        if(!insert) return 0;
        savedscore &sc = scores.add();
        sc.ip = ip;
        copystring(sc.name, ci->name);
        return &sc;
    }

    void savescore(clientinfo *ci)
    {
        savedscore *sc = findscore(ci, true);
        if(sc) sc->save(ci->state);
    }

    static struct msgfilter
    {
        uchar msgmask[NUMMSG];

        msgfilter(int msg, ...)
        {
            memset(msgmask, 0, sizeof(msgmask));
            va_list msgs;
            va_start(msgs, msg);
            for(uchar val = 1; msg < NUMMSG; msg = va_arg(msgs, int))
            {
                if(msg < 0) val = uchar(-msg);
                else msgmask[msg] = val;
            }
            va_end(msgs);
        }

        uchar operator[](int msg) const { return msg >= 0 && msg < NUMMSG ? msgmask[msg] : 0; }
    } msgfilter(-1, N_CONNECT, N_SERVINFO, N_INITCLIENT, N_WELCOME, N_MAPCHANGE, N_SERVMSG, N_DAMAGE, N_HITPUSH, N_SHOTFX, N_EXPLODEFX, N_DIED, N_SPAWNSTATE, N_FORCEDEATH, N_TEAMINFO, N_ITEMACC, N_ITEMSPAWN, N_TIMEUP, N_CDIS, N_CURRENTMASTER, N_PONG, N_RESUME, N_BASESCORE, N_BASEINFO, N_BASEREGEN, N_ANNOUNCE, N_SENDDEMOLIST, N_SENDDEMO, N_DEMOPLAYBACK, N_SENDMAP, N_DROPFLAG, N_SCOREFLAG, N_RETURNFLAG, N_RESETFLAG, N_INVISFLAG, N_CLIENT, N_AUTHCHAL, N_INITAI, N_EXPIRETOKENS, N_DROPTOKENS, N_STEALTOKENS, N_DEMOPACKET, -2, N_REMIP, N_NEWMAP, N_GETMAP, N_SENDMAP, N_CLIPBOARD, -3, N_EDITENT, N_EDITF, N_EDITT, N_EDITM, N_FLIP, N_COPY, N_PASTE, N_ROTATE, N_REPLACE, N_DELCUBE, N_EDITVAR, -4, N_POS, NUMMSG),
      connectfilter(-1, N_CONNECT, -2, N_AUTHANS, -3, N_PING, NUMMSG);

    int checktype(int type, clientinfo *ci)
    {
        if(ci)
        {
            if(!ci->connected) switch(connectfilter[type])
            {
                // allow only before authconnect
                case 1: return !ci->connectauth ? type : -1;
                // allow only during authconnect
                case 2: return ci->connectauth ? type : -1;
                // always allow
                case 3: return type;
                // never allow
                default: return -1;
            }
            if(ci->local) return type;
        }
        switch(msgfilter[type])
        {
            // server-only messages
            case 1: return ci ? -1 : type;
            // only allowed in coop-edit
            case 2: if(m_edit) break; return -1;
            // only allowed in coop-edit, no overflow check
            case 3: return m_edit ? type : -1;
            // no overflow check
            case 4: return type;
        }
        if(ci && ++ci->overflow >= 200) return -2;
        return type;
    }

    struct worldstate
    {
        int uses, len;
        uchar *data;

        worldstate() : uses(0), len(0), data(NULL) {}

        void setup(int n) { len = n; data = new uchar[n]; }
        void cleanup() { DELETEA(data); len = 0; }
        bool contains(const uchar *p) const { return p >= data && p < &data[len]; }
    };
    vector<worldstate> worldstates;
    bool reliablemessages = false;

    void cleanworldstate(ENetPacket *packet)
    {
        loopv(worldstates)
        {
            worldstate &ws = worldstates[i];
            if(!ws.contains(packet->data)) continue;
            ws.uses--;
            if(ws.uses <= 0)
            {
                ws.cleanup();
                worldstates.removeunordered(i);
            }
            break;
        }
    }

    void flushclientposition(clientinfo &ci)
    {
        if(ci.position.empty() || (!hasnonlocalclients() && !demorecord)) return;
        packetbuf p(ci.position.length(), 0);
        p.put(ci.position.getbuf(), ci.position.length());
        ci.position.setsize(0);
        sendpacket(-1, 0, p.finalize(), ci.ownernum);
    }

    static void sendpositions(worldstate &ws, ucharbuf &wsbuf)
    {
        if(wsbuf.empty()) return;
        int wslen = wsbuf.length();
        recordpacket(0, wsbuf.buf, wslen);
        wsbuf.put(wsbuf.buf, wslen);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            uchar *data = wsbuf.buf;
            int size = wslen;
            if(ci.wsdata >= wsbuf.buf) { data = ci.wsdata + ci.wslen; size -= ci.wslen; }
            if(size <= 0) continue;
            ENetPacket *packet = enet_packet_create(data, size, ENET_PACKET_FLAG_NO_ALLOCATE);
            sendpacket(ci.clientnum, 0, packet);
            if(packet->referenceCount) { ws.uses++; packet->freeCallback = cleanworldstate; }
            else enet_packet_destroy(packet);
        }
        wsbuf.offset(wsbuf.length());
    }

    static inline void addposition(worldstate &ws, ucharbuf &wsbuf, int mtu, clientinfo &bi, clientinfo &ci)
    {
        if(bi.position.empty()) return;
        if(wsbuf.length() + bi.position.length() > mtu) sendpositions(ws, wsbuf);
        int offset = wsbuf.length();
        wsbuf.put(bi.position.getbuf(), bi.position.length());
        bi.position.setsize(0);
        int len = wsbuf.length() - offset;
        if(ci.wsdata < wsbuf.buf) { ci.wsdata = &wsbuf.buf[offset]; ci.wslen = len; }
        else ci.wslen += len;
    }

    static void sendmessages(worldstate &ws, ucharbuf &wsbuf)
    {
        if(wsbuf.empty()) return;
        int wslen = wsbuf.length();
        recordpacket(1, wsbuf.buf, wslen);
        wsbuf.put(wsbuf.buf, wslen);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            uchar *data = wsbuf.buf;
            int size = wslen;
            if(ci.wsdata >= wsbuf.buf) { data = ci.wsdata + ci.wslen; size -= ci.wslen; }
            if(size <= 0) continue;
            ENetPacket *packet = enet_packet_create(data, size, (reliablemessages ? ENET_PACKET_FLAG_RELIABLE : 0) | ENET_PACKET_FLAG_NO_ALLOCATE);
            sendpacket(ci.clientnum, 1, packet);
            if(packet->referenceCount) { ws.uses++; packet->freeCallback = cleanworldstate; }
            else enet_packet_destroy(packet);
        }
        wsbuf.offset(wsbuf.length());
    }

    static inline void addmessages(worldstate &ws, ucharbuf &wsbuf, int mtu, clientinfo &bi, clientinfo &ci)
    {
        if(bi.messages.empty()) return;
        if(wsbuf.length() + 10 + bi.messages.length() > mtu) sendmessages(ws, wsbuf);
        int offset = wsbuf.length();
        putint(wsbuf, N_CLIENT);
        putint(wsbuf, bi.clientnum);
        putuint(wsbuf, bi.messages.length());
        wsbuf.put(bi.messages.getbuf(), bi.messages.length());
        bi.messages.setsize(0);
        int len = wsbuf.length() - offset;
        if(ci.wsdata < wsbuf.buf) { ci.wsdata = &wsbuf.buf[offset]; ci.wslen = len; }
        else ci.wslen += len;
    }

    bool buildworldstate()
    {
        int wsmax = 0;
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            ci.overflow = 0;
            ci.wsdata = NULL;
            wsmax += ci.position.length();
            if(ci.messages.length()) wsmax += 10 + ci.messages.length();
        }
        if(wsmax <= 0)
        {
            reliablemessages = false;
            return false;
        }
        worldstate &ws = worldstates.add();
        ws.setup(2*wsmax);
        int mtu = getservermtu() - 100;
        if(mtu <= 0) mtu = ws.len;
        ucharbuf wsbuf(ws.data, ws.len);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            addposition(ws, wsbuf, mtu, ci, ci);
            loopvj(ci.bots) addposition(ws, wsbuf, mtu, *ci.bots[j], ci);
        }
        sendpositions(ws, wsbuf);
        loopv(clients)
        {
            clientinfo &ci = *clients[i];
            if(ci.state.aitype != AI_NONE) continue;
            addmessages(ws, wsbuf, mtu, ci, ci);
            loopvj(ci.bots) addmessages(ws, wsbuf, mtu, *ci.bots[j], ci);
        }
        sendmessages(ws, wsbuf);
        reliablemessages = false;
        if(ws.uses) return true;
        ws.cleanup();
        worldstates.drop();
        return false;
    }

    bool sendpackets(bool force)
    {
        if(clients.empty() || (!hasnonlocalclients() && !demorecord)) return false;
        enet_uint32 curtime = enet_time_get()-lastsend;
        if(curtime<33 && !force) return false;
        bool flush = buildworldstate();
        lastsend += curtime - (curtime%33);
        return flush;
    }

    template<class T>
    void sendstate(gamestate &gs, T &p)
    {
        putint(p, gs.lifesequence);
        putint(p, gs.health);
        putint(p, gs.maxhealth);
        putint(p, gs.armour);
        putint(p, gs.armourtype);
        putint(p, gs.gunselect);
        loopi(GUN_PISTOL-GUN_SG+1) putint(p, gs.ammo[GUN_SG+i]);
    }

    void spawnstate(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        gs.spawnstate(gamemode);
        gs.lifesequence = (gs.lifesequence + 1)&0x7F;
    }

    void sendspawn(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        spawnstate(ci);
        sendf(ci->ownernum, 1, "rii7v", N_SPAWNSTATE, ci->clientnum, gs.lifesequence,
            gs.health, gs.maxhealth,
            gs.armour, gs.armourtype,
            gs.gunselect, GUN_PISTOL-GUN_SG+1, &gs.ammo[GUN_SG]);
        gs.lastspawn = gamemillis;
    }

    void sendwelcome(clientinfo *ci)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        int chan = welcomepacket(p, ci);
        sendpacket(ci->clientnum, chan, p.finalize());
    }

    void putinitclient(clientinfo *ci, packetbuf &p)
    {
        if(ci->isspy) return;
        if(ci->state.aitype != AI_NONE)
        {
            putint(p, N_INITAI);
            putint(p, ci->clientnum);
            putint(p, ci->ownernum);
            putint(p, ci->state.aitype);
            putint(p, ci->state.skill);
            putint(p, ci->playermodel);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
        }
        else
        {
            putint(p, N_INITCLIENT);
            putint(p, ci->clientnum);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
            putint(p, ci->playermodel);
        }
    }

    void welcomeinitclient(packetbuf &p, int exclude = -1)
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(!ci->connected || ci->clientnum == exclude || ci->isspy) continue;

            putinitclient(ci, p);
        }
    }

    bool hasmap(clientinfo *ci)
    {
        return (m_edit && (clients.length() > 0 || ci->local)) ||
               (smapname[0] && (!m_timed || gamemillis < gamelimit || (ci->state.state==CS_SPECTATOR && !ci->privilege && !ci->local) || numclients(ci->clientnum, true, true, true)));
    }

    int welcomepacket(packetbuf &p, clientinfo *ci)
    {
        putint(p, N_WELCOME);
        putint(p, N_MAPCHANGE);
        sendstring(smapname, p);
        putint(p, gamemode);
        putint(p, notgotitems ? 1 : 0);
        if(!ci || (m_timed && smapname[0]))
        {
            putint(p, N_TIMEUP);
            putint(p, gamemillis < gamelimit && !interm ? max((gamelimit - gamemillis)/1000, 1) : 0);
        }
        if(!notgotitems)
        {
            putint(p, N_ITEMLIST);
            loopv(sents) if(sents[i].spawned)
            {
                putint(p, i);
                putint(p, sents[i].type);
            }
            putint(p, -1);
        }
        bool hasmaster = false;
        if(mastermode != MM_OPEN)
        {
            putint(p, N_CURRENTMASTER);
            putint(p, mastermode);
            hasmaster = true;
        }
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && !clients[i]->isspy)
        {
            if(!hasmaster)
            {
                putint(p, N_CURRENTMASTER);
                putint(p, mastermode);
                hasmaster = true;
            }
            if(!hidepriv || clients[i]->privilege == PRIV_MASTER || clients[i]->privilege == PRIV_AUTH)
            {
                putint(p, clients[i]->clientnum);
                putint(p, clients[i]->privilege);
            }
        }
        if(hasmaster) putint(p, -1);
        if(gamepaused)
        {
            putint(p, N_PAUSEGAME);
            putint(p, 1);
            putint(p, -1);
        }
        if(gamespeed != 100)
        {
            putint(p, N_GAMESPEED);
            putint(p, gamespeed);
            putint(p, -1);
        }
        if(m_teammode)
        {
            putint(p, N_TEAMINFO);
            enumerates(teaminfos, teaminfo, t,
                if(t.frags) { sendstring(t.team, p); putint(p, t.frags); }
            );
            sendstring("", p);
        } 
        if(ci)
        {
            putint(p, N_SETTEAM);
            putint(p, ci->clientnum);
            sendstring(ci->team, p);
            putint(p, -1);
        }
        if(ci && (m_demo || m_mp(gamemode)) && ci->state.state!=CS_SPECTATOR)
        {
            if(smode && !smode->canspawn(ci, true))
            {
                ci->state.state = CS_DEAD;
                putint(p, N_FORCEDEATH);
                putint(p, ci->clientnum);
                sendf(-1, 1, "ri2x", N_FORCEDEATH, ci->clientnum, ci->clientnum);
            }
            else
            {
                gamestate &gs = ci->state;
                spawnstate(ci);
                putint(p, N_SPAWNSTATE);
                putint(p, ci->clientnum);
                sendstate(gs, p);
                gs.lastspawn = gamemillis;
            }
        }
        if(ci && (ci->state.state==CS_SPECTATOR || mastermode>=MM_LOCKED))
        {
            if(ci->state.state!=CS_SPECTATOR) ci->state.state = CS_SPECTATOR;
            putint(p, N_SPECTATOR);
            putint(p, ci->clientnum);
            putint(p, 1);
            sendf(-1, 1, "ri3x", N_SPECTATOR, ci->clientnum, 1, ci->clientnum);
        }
        if(!ci || clients.length()>1)
        {
            putint(p, N_RESUME);
            loopv(clients)
            {
                clientinfo *oi = clients[i];
                if(ci && oi->clientnum==ci->clientnum) continue;
                if(oi->isspy) continue;
                putint(p, oi->clientnum);
                putint(p, oi->state.state);
                putint(p, oi->state.frags);
                putint(p, oi->state.flags);
                putint(p, oi->state.quadmillis);
                sendstate(oi->state, p);
            }
            putint(p, -1);
            welcomeinitclient(p, ci ? ci->clientnum : -1);
        }
        if(smode) smode->initclient(ci, p, true);
        return 1;
    }

    bool restorescore(clientinfo *ci)
    {
        //if(ci->local) return false;
        savedscore *sc = findscore(ci, false);
        if(sc)
        {
            sc->restore(ci->state);
            return true;
        }
        return false;
    }

    void sendresume(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        sendf(-1, 1, "ri3i9vi", N_RESUME, ci->clientnum,
            gs.state, gs.frags, gs.flags, gs.quadmillis,
            gs.lifesequence,
            gs.health, gs.maxhealth,
            gs.armour, gs.armourtype,
            gs.gunselect, GUN_PISTOL-GUN_SG+1, &gs.ammo[GUN_SG], -1);
    }

    void sendinitclient(clientinfo *ci)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        putinitclient(ci, p);
        sendpacket(-1, 1, p.finalize(), ci->clientnum);
    }

    void loaditems()
    {
        resetitems();
        notgotitems = true;
        if(m_edit || !loadents(smapname, ments, &mcrc))
            return;
        loopv(ments) if(canspawnitem(ments[i].type))
        {
            server_entity se = { NOTUSED, 0, false };
            while(sents.length()<=i) sents.add(se);
            sents[i].type = ments[i].type;
            if(m_mp(gamemode) && delayspawn(sents[i].type)) sents[i].spawntime = spawntime(sents[i].type);
            else sents[i].spawned = true;
        }
        notgotitems = false;
    }

    bool persistteams = false;
    bool persistbots = false;

    void suicide(clientinfo *);

    ICOMMAND(respawn_all, "", (), {
        loopv(clients)
        {
            suicide(clients[i]);
        }
    })

    void startintermission();

    ICOMMAND(intermission, "", (), {
        startintermission();
    })

    ICOMMAND(getclientname, "i", (int *cn), {
        loopv(clients)
        {
            if(clients[i]->clientnum == *cn)
            {
                result(colorname(clients[i]));
                return;
            }
        }
        result("");
    })

    ICOMMAND(clientshavemap, "", (), {
        int ret = 1;
        loopv(clients) if(clients[i]->clientmap[0] == '\0' || !clients[i]->clientmap[0]) ret = 0;
        intret(ret);
    })

    struct racerun
    {
        string map;
        string name;
        int millis;
    };
    vector<racerun> raceruns;

    ICOMMAND(raceseconds, "", (), {
        floatret((float)(gamemillis-5000)/1000.0f);
    })

    ICOMMAND(bestraceseconds, "", (), {
        loopv(raceruns)
        {
            if(!strcmp((const char *)raceruns[i].map, smapname))
            {
                floatret((float)raceruns[i].millis/1000.0f);
                return;
            }
        }
        floatret(0.0f);
    })

    ICOMMAND(bestracerunner, "", (), {
        loopv(raceruns)
        {
            if(!strcmp((const char *)raceruns[i].map, smapname))
            {
                result((const char *)raceruns[i].name);
                return;
            }
        }
        result("");
    })

    ICOMMAND(isbestrace, "", (), {
        loopv(raceruns)
        {
            if(!strcmp((const char *)raceruns[i].map, smapname))
            {
                if(raceruns[i].millis > (gamemillis - 5000)) intret(1);
                else intret(0);
                return;
            }
        }
        intret(1);
    })

    ICOMMAND(addbestrace, "s", (const char *runner), {
        loopv(raceruns)
        {
            if(!strcmp(raceruns[i].map, smapname))
            {
                if(raceruns[i].millis > (gamemillis - 5000)) raceruns.remove(i);
                else return;
            }
        }
        racerun &cur = raceruns.add();
        copystring(cur.map, smapname);
        copystring(cur.name, runner);
        cur.millis = (gamemillis - 5000);
    })

    ICOMMAND(addbestracen, "ssi", (const char *runner, const char *map, int *millis), {
        racerun &cur = raceruns.add();
        copystring(cur.map, map);
        copystring(cur.name, runner);
        cur.millis = *millis;
    })

    void loadmap();

    void changemap(const char *s, int mode)
    {
        stopdemo();
        pausegame(false);
        changegamespeed(100);
        if(smode) smode->cleanup();
        if(!persistbots) aiman::clearai();

        gamemode = mode;
        gamemillis = 0;
        gamelimit = (m_overtime ? 15 : 10)*60000;
        interm = 0;
        nextexceeded = 0;
        copystring(smapname, s);
        loaditems();
        scores.shrink(0);
        shouldcheckteamkills = false;
        teamkills.shrink(0);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
        }

        if(!m_mp(gamemode)) kicknonlocalclients(DISC_LOCAL);

        sendf(-1, 1, "risii", N_MAPCHANGE, smapname, gamemode, 1);

        clearteaminfo();
        if(m_teammode && !persistteams) autoteam();

        if(m_capture) smode = &capturemode;
        else if(m_ctf) smode = &ctfmode;
        else if(m_collect) smode = &collectmode;
        else smode = NULL;

        if(m_timed && smapname[0]) sendf(-1, 1, "ri2", N_TIMEUP, gamemillis < gamelimit && !interm ? max((gamelimit - gamemillis)/1000, 1) : 0);
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            ci->mapchange();
            ci->state.lasttimeplayed = lastmillis;
            if(m_mp(gamemode) && ci->state.state!=CS_SPECTATOR && !ci->isspy) sendspawn(ci);
        }

        aiman::changemap();

        if(m_demo)
        {
            if(clients.length()) setupdemoplayback();
        }
        else if(demonextmatch)
        {
            demonextmatch = false;
            setupdemorecord();
        }

        if(smode) smode->setup();

        if(racemode && m_edit)
        {
            loopv(clients) { clients[i]->islooser = false; if(clients[i]->spectimes == 2) clients[i]->forcespec = false; clients[i]->spectimes = 0; }
            execute("clearsleep");
            sendservmsg("\f0[RACE-BOT]\f7: Delivering the \f3map\f7...");
            loadmap();
            sendservmsg("\f0[RACE-BOT]\f7: Waiting for all \f2clients \f7to get the map...");
            execfile("racebot.cfg", false);
        }
        else execute("z = []; zz = []; zzz = []; zzzz = [];");
    }

    // Some small scripting improvements ~>

    ICOMMAND(getplayerposx, "i", (int *cn), {
        loopv(clients) if(clients[i]->clientnum == *cn)
        {
            floatret(clients[i]->state.o.x);
            return;
        }
        intret(0);
    });

    ICOMMAND(getplayerposy, "i", (int *cn), {
        loopv(clients) if(clients[i]->clientnum == *cn)
        {
            floatret(clients[i]->state.o.y);
            return;
        }
        intret(0);
    });

    ICOMMAND(getplayerposz, "i", (int *cn), {
        loopv(clients) if(clients[i]->clientnum == *cn)
        {
            floatret(clients[i]->state.o.z);
            return;
        }
        intret(0);
    });

    // <~ EOSI

    struct lrend
    {
        int x, y, z;
        string name;
        bool operator==(const vec &o)
        {
            int a = (int)o.x > x ? (int)o.x : x;
            int b = (int)o.y > y ? (int)o.y : y;
            int c = (int)o.z > z ? (int)o.z : z;
            // ------------------------------ //
            int d = (int)o.x < x ? (int)o.x : x;
            int e = (int)o.y < y ? (int)o.y : y;
            int f = (int)o.z < z ? (int)o.z : z;
            return ((a - d) < 32) && ((b - e) < 32) && ((c - f) < 32);
        }
    };

    vector<lrend> lrends;

    void addlrend(const char *n, int a, int b, int c)
    {
        lrend &cur = lrends.add();
        cur.x = a;
        cur.y = b;
        cur.z = c;
        copystring(cur.name, n);
    }

    ICOMMAND(lrinitialize, "", (), {
        // LITTLE-RACE-XX map end locations taken from CMEdition client.
        addlrend("LITTLE-RACE-0" ,   512,    83,   405);
        addlrend("LITTLE-RACE-1" ,  1426,  1023,  1043); 
        addlrend("LITTLE-RACE-2" ,   365,  2814,   990);
        addlrend("LITTLE-RACE-3" ,  9472,  6579,  8542);
        addlrend("LITTLE-RACE-4" , 16509, 24436, 24862);
        addlrend("LITTLE-RACE-5" ,  8190, 13715,  4126);
        addlrend("LITTLE-RACE-6" ,  1401,   808,  2146);
        addlrend("LITTLE-RACE-7" ,  3361,  1680,  2276);
        addlrend("LITTLE-RACE-8" ,   641,  1414,  1166);
        addlrend("LITTLE-RACE-9" ,  6494,  8727,  4318);
        addlrend("LITTLE-RACE-10",  5335,  5919,  4190);
        addlrend("LITTLE-RACE-11",  1951,  3219,  1619);
        addlrend("LITTLE-RACE-12",  7378,  4095,  4116);
        addlrend("LITTLE-RACE-13",   991,  1994,  1361);
        addlrend("LITTLE-RACE-14",  3403,  9856,  6419);
        addlrend("LITTLE-RACE-15",  5936,  5591,  7070);
        addlrend("LITTLE-RACE-16",  1152,  3095,  2718);
        addlrend("LITTLE-RACE-17",   578,   620,   853);
        addlrend("LITTLE-RACE-18",  3039,  2700,  2466);
        addlrend("LITTLE-RACE-19",  4639,  4996,  3422);
        addlrend("LITTLE-RACE-20",  2688,  2664,  2462);
        addlrend("LITTLE-RACE-21",  3392,    51,  3470);
        addlrend("LITTLE-RACE-22",  1152,   313,  1374);
        addlrend("LITTLE-RACE-23", 10240, 10030,  8985);
        addlrend("LITTLE-RACE-24",  4341,  5504,  4979);
        addlrend("LITTLE-RACE-25",  4864,  3471,  3988);
        addlrend("LITTLE-RACE-26",  1086,  2793,  1950);
        addlrend("LITTLE-RACE-27",  1020,  2047,  2515);
        addlrend("LITTLE-RACE-28",  3761,  2607,  1830);
        addlrend("LITTLE-RACE-29",  6402,  6248,  9630);
        addlrend("LITTLE-RACE-30", 33387, 35008, 33630);
        addlrend("LITTLE-RACE-31", 38227, 32899, 32853);
        addlrend("LITTLE-RACE-32",  2383,   784,  2930);
        addlrend("LITTLE-RACE-33",  4518,  6271,  2142);
        addlrend("LITTLE-RACE-34",  2400,   661,   838);
        addlrend("LITTLE-RACE-35", 32496, 32767, 33342);
        addlrend("LITTLE-RACE-36", 30107, 33271, 33550);
        addlrend("LITTLE-RACE-37",  2047,  3826,  1694);
        addlrend("LITTLE-RACE-38", 33150, 27537, 32832);
        addlrend("LITTLE-RACE-39", 39968, 35808, 10389);
        addlrend("LITTLE-RACE-40",  3115,  4033,  6177);
        addlrend("LITTLE-RACE-41", 32832, 36495, 20198);
        addlrend("LITTLE-RACE-42", 32634, 32768, 33234);
        addlrend("LITTLE-RACE-43",  2863,  2178,  1950);
        addlrend("LITTLE-RACE-44",  6660,  4337,   145);
        addlrend("LITTLE-RACE-45",  1551,   155,  1107);
        addlrend("LITTLE-RACE-46",  2482,  2031,  4129);
        addlrend("LITTLE-RACE-47",  9215, 13846,  4114);
        addlrend("LITTLE-RACE-48",  5194,  4657,  3390);
    }) 

    ICOMMAND(lruninitialize, "", (), {
        lrends.shrink(0);
    })

    ICOMMAND(checkplayerspos, "", (), {
        if(!racemode || !m_edit) { intret(-1); return; }
        loopv(lrends)
        {
            if(!strcmp(lrends[i].name, smapname))
            {
                loopvj(clients)
                {
                    if(clients[j]->state.state!=CS_ALIVE || clients[j]->islooser) continue;
                    if(lrends[i] == clients[j]->state.o)
                    {
                        intret(clients[j]->clientnum);
                        return;
                    }
                }
            }
        }
        intret(-1);
    })

    void rotatemap(bool next)
    {
        if(!maprotations.inrange(curmaprotation))
        {
            changemap("", 1);
            return;
        }
        if(next) 
        {
            curmaprotation = findmaprotation(gamemode, smapname);
            if(curmaprotation >= 0) nextmaprotation();
            else curmaprotation = smapname[0] ? max(findmaprotation(gamemode, ""), 0) : 0;
        }
        maprotation &rot = maprotations[curmaprotation];
        changemap(rot.map, rot.findmode(gamemode));
    }
    
    struct votecount
    {
        char *map;
        int mode, count;
        votecount() {}
        votecount(char *s, int n) : map(s), mode(n), count(0) {}
    };

    void checkvotes(bool force = false)
    {
        vector<votecount> votes;
        int maxvotes = 0;
        loopv(clients)
        {
            clientinfo *oi = clients[i];
            if(oi->state.state==CS_SPECTATOR && !oi->privilege && !oi->local) continue;
            if(oi->state.aitype!=AI_NONE) continue;
            maxvotes++;
            if(!m_valid(oi->modevote)) continue;
            votecount *vc = NULL;
            loopvj(votes) if(!strcmp(oi->mapvote, votes[j].map) && oi->modevote==votes[j].mode)
            {
                vc = &votes[j];
                break;
            }
            if(!vc) vc = &votes.add(votecount(oi->mapvote, oi->modevote));
            vc->count++;
        }
        votecount *best = NULL;
        loopv(votes) if(!best || votes[i].count > best->count || (votes[i].count == best->count && rnd(2))) best = &votes[i];
        if(force || (best && best->count > maxvotes/2))
        {
            if(demorecord) enddemorecord();
            if(best && (best->count > (force ? 1 : maxvotes/2)))
            {
                sendservmsg(force ? "vote passed by default" : "vote passed by majority");
                changemap(best->map, best->mode);
            }
            else rotatemap(true);
        }
    }

    void forcemap(const char *map, int mode)
    {
        stopdemo();
        if(!map[0] && !m_check(mode, M_EDIT)) 
        {
            int idx = findmaprotation(mode, smapname);
            if(idx < 0 && smapname[0]) idx = findmaprotation(mode, "");
            if(idx < 0) return;
            map = maprotations[idx].map;
        }
        if(hasnonlocalclients()) sendservmsgf("local player forced %s on map %s", modename(mode), map[0] ? map : "[new map]");
        changemap(map, mode);
    }

    VARP(forcegamemode, 0, 0, 2);

    void vote(const char *map, int reqmode, int sender)
    {
        clientinfo *ci = getinfo(sender);
        if(!ci || (ci->state.state==CS_SPECTATOR && !ci->privilege && !ci->local) || (!ci->local && !m_mp(reqmode))) return;
        if(!m_valid(reqmode)) return;
        if(!map[0] && !m_check(reqmode, M_EDIT)) 
        {
            int idx = findmaprotation(reqmode, smapname);
            if(idx < 0 && smapname[0]) idx = findmaprotation(reqmode, "");
            if(idx < 0) return;
            map = maprotations[idx].map;
        }
        if(lockmaprotation && !ci->local && ci->privilege < (lockmaprotation > 1 ? PRIV_ADMIN : PRIV_MASTER) && findmaprotation(reqmode, map) < 0) 
        {
            sendf(sender, 1, "ris", N_SERVMSG, "This server has locked the map rotation.");
            return;
        }
        if(forcegamemode && reqmode != defaultgamemode && ci->privilege < (forcegamemode > 1 ? PRIV_ADMIN : PRIV_MASTER))
        {
            sendf(sender, 1, "ris", N_SERVMSG, "This server has locked the gamemode.");
            return;
        }
        copystring(ci->mapvote, map);
        ci->modevote = reqmode;
        if(ci->local || (ci->privilege && mastermode>=MM_VETO))
        {
            if(demorecord) enddemorecord();
            if(!ci->local || hasnonlocalclients())
                sendservmsgf("%s forced %s on map %s", colorname(ci), modename(ci->modevote), ci->mapvote[0] ? ci->mapvote : "[new map]");
            changemap(ci->mapvote, ci->modevote);
        }
        else
        {
            sendservmsgf("%s suggests %s on map %s (select map to vote)", colorname(ci), modename(reqmode), map[0] ? map : "[new map]");
            checkvotes();
        }
    }

    int curlr = 1;

    void checkintermission()
    {
        if(gamemillis >= gamelimit && !interm)
        {
            sendf(-1, 1, "ri2", N_TIMEUP, 0);
            if(smode) smode->intermission();
            changegamespeed(100);
            string message;
            string bak;
            if(racemode && m_edit)
            {
                sendservmsg("\f0[INFO]\f7: Changing to a \f3new \f7race...");
                defformatstring(mapname)("LITTLE-RACE-%i", curlr < 48 ? curlr+1 : 1);
                curlr = curlr < 48 ? curlr+1 : 1;
                changemap(mapname, 1);
                return;
            }
            interm = gamemillis + 10000;
            loopv(clients)
            {
                clientinfo *ci = clients[i];
                formatstring(message)("\f0[STATS]\f7: \f1Your \f2game \f6statistics \f7for this \f2match\f7: \f4%i \f6frags \f7- \f4%i \f2deaths \f0(kpd: \f4%.3f\f0) \f7/ \f3accuracy: \f4%.3f%%",
                    ci->state.frags,
                    ci->state.deaths,
                    ((float)ci->state.frags/max(ci->state.deaths, 1)),
                    (float)(ci->state.damage*100/max(ci->state.shotdamage, 1))
                );
                if(m_teammode)
                {
                    copystring(bak, message);
                    formatstring(message)("%s \f7/ \f4%i \f5teamkills", bak, ci->state.teamkills);
                }
                if(m_ctf || m_collect)
                {
                    copystring(bak, message);
                    formatstring(message)("%s \f7/ \f2%s \f1scored: \f4%i", bak, (m_ctf && !m_collect) ? "flags" : "skulls", ci->state.flags);
                    if(!m_collect)
                    {
                        copystring(bak, message);
                        formatstring(message)("%s \f7/ \f2flags \f0stolen: \f4%i \f7/ \f2flags \f5returned: \f4%i", bak, ci->state.stolen, ci->state.returned);
                    }
                }
                sendmsg(ci, message);
            }
        }
    }

    void startintermission() { gamelimit = min(gamelimit, gamemillis); checkintermission(); }

    void dodamage(clientinfo *target, clientinfo *actor, int damage, int gun, const vec &hitpush = vec(0, 0, 0))
    {
        if(m_edit && (racemode/* || nodamage*/)) return;
        gamestate &ts = target->state;
        ts.dodamage(damage);
        if(target!=actor && !isteam(target->team, actor->team)) actor->state.damage += damage;
        sendf(-1, 1, "ri6", N_DAMAGE, target->clientnum, actor->clientnum, damage, ts.armour, ts.health);
        if(target==actor) target->setpushed();
        else if(!hitpush.iszero())
        {
            ivec v = vec(hitpush).rescale(DNF);
            sendf(ts.health<=0 ? -1 : target->ownernum, 1, "ri7", N_HITPUSH, target->clientnum, gun, damage, v.x, v.y, v.z);
            target->setpushed();
        }
        if(ts.health<=0)
        {
            target->state.deaths++;
            int fragvalue = smode ? smode->fragvalue(target, actor) : (target==actor || isteam(target->team, actor->team) ? -1 : 1);
            actor->state.frags += fragvalue;
            if(fragvalue>0)
            {
                int friends = 0, enemies = 0; // note: friends also includes the fragger
                if(m_teammode) loopv(clients) if(strcmp(clients[i]->team, actor->team)) enemies++; else friends++;
                else { friends = 1; enemies = clients.length()-1; }
                actor->state.effectiveness += fragvalue*friends/float(max(enemies, 1));
            }
            teaminfo *t = m_teammode ? teaminfos.access(actor->team) : NULL;
            if(t) t->frags += fragvalue; 
            sendf(-1, 1, "ri5", N_DIED, target->clientnum, actor->clientnum, actor->state.frags, t ? t->frags : 0);
            target->position.setsize(0);
            if(smode) smode->died(target, actor);
            ts.state = CS_DEAD;
            ts.lastdeath = gamemillis;
            if(actor!=target && isteam(actor->team, target->team)) 
            {
                actor->state.teamkills++;
                addteamkill(actor, target, 1);
            }
            ts.deadflush = ts.lastdeath + DEATHMILLIS;
            // don't issue respawn yet until DEATHMILLIS has elapsed
            // ts.respawn();
        }
    }

    void suicide(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        if(gs.state!=CS_ALIVE) return;
        int fragvalue = smode ? smode->fragvalue(ci, ci) : -1;
        ci->state.frags += fragvalue;
        ci->state.deaths++;
        teaminfo *t = m_teammode ? teaminfos.access(ci->team) : NULL;
        if(t) t->frags += fragvalue;
        sendf(-1, 1, "ri5", N_DIED, ci->clientnum, ci->clientnum, gs.frags, t ? t->frags : 0);
        ci->position.setsize(0);
        if(smode) smode->died(ci, NULL);
        gs.state = CS_DEAD;
        gs.lastdeath = gamemillis;
        gs.respawn();
    }

    void suicideevent::process(clientinfo *ci)
    {
        suicide(ci);
    }

    void explodeevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        switch(gun)
        {
            case GUN_RL:
                if(!gs.rockets.remove(id)) return;
                break;

            case GUN_GL:
                if(!gs.grenades.remove(id)) return;
                break;

            default:
                return;
        }
        sendf(-1, 1, "ri4x", N_EXPLODEFX, ci->clientnum, gun, id, ci->ownernum);
        loopv(hits)
        {
            hitinfo &h = hits[i];
            clientinfo *target = getinfo(h.target);
            if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence || h.dist<0 || h.dist>guns[gun].exprad) continue;

            bool dup = false;
            loopj(i) if(hits[j].target==h.target) { dup = true; break; }
            if(dup) continue;

            int damage = guns[gun].damage;
            if(gs.quadmillis) damage *= 4;
            damage = int(damage*(1-h.dist/EXP_DISTSCALE/guns[gun].exprad));
            if(target==ci) damage /= EXP_SELFDAMDIV;
            dodamage(target, ci, damage, gun, h.dir);
        }
    }

    void shotevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        int wait = millis - gs.lastshot;
        // anticheat >
        if(wait<gs.gunwait) { ac(ci, GUNHACKRELOAD, wait); return; }
        if(gun<GUN_FIST || gun>GUN_PISTOL) { ac(ci, UNKNOWNGUN, gun); }
        if(m_insta && gun!=GUN_FIST && gun!=GUN_RIFLE) { ac(ci, NONINSTAGUNININSTA, gun); return; }
        if(guns[gun].range && from.dist(to) > guns[gun].range + 1) { ac(ci, GUNHACKRANGE, from.dist(to)); return; }
        if(!gs.isalive(gamemillis)) { ac(ci, GUNHACKNOTALIVE); return; }
        if(gs.ammo[gun]<=0) { ac(ci, GUNHACKNOAMMO); return; }
        // < anticheat
        if(gun!=GUN_FIST) gs.ammo[gun]--;
        gs.lastshot = millis;
        gs.gunwait = guns[gun].attackdelay;
        sendf(-1, 1, "rii9x", N_SHOTFX, ci->clientnum, gun, id,
                int(from.x*DMF), int(from.y*DMF), int(from.z*DMF),
                int(to.x*DMF), int(to.y*DMF), int(to.z*DMF),
                ci->ownernum);
        gs.shotdamage += guns[gun].damage*(gs.quadmillis ? 4 : 1)*guns[gun].rays;
        switch(gun)
        {
            case GUN_RL: gs.rockets.add(id); break;
            case GUN_GL: gs.grenades.add(id); break;
            default:
            {
                int totalrays = 0, maxrays = guns[gun].rays;
                loopv(hits)
                {
                    hitinfo &h = hits[i];
                    clientinfo *target = getinfo(h.target);
                    if(!target || target->state.state!=CS_ALIVE || h.lifesequence!=target->state.lifesequence) continue;
                    // anticheat >
                    if(h.rays<1) { ac(ci, GENERICGUNHACK); return; }
                    if(h.dist > guns[gun].range + 1) { ac(ci, GUNHACKRANGE, h.dist); return; }
                    // < anticheat
                    totalrays += h.rays;
                    // anticheat >
                    if(totalrays>maxrays) { ac(ci, GUNHACKRAYS, totalrays); return; }
                    // < anticheat
                    int damage = h.rays*guns[gun].damage;
                    if(gs.quadmillis) damage *= 4;
                    dodamage(target, ci, damage, gun, h.dir);
                }
                break;
            }
        }
    }

    void pickupevent::process(clientinfo *ci)
    {
        gamestate &gs = ci->state;
        if(m_mp(gamemode) && !gs.isalive(gamemillis)) return;
        pickup(ent, ci->clientnum);
    }

    bool gameevent::flush(clientinfo *ci, int fmillis)
    {
        process(ci);
        return true;
    }

    bool timedevent::flush(clientinfo *ci, int fmillis)
    {
        if(millis > fmillis) return false;
        else if(millis >= ci->lastevent)
        {
            ci->lastevent = millis;
            process(ci);
        }
        return true;
    }

    void clearevent(clientinfo *ci)
    {
        delete ci->events.remove(0);
    }

    void flushevents(clientinfo *ci, int millis)
    {
        while(ci->events.length())
        {
            gameevent *ev = ci->events[0];
            if(ev->flush(ci, millis)) clearevent(ci);
            else break;
        }
    }

    void processevents()
    {
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(curtime>0 && ci->state.quadmillis) ci->state.quadmillis = max(ci->state.quadmillis-curtime, 0);
            flushevents(ci, gamemillis);
        }
    }

    void cleartimedevents(clientinfo *ci)
    {
        int keep = 0;
        loopv(ci->events)
        {
            if(ci->events[i]->keepable())
            {
                if(keep < i)
                {
                    for(int j = keep; j < i; j++) delete ci->events[j];
                    ci->events.remove(keep, i - keep);
                    i = keep;
                }
                keep = i+1;
                continue;
            }
        }
        while(ci->events.length() > keep) delete ci->events.pop();
        ci->timesync = false;
    }

    void serverupdate()
    {
        if(shouldstep && !gamepaused)
        {
            gamemillis += curtime;

            if(m_demo) readdemo();
            else if(!m_timed || gamemillis < gamelimit)
            {
                processevents();
                if(curtime)
                {
                    loopv(sents) if(sents[i].spawntime) // spawn entities when timer reached
                    {
                        int oldtime = sents[i].spawntime;
                        sents[i].spawntime -= curtime;
                        if(sents[i].spawntime<=0)
                        {
                            sents[i].spawntime = 0;
                            sents[i].spawned = true;
                            sendf(-1, 1, "ri2", N_ITEMSPAWN, i);
                        }
                        else if(sents[i].spawntime<=10000 && oldtime>10000 && (sents[i].type==I_QUAD || sents[i].type==I_BOOST))
                        {
                            sendf(-1, 1, "ri2", N_ANNOUNCE, sents[i].type);
                        }
                    }
                }
                aiman::checkai();
                if(smode) smode->update();
            }
        }

        while(bannedips.length() && bannedips[0].expire-totalmillis <= 0) bannedips.remove(0);
        loopv(connects) if(totalmillis-connects[i]->connectmillis>15000) disconnect_client(connects[i]->clientnum, DISC_TIMEOUT);

        if(nextexceeded && gamemillis > nextexceeded && (!m_timed || gamemillis < gamelimit))
        {
            nextexceeded = 0;
            loopvrev(clients) 
            {
                clientinfo &c = *clients[i];
                if(c.state.aitype != AI_NONE) continue;
                if(c.checkexceeded()) disconnect_client(c.clientnum, DISC_MSGERR);
                else c.scheduleexceeded();
            }
        }

        if(shouldcheckteamkills) checkteamkills();

        if(shouldstep && !gamepaused)
        {
            if(m_timed && smapname[0] && gamemillis-curtime>0) checkintermission();
            if(interm > 0 && gamemillis>interm)
            {
                if(demorecord) enddemorecord();
                interm = -1;
                checkvotes(true);
            }
        }

        shouldstep = clients.length() > 0;
    }

    struct crcinfo
    {
        int crc, matches;

        crcinfo() {}
        crcinfo(int crc, int matches) : crc(crc), matches(matches) {}

        static bool compare(const crcinfo &x, const crcinfo &y) { return x.matches > y.matches; }
    };

    VARP(forceSpec, 0, 1, 1);

    void checkmaps(int req = -1)
    {
        if(m_edit || !smapname[0]) return;
        vector<crcinfo> crcs;
        int total = 0, unsent = 0, invalid = 0;
        if(mcrc) crcs.add(crcinfo(mcrc, clients.length() + 1));
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.state==CS_SPECTATOR || ci->state.aitype != AI_NONE || ci->isspy) continue;
            total++;
            if(!ci->clientmap[0])
            {
                if(ci->mapcrc < 0) invalid++;
                else if(!ci->mapcrc) unsent++;
            }
            else
            {
                crcinfo *match = NULL;
                loopvj(crcs) if(crcs[j].crc == ci->mapcrc) { match = &crcs[j]; break; }
                if(!match) crcs.add(crcinfo(ci->mapcrc, 1));
                else match->matches++;
            }
        }
        if(!mcrc && total - unsent < min(total, 4)) return;
        crcs.sort(crcinfo::compare);
        string msg;
        loopv(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->state.state==CS_SPECTATOR || ci->state.aitype != AI_NONE || ci->clientmap[0] || ci->mapcrc >= 0 || (req < 0 && ci->warned) || ci->isspy) continue;
            formatstring(msg)("\f0[INFO]\f7: \f7Client \f1%s \f5(%i) \f7has modified the \f2map \f6\"%s\"\f7.", ci->name, ci->clientnum, smapname);
            sendf(req, 1, "ris", N_SERVMSG, msg);
            if(forceSpec)
            {
                sendf(-1, 1, "riii", N_SPECTATOR, ci->clientnum, 1);
                ci->forcespec = true;
            }
            if(req < 0) ci->warned = true;
        }
        if(crcs.empty() || crcs.length() < 2) return;
        loopv(crcs)
        {
            crcinfo &info = crcs[i];
            if(i || info.matches <= crcs[i+1].matches) loopvj(clients)
            {
                clientinfo *ci = clients[j];
                if(ci->state.state==CS_SPECTATOR || ci->state.aitype != AI_NONE || !ci->clientmap[0] || ci->mapcrc != info.crc || (req < 0 && ci->warned) || ci->isspy) continue;
                formatstring(msg)("\f0[INFO]\f7: \f7Client \f1%s \f5(%i) \f7has modified the \f2map \f6\"%s\"\f7.", ci->name, ci->clientnum, smapname);
                sendf(req, 1, "ris", N_SERVMSG, msg);
                if(forceSpec)
                {
                    sendf(-1, 1, "riii", N_SPECTATOR, ci->clientnum, 1);
                    ci->forcespec = true;
                }
                if(req < 0) ci->warned = true;
            }
        }
    }

    void sendservinfo(clientinfo *ci)
    {
        sendf(ci->clientnum, 1, "ri5ss", N_SERVINFO, ci->clientnum, PROTOCOL_VERSION, ci->sessionid, serverpass[0] ? 1 : 0, serverdesc, serverauth);
    }

    void noclients()
    {
        gamemode = defaultgamemode;
        bannedips.shrink(0);
        aiman::clearai();
    }

    void localconnect(int n)
    {
        clientinfo *ci = getinfo(n);
        ci->clientnum = ci->ownernum = n;
        ci->connectmillis = totalmillis;
        ci->sessionid = (rnd(0x1000000)*((totalmillis%10000)+1))&0xFFFFFF;
        ci->local = true;

        connects.add(ci);
        sendservinfo(ci);
    }

    void localdisconnect(int n)
    {
        if(m_demo) enddemoplayback();
        clientdisconnect(n);
    }

    int clientconnect(int n, uint ip)
    {
        clientinfo *ci = getinfo(n);
        ci->clientnum = ci->ownernum = n;
        ci->connectmillis = totalmillis;
        ci->sessionid = (rnd(0x1000000)*((totalmillis%10000)+1))&0xFFFFFF;

        connects.add(ci);
        if(!m_mp(gamemode)) return DISC_LOCAL;
        sendservinfo(ci);
        return DISC_NONE;
    }

    void clientdisconnect(int n)
    {
        clientinfo *ci = getinfo(n);
        loopv(clients) if(clients[i]->authkickvictim == ci->clientnum) clients[i]->cleanauth(); 
        if(ci->connected)
        {
            if(ci->privilege) setmaster(ci, false);
            if(smode) smode->leavegame(ci, true);
            ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            savescore(ci);
            if(!ci->isspy) sendf(-1, 1, "ri2", N_CDIS, n);
            clients.removeobj(ci);
            aiman::removeai(ci);
            if(!numclients(-1, false, true)) noclients(); // bans clear when server empties
            if(ci->local) checkpausegame();
        }
        else connects.removeobj(ci);
    }

    int reserveclients() { return 3; }

    struct gbaninfo
    {
        enet_uint32 ip, mask;
    };

    vector<gbaninfo> gbans;

    void cleargbans()
    {
        gbans.shrink(0);
    }

    bool checkgban(uint ip)
    {
        loopv(gbans) if((ip & gbans[i].mask) == gbans[i].ip) return true;
        return false;
    }

    void addgban(const char *name)
    {
        union { uchar b[sizeof(enet_uint32)]; enet_uint32 i; } ip, mask;
        ip.i = 0;
        mask.i = 0;
        loopi(4)
        {
            char *end = NULL;
            int n = strtol(name, &end, 10);
            if(!end) break;
            if(end > name) { ip.b[i] = n; mask.b[i] = 0xFF; }
            name = end;
            while(*name && *name++ != '.');
        }
        gbaninfo &ban = gbans.add();
        ban.ip = ip.i;
        ban.mask = mask.i;

        loopvrev(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->local || ci->privilege >= PRIV_ADMIN) continue;
            if(checkgban(getclientip(ci->clientnum))) disconnect_client(ci->clientnum, DISC_IPBAN);
        }
    }
       
    int allowconnect(clientinfo *ci, const char *pwd = "")
    {
        if(ci->local) return DISC_NONE;
        if(!m_mp(gamemode)) return DISC_LOCAL;
        if(serverpass[0])
        {
            if(!checkpassword(ci, serverpass, pwd)) return DISC_PASSWORD;
            return DISC_NONE;
        }
        if(adminpass[0] && checkpassword(ci, adminpass, pwd)) return DISC_NONE;
        if(numclients(-1, false, true)>=maxclients) return DISC_MAXCLIENTS;
        uint ip = getclientip(ci->clientnum);
        loopv(bannedips) if(bannedips[i].ip==ip) return DISC_IPBAN;
        if(checkgban(ip)) return DISC_IPBAN;
        if(mastermode>=MM_PRIVATE && allowedips.find(ip)<0) return DISC_PRIVATE;
        return DISC_NONE;
    }

    bool allowbroadcast(int n)
    {
        clientinfo *ci = getinfo(n);
        return ci && ci->connected;
    }

    clientinfo *findauth(uint id)
    {
        loopv(clients) if(clients[i]->authreq == id) return clients[i];
        return NULL;
    }


    void authfailed(clientinfo *ci)
    {
        if(!ci) return;
        ci->cleanauth();
        if(ci->connectauth) disconnect_client(ci->clientnum, ci->connectauth);
    }

    void authfailed(uint id)
    {
        authfailed(findauth(id));
    }

    void authsucceeded(uint id)
    {
        clientinfo *ci = findauth(id);
        if(!ci) return;
        ci->cleanauth(ci->connectauth!=0);
        if(ci->connectauth) connected(ci);
        if(ci->authkickvictim >= 0)
        {
            if(setmaster(ci, true, "", ci->authname, NULL, PRIV_AUTH, false, true))
                trykick(ci, ci->authkickvictim, ci->authkickreason, ci->authname, NULL, PRIV_AUTH);    
            ci->cleanauthkick();
        }
        else setmaster(ci, true, "", ci->authname, NULL, PRIV_AUTH);
    }

    void authchallenged(uint id, const char *val, const char *desc = "")
    {
        clientinfo *ci = findauth(id);
        if(!ci) return;
        sendf(ci->clientnum, 1, "risis", N_AUTHCHAL, desc, id, val);
    }

    uint nextauthreq = 0;

    bool tryauth(clientinfo *ci, const char *user, const char *desc)
    {
        ci->cleanauth();
        if(!nextauthreq) nextauthreq = 1;
        ci->authreq = nextauthreq++;
        filtertext(ci->authname, user, false, 100);
        copystring(ci->authdesc, desc);
        if(ci->authdesc[0])
        {
            userinfo *u = users.access(userkey(ci->authname, ci->authdesc));
            if(u) 
            {
                uint seed[3] = { ::hthash(serverauth) + detrnd(size_t(ci) + size_t(user) + size_t(desc), 0x10000), uint(totalmillis), randomMT() };
                vector<char> buf;
                ci->authchallenge = genchallenge(u->pubkey, seed, sizeof(seed), buf);
                sendf(ci->clientnum, 1, "risis", N_AUTHCHAL, desc, ci->authreq, buf.getbuf());
            }
            else ci->cleanauth();
        }
        else if(!requestmasterf("reqauth %u %s\n", ci->authreq, ci->authname))
        {
            ci->cleanauth();
            sendf(ci->clientnum, 1, "ris", N_SERVMSG, "not connected to authentication server");
        }
        if(ci->authreq) return true;
        if(ci->connectauth) disconnect_client(ci->clientnum, ci->connectauth);
        return false;
    }

    bool answerchallenge(clientinfo *ci, uint id, char *val, const char *desc)
    {
        if(ci->authreq != id || strcmp(ci->authdesc, desc)) 
        {
            ci->cleanauth();
            return !ci->connectauth;
        }
        for(char *s = val; *s; s++)
        {
            if(!isxdigit(*s)) { *s = '\0'; break; }
        }
        if(desc[0])
        {
            if(ci->authchallenge && checkchallenge(val, ci->authchallenge))
            {
                userinfo *u = users.access(userkey(ci->authname, ci->authdesc));
                if(u) 
                {
                    if(ci->connectauth) connected(ci);
                    if(ci->authkickvictim >= 0)
                    {
                        if(setmaster(ci, true, "", ci->authname, ci->authdesc, u->privilege, false, true))
                            trykick(ci, ci->authkickvictim, ci->authkickreason, ci->authname, ci->authdesc, u->privilege);
                    }
                    else setmaster(ci, true, "", ci->authname, ci->authdesc, u->privilege);
                }
            }
            ci->cleanauth(); 
        } 
        else if(!requestmasterf("confauth %u %s\n", id, val))
        {
            ci->cleanauth();
            sendf(ci->clientnum, 1, "ris", N_SERVMSG, "not connected to authentication server");
        }
        return ci->authreq || !ci->connectauth;
    }

    void masterconnected()
    {
    }

    void masterdisconnected()
    {
        loopvrev(clients)
        {
            clientinfo *ci = clients[i];
            if(ci->authreq) authfailed(ci); 
        }
    }

    void processmasterinput(const char *cmd, int cmdlen, const char *args)
    {
        uint id;
        string val;
        if(sscanf(cmd, "failauth %u", &id) == 1)
            authfailed(id);
        else if(sscanf(cmd, "succauth %u", &id) == 1)
            authsucceeded(id);
        else if(sscanf(cmd, "chalauth %u %255s", &id, val) == 2)
            authchallenged(id, val);
        else if(!strncmp(cmd, "cleargbans", cmdlen))
            cleargbans();
        else if(sscanf(cmd, "addgban %100s", val) == 1)
            addgban(val);
    }

    VAR(autosendto, 0, 0, 1);

    void loadmap()
    {
        //if(!m_edit || !racemode) return;
        defformatstring(mapfile)("packages/base/%s.ogz", smapname);
        stream *nmapdata = openrawfile(mapfile, "rb");
        if(!nmapdata) return;
        DELETEP(mapdata);
        mapdata = nmapdata;
        if(autosendto)
        {
            loopv(clients)
            {
                clientinfo *ci = clients[i];
                if(ci->state.aitype!=AI_NONE || ci->getmap) continue;
                if((ci->getmap = sendfile(ci->clientnum, 2, mapdata, "ri", N_SENDMAP)))
                    ci->getmap->freeCallback = freegetmap;
                ci->needclipboard = totalmillis ? totalmillis : 1;
            }
        }
    }

    void receivefile(int sender, uchar *data, int len)
    {
        if(!m_edit || len > 8*1024*1024) return;
        clientinfo *ci = getinfo(sender);
        if(ci->state.state==CS_SPECTATOR && !ci->privilege && !ci->local) return;
        if(mapdata) DELETEP(mapdata);
        if(!len) return;
        mapdata = opentempfile("mapdata", "w+b");
        if(!mapdata) { sendf(sender, 1, "ris", N_SERVMSG, "failed to open temporary file for map"); return; }
        mapdata->write(data, len);
        sendservmsgf("[%s sent a map to server, \"/getmap\" to receive it]", colorname(ci));
        if(autosendto)
        {
            loopvj(clients)
            {
                clientinfo *cx = clients[j];
                if(!cx || cx->ownernum == ci->ownernum) continue;
                if(!mapdata || cx->getmap) continue;
                sendservmsgf("\f0[INFO]\f7: Automatically delivering the \f1map\f7 to \f2%s\f7.", colorname(cx));
                if((cx->getmap = sendfile(cx->clientnum, 2, mapdata, "ri", N_SENDMAP)))
                    cx->getmap->freeCallback = freegetmap;
                cx->needclipboard = totalmillis ? totalmillis : 1;
            }
        }
    }

    void sendclipboard(clientinfo *ci)
    {
        if(!ci->lastclipboard || !ci->clipboard) return;
        bool flushed = false;
        loopv(clients)
        {
            clientinfo &e = *clients[i];
            if(e.clientnum != ci->clientnum && e.needclipboard - ci->lastclipboard >= 0)
            {
                if(!flushed) { flushserver(true); flushed = true; }
                sendpacket(e.clientnum, 1, ci->clipboard);
            }
        }
    }

#ifndef WIN32
# include <dlfcn.h>
    struct gfunc
    {
        char n[64];
        void *f;
    };
    vector<gfunc *> gfuncs;

    void *getexternal(char *s)
    {
        loopv(gfuncs)
            if(!strcmp(gfuncs[i]->n, s) && gfuncs[i]->f) return gfuncs[i]->f;
        return 0;
    }

    bool addexternal(char *s, void *v)
    {
        loopv(gfuncs) if(!strcmp(gfuncs[i]->n, s)) return false;
        gfunc *p = new gfunc;
        if(!p) return false;
        gfuncs.add(p);
        strncpy(p->n, s, 64);
        p->f = v;
        return true;
    }

    void setexternal(char *s, void *v)
    {
        gfunc* cur = 0;
        loopv(gfuncs) if(!strcmp(gfuncs[i]->n, s)) cur = gfuncs[i];
        if(cur) cur->f = v;
        else addexternal(s, v);
    }

    struct module
    {
        string name;
        void *(*function)(char*args[16]);
        void *h;
    };
    vector<module> modules;

    int is_mod_loaded(const char *_name)
    {
        loopv(modules)
            if(!strcmp(modules[i].name, _name)) return i;
        return -1;
    }

    int _load(const char *_name, char args[16])
    {
        defformatstring(__name)("modules/mod_%s.so", _name);
        if(is_mod_loaded(_name) >= 0) return 1; // Module already loaded.
        module &mod = modules.add();
        copystring(mod.name, _name);
        mod.h = dlopen(__name, RTLD_NOW);
        if(!mod.h)
        {
            int i = is_mod_loaded(_name);
            modules.remove(i);
            return 2; // Module loading failed.
        }
        void *(*mod_main)(void*, void*, char*);
        typedef void *(* maintype)(void*, void*, char*);
        mod_main = (maintype)dlsym(mod.h, "mod_init");
        if(!mod_main)
        {
            dlclose(mod.h);
            int i = is_mod_loaded(_name);
            modules.remove(i);
            return 3; // Could not find mod_init.
        }
        mod_main((void*)getexternal, (void*)setexternal, args);
        void *(*mod_function)(char**);
        typedef void *(* functype)(char **);
        mod_function = (functype)dlsym(mod.h, "mod_func");
        if(!mod_function)
        {
            dlclose(mod.h);
            int i = is_mod_loaded(_name);
            modules.remove(i);
            return 4; // Could not find mod_func.
        }
        mod.function = mod_function;
        return 0;
    }

    int _unload(const char *_name)
    {
        int i = is_mod_loaded(_name);
        if(i < 0) return 1; // Module not loaded.
        void *(*mod_uninit)();
        typedef void *(* closetype)();
        mod_uninit = (closetype)dlsym(modules[i].h, "mod_close");
        if(!mod_uninit)
        {
            dlclose(modules[i].h);
            modules.remove(i);
            return 2; // Could not find mod_close.
        }
        mod_uninit();
        dlclose(modules[i].h);
        modules.remove(i);
        return 0;
    }

    int _reload(const char *_name, char args[16])
    {
        int i = _unload(_name);
        int j = _load(_name, args);
        return (i == 0 && j == 0) ? 0 : 1;
    }

    ICOMMAND(loadmod, "s", (const char * _s), {
        char chrs[16] = {};
        _load(_s, chrs);
    })
#endif

    void connected(clientinfo *ci)
    {
        if(m_demo) enddemoplayback();

        if(!hasmap(ci)) rotatemap(false);

        shouldstep = true;

        connects.removeobj(ci);
        clients.add(ci);

        ci->connectauth = 0;
        ci->connected = true;
        ci->needclipboard = totalmillis ? totalmillis : 1;
        if(mastermode>=MM_LOCKED) ci->state.state = CS_SPECTATOR;
        ci->state.lasttimeplayed = lastmillis;

        const char *worst = m_teammode ? chooseworstteam(NULL, ci) : NULL;
        copystring(ci->team, worst ? worst : "good", MAXTEAMLEN+1);

        sendwelcome(ci);
        if(restorescore(ci)) sendresume(ci);
        sendinitclient(ci);

        aiman::addclient(ci);

        if(m_demo) setupdemoplayback();

        if(servermotd[0]) sendf(ci->clientnum, 1, "ris", N_SERVMSG, servermotd);

        if(ci->isspy)
        {
            packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(p, N_INITCLIENT);
            putint(p, ci->clientnum);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
            putint(p, ci->playermodel);
            sendpacket(-1, 1, p.finalize());
            aiman::addclient(ci);
            sendf(-1, 1, "riii", N_SPECTATOR, ci->clientnum, 0);
        }

#ifndef WIN32
        int i = is_mod_loaded("geolocation");
        if(i >= 0)
        {
            char *z[16];
            z[0] = (char*)colorname(ci);
            z[1] = (char*)getclienthostname(ci->clientnum);
            modules[i].function(z);
        }
#endif
        if(m_edit && autosendto && mapdata && !ci->getmap)
        {
            sendservmsgf("\f0[INFO]\f7: Automatically delivering the \f1map\f7 to \f2%s\f7.", colorname(ci));
            if((ci->getmap = sendfile(ci->clientnum, 2, mapdata, "i", N_SENDMAP)))
                ci->getmap->freeCallback = freegetmap;
        }
        ci->forcespec = false;
        ci->mute = false;
        ci->emute = false;
        ci->nmute = false;
        ci->islooser = false;
        ci->isspy = false;
        ci->state.stolen = 0;
        ci->state.returned = 0;
        ci->spectimes = 0;
        if(m_edit && racemode)
        {
            ci->emute = true;
        }
        if(ci->state.state==CS_SPECTATOR) sendf(-1, 1, "riii", N_SPECTATOR, ci->clientnum, 1);
    }

    VARP(Debug, 0, 1, 1); // enables debug features.
    VARP(DebugErrorOnly, 0, 1, 1); // displays only the debug messages in case of errors.
    bool quit = false;
    int explodeString(char * string, char * array [], char separator = ' ', int count = 1024)
    {
        char *pch;
        for(int i = 0; i < count; i++) array[i]=NULL;
        if(!string || !*string) return 0;
        array[0] = string;
        int output = 1;
        for(int i = 1; i < count; i++)
        {
            pch = strchr(array[i-1], separator);
            if(!pch) break;
            *pch=0;
            pch++;
            while (*pch == separator) pch++;
            array[i] = pch;
            output++;
        }
        return output;
    }

    int explodeString(const char * _s, char * array [], char separator = ' ', int count = 1024)
    {
        return explodeString((char*)_s, array, separator, count);
    }

    void debug(bool s, bool e, int t, const char * n, const char * _e, const char * a, char * i [6], const char * _a)
    {
        //  s    = "short" message?
        //  e    = error?
        //  t    = type (0 = variable, 1 = function)
        //  n    = variable / function name
        // _e    = error message (only required if e == true, else put "")
        //  a    = expected arguments types
        //  i    = array of additional information
        //  i[0] = line where the function has been declared
        //  i[1] = file where the function has been declared
        //  i[2] = expected arguments count
        //  i[3] = given arguments count
        //  i[4] = additional information about the error
        //  i[5] = backtrace
        // _a    = given arguments
        if(!Debug || (DebugErrorOnly && !e)) return;
        switch (t)
        {
            case 0:
                if(e) logoutf("Debug: Variable %s (value %s) error: %s", n, a, _e);
                else logoutf("Debug: Variable %s new value %s", n, a);
                break;
            case 1:
                if(s)
                {
                    if(e) logoutf("Debug: Error in function %s(%s) called as %s(%s): %s", n, a, n, _a, _e);
                    else {
                        defformatstring (__e)("%s%s",
                            strcmp(_e, "") ? ", additional information: " : "",
                            _e
                        );
                        logoutf("Debug: %s(%s) called as %s(%s)%s",
                            n,
                            a,
                            n,
                            _a,
                            __e
                        );
                    }
                }
                else
                {
                    if(e) logoutf("Error: In function %s(%s)...", n, a);
                    else  logoutf("Debug: In function %s(%s)...", n, a);
                    logoutf("=================================");
                    string _s;
                    formatstring(_s)("%s%s%s%s%s%s%s%s%s",
                        i[0] ? "\nNote: declared here " : "",
                        i[0] ? i[0] : "",
                        i[1] ? ":"  : "",
                        i[1] ? i[1] : "",
                        i[0] && i[1] ? "\n": "",
                        i[0] && i[1] ?  n  : "",
                        i[0] && i[1] ? "(" : "",
                        i[0] && i[1] ?  a  : "",
                        i[0] && i[1] ? ")" : "");
                    logoutf("Function called as %s(%s)%s", n, _a, _s);
                    if(i[2])
                    {
                        logoutf("Expected arguments count: %s", i[2]);
                        if(i[3]) logoutf("Given arguments: %s", i[3]);
                    }
                    if(e)
                    {
                        logoutf("Error: %s", _e);
                        if(i[4]) logoutf("Additional information about the error:\n%s", i[4]);
                    }
                    else if(i[4]) logoutf("Additional information:\n%s", i[4]);
                    if(i[5] && strcmp(i[5], ""))
                    {
                        logoutf("\nBacktrace:");
                        logoutf("====================");
                        char * array [16];
                        explodeString(i[5], array, '\n', 16);
                        for(int i = 0; (array[i] && strcmp(array[i], "")); i++)
                            logoutf("#%i %s", i, array[i]);
                        logoutf("====================");
                        logoutf("End of backtrace.\n");
                    }
                    if(e)
                    {
                        logoutf("End of bug report. Please report this bug at");
                        logoutf("https://github.com/SauerMod/SM-Server/issues/new");
                        logoutf("with some additional information.");
                    }
                    logoutf("=================================");
                }
                if(e) quit = true;
                break;
            default:
                char *_a_i[6] = { };
                _a_i[0] = i[0];
                _a_i[1] = i[1];
                _a_i[2] = (char*)"8";
                _a_i[3] = (char*)"8";
                formatstring(_a_i[4])("invalid value for \"t\" (int, 3rd value) (given: %i, expected: 0...1)", t);
                _a_i[5] = (char*)"";
                defformatstring(__s)("%s, %s, %i, %s, %s, %s, {%s, %s, %s, %s, %s, Backtrace-Data}, %s",
                    s ? "true" : "false",
                    e ? "true" : "false",
                    t,
                    n,
                    _e,
                    a,
                    _a_i[0],
                    _a_i[1],
                    _a_i[2],
                    _a_i[3],
                    _a_i[4],
                    _a);

                debug(
                    false,
                    true,
                    1,
                    "server::debug",
                    "unknown server::debug usage",
                    "bool, bool, int, const char *, const char *, const char *, char * [5], const char *",
                    _a_i,
                    __s
                );
                break;
        }
    }
    void debugf(bool s, bool e, int t, const char * n, const char * _e, const char * a, char * i [6], const char * _a, ...) { defvformatstring(__a, _a, _a); debug(s, e, t, n, _e, a, i, __a); }

    struct command
    {
        string name;
        int priv;
        void (*func)(const char *, clientinfo *);
    };
    vector<command> commands;

    struct manpage
    {
        string command;
        string arguments;
        string manpage;
    };
    vector<manpage> manpages;

    bool addcmd(const char * name, int privilege, void (*function)(const char *, clientinfo *))
    {
        loopv(commands)
        {
            if(!strcmp(commands[i].name, name))
            {
                return false;
            }
        }
        int id = commands.length();
        commands.add();
        copystring(commands[id].name, name);
        commands[id].priv = privilege;
        commands[id].func = function;
        return true;
    }

    bool addman(const char * command, const char * arguments, const char * manpage)
    {
        loopv(manpages)
            if(!strcmp(manpages[i].command, command)) { return false; }
        uint id = manpages.length();
        manpages.add();
        copystring(manpages[id].command, command);
        copystring(manpages[id].arguments, arguments);
        copystring(manpages[id].manpage, manpage);
        return true;
    }

#define servcmdn(name, sname, privilege, mana, manp, function) template <int N> struct name; template <> struct name<__LINE__> { static bool initc; static bool initm; static void run (const char * args, clientinfo * ci); }; bool name<__LINE__>::initc = addcmd(sname, privilege, name<__LINE__>::run); bool name<__LINE__>::initm = addman(sname, mana, manp); void name<__LINE__>::run (const char * args, clientinfo * ci) { function; }
#define servcmdname(name) __servcmd_##name
#define servcmd(name, privilege, mana, manp, function) servcmdn(servcmdname(name), #name, privilege, mana, manp, function)

    int parsecommand (const char * text, clientinfo * ci)
    {
        if(!text || !*text)
            return 1;
        command * current = 0;
        char * input[2] = { };
        explodeString(text, input, ' ', 2);
        if(!input[0])
        {
            return 2;
        }
        loopv(commands)
        {
            if(!strcmp(commands[i].name,(const char *)input[0]))
            {
                current = &commands[i];
            }
        }
        if(!current)
        {
            sendmsgf(ci, "Unknown command: %s", input[0]);
            return 2;
        }
        if(current->priv > ci->privilege)
        {
            sendmsg(ci, "Permission denied.");
            return 3;
        }
        current->func((const char*)input[1], ci);
        return 0;
    }

    servcmd(help, PRIV_NONE, "(command)", "displays a list of commands, or displays help about a given command.", {
        char * array [2];
        explodeString (args, array, ' ', 2);
        if(array[0])
        {
            loopv(commands)
            {
                if(!strcmp(commands[i].name, array[0]))
                {
                    if(commands[i].priv > ci->privilege)
                    {
                        sendmsg(ci, "\f1Permission denied.");
                        return;
                    }
                }
            }
            loopv(manpages)
            {
                if(!strcmp(manpages[i].command, array[0]))
                {
                    sendmsgf(ci, "\f1\fsHelp: \f4#\f7%s %s\fr\fs: %s", array[0], manpages[i].arguments, manpages[i].manpage);
                    return;
                }
            }
            loopv(commands)
            {
                if(!strcmp(commands[i].name, array[0]))
                {
                    sendmsgf(ci, "\f1Help: No manpage for the command \fs\f7%s\fr could be found.", array[0]);
                    return;
                }
            }
            sendmsgf(ci, "\f1Unknown command: \f7%s", array[0]);
        }
        else
        {
            string message;
            copystring(message, "\f1Commands list: ");
            string bak;
            loopv(commands)
            {
                if(commands[i].priv == PRIV_NONE)
                {
                    copystring(bak, message);
                    formatstring(message)("%s\f4#\f7%s ", bak, commands[i].name);
                }
            }
            sendmsg(ci, message);
            if(ci->privilege < PRIV_MASTER) return;
            copystring(message, "");
            loopv(commands)
            {
                if(commands[i].priv == PRIV_MASTER)
                {
                    copystring(bak, message);
                    formatstring(message)("%s\f4#\f0%s ", bak, commands[i].name);
                }
            }
            sendmsg(ci, message);
            if(ci->privilege < PRIV_AUTH) return;
            copystring(message, "");
            loopv(commands)
            {
                if(commands[i].priv == PRIV_AUTH)
                {
                    copystring(bak, message);
                    formatstring(message)("%s\f4#\f1%s ", bak, commands[i].name);
                }
            }
            sendmsg(ci, message);
            if(ci->privilege < PRIV_ADMIN) return;
            copystring(message, "");
            loopv(commands)
            {
                if(commands[i].priv == PRIV_ADMIN)
                {
                    copystring(bak, message);
                    formatstring(message)("%s\f4#\f6%s ", bak, commands[i].name);
                }
            }
            sendmsg(ci, message);
            if(ci->privilege < PRIV_OWNER) return;
            copystring(message, "");
            loopv(commands)
            {
                if(commands[i].priv == PRIV_OWNER)
                {
                    copystring(bak, message);
                    formatstring(message)("%s\f4#\f3%s ", bak, commands[i].name);
                }
            }
            sendmsg(ci, message);
        }
    })

    servcmd(info, PRIV_NONE, "", "displays information about the server mod and the server.", {
        sendmsg(ci, "\fs\f0[INFO]\fr: running SM-Server Cube 2: Sauerbraten server modification.");
        sendmsgf(ci, "\fs\f0[INFO]\fr: running on a%s %s machine.", 
            sizeof(void*) == 8 ? " x86_64" : "n i686",
#ifdef _WIN32
                "Windows"
#else
# if defined(__linux__) || defined(__linux) || defined(linux) || defined(__gnu_linux__)
                "GNU/Linux"
# elif defined(__FreeBSD_kernel__) && defined(__GLIBC__)
                "GNU/FreeBSD"
# elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
                "FreeBSD"
# elif defined(__MACH__)
#  ifdef __APPLE__
                "OS X"
#  else
                "Mach"
#  endif
# else
                "Unknown"
# endif
#endif
        );
        int Time = totalsecs;
        int Years = 0;
        while(Time >=(60 * 60 * 24 * 30 * 12)) // Years
        {
            Years ++;
            Time -=(60 * 60 * 24 * 30 * 12);
        }
        int Months = 0;
        while(Time >=(60 * 60 * 24 * 30)) // Months
        {
            Months ++;
            Time -=(60 * 60 * 24 * 30);
        }
        int Days = 0;
        while(Time >=(60 * 60 * 24)) // Days
        {
            Days ++;
            Time -=(60 * 60 * 24);
        }
        int Hours = 0;
        while(Time >=(60 * 60)) // Hours
        {
            Hours ++;
            Time -=(60 * 60);
        }
        int Minutes = 0;
        while(Time >=(60)) // Minutes
        {
            Minutes ++;
            Time -=(60);
        }
        int Seconds = 0;
        while(Time >=(1)) // Seconds
        {
            Seconds ++;
            Time -= (1);
        }
        defformatstring(ymsg)(" \fs\f0%i\fr year%s,", Years, Years != 1 ? "s" : "");
        defformatstring(mmsg)(" \fs\f0%i\fr month%s,", Months, Months != 1 ? "s" : "");
        defformatstring(dmsg)(" \fs\f0%i\fr day%s,", Days, Days != 1 ? "s" : "");
        defformatstring(hmsg)(" \fs\f0%i\fr hour%s,", Hours, Hours != 1 ? "s" : "");
        defformatstring(Mmsg)(" \fs\f0%i\fr minute%s and", Minutes, Minutes != 1 ? "s" : "");
        defformatstring(smsg)(" \fs\f0%i\fr second%s.", Seconds, Seconds != 1 ? "s" : "");
        sendmsgf (ci, "\fs\f0[INFO]\fr: this server has been up for%s%s%s%s%s%s",
            (totalsecs >= (60 * 60 * 24 * 30 * 12)) ? ymsg : "",
            (totalsecs >= (60 * 60 * 24 * 30)) ? mmsg : "",
            (totalsecs >= (60 * 60 * 24)) ? dmsg : "",
            (totalsecs >= (60 * 60)) ? hmsg : "",
            (totalsecs >= (60)) ? Mmsg : "",
            (totalsecs >= (1)) ? smsg : ""
        );
    })

    servcmd(flagrun, PRIV_NONE, "", "displays the best flagrun on the current map and mode.", {
        if(!m_ctf) { sendmsg(ci, "\f0[FLAGRUN]\f7: This is \f3no \f2CTF \f7gamemode."); return; }
        loopv(frs) if(!strcmp(frs[i].map, smapname) && frs[i].mode == gamemode)
        {
            double frseconds = (double)frs[i].scoremillis/1000.0;
            sendmsgf(ci, "\f0[FLAGRUN]\f7: \f3Best \f2flagrun \f7on this \f2map \f1and \f6mode\f7: \f1%s, \f0%.3f \f6seconds\f7.",
                frs[i].scorer,
                frseconds
            );
            return;
        }
        sendmsg(ci, "\f0[FLAGRUN]\f7: There is \f2currently \f3no \f7best \f2flagrun \f7on this \f2map \f1and \f6mode\f7.");
    })

    servcmd(pm, PRIV_NONE, "<cn> <message>", "delivers a private message to a player.", {
        char * array [2] = {};
        explodeString((char *) args, array, ' ', 2);
        if(array[0] && array[1])
        {
            char * cns [128];
            explodeString(array[0], cns, ',', 128);
            for(int i = 0; cns[i]; i++)
            {
                int cn = atoi(cns[i]);
                clientinfo * cx = getinfo(cn);
                if(!cx)
                {
                    sendmsgf(ci, "\f0[INFO]\f7: \f6Unknown \f1client \f1number\f7: \f5%i", cn);
                }
                else
                {
                    sendmsgf(cx, "\f0[PM]\f7: \f1%s \f7has sent you the \f0following \f6private \f2message\f7: \f1%s\f7.", colorname(ci), array[1]);
                    sendmsgf(ci, "\f0[PM]\f7: \f1Your \f6private \f2message \f7has been \f0succsessfully \f7delivered to \f1%s\f7.", colorname(cx));
                }
            }
        }
        else
        {
            sendmsg(ci, "\f0[INFO]\f7: \f6Usage: \f5#\f2pm \f4<\f1cn\f4[,\f1cn2\f4[,...]]> <\f6message\f4>");
        }
    })

    servcmd(stats, PRIV_NONE, "[cn[,cn2[,...]]]", "displays current game statistics for the specified player(s).", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        string message;
        string bak;
        if(!array[0])
        {
            formatstring(message)("\f0[STATS]\f7: Current \f2game \f6statistics \f7for \f1%s\f7: \f4%i \f6frags \f7- \f4%i \f2deaths \f0(kpd: \f4%.3f\f0) \f7/ \f3accuracy: \f4%.3f%%",
                colorname(ci),
                ci->state.frags,
                ci->state.deaths,
                ((float)ci->state.frags/max(ci->state.deaths, 1)),
                (float)(ci->state.damage*100/max(ci->state.shotdamage, 1))
            );
            if(m_teammode)
            {
                copystring(bak, message);
                formatstring(message)("%s \f7/ \f4%i \f5teamkills", bak, ci->state.teamkills);
            }
            if(m_ctf || m_collect)
            {
                copystring(bak, message);
                formatstring(message)("%s \f7/ \f2%s \f1scored: \f4%i", bak, (m_ctf && !m_collect) ? "flags" : "skulls", ci->state.flags);
                if(!m_collect)
                {
                    copystring(bak, message);
                    formatstring(message)("%s \f7/ \f2flags \f0stolen: \f4%i \f7/ \f2flags \f5returned: \f4%i", bak, ci->state.stolen, ci->state.returned);
                }
            }
            sendmsg(ci, message);
            return;
        }
        char*cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            if(cn == -1)
            {
                loopv(clients)
                {
                    clientinfo *cx = clients[i];
                    if(!cx || cx->isspy) { continue; }
                    formatstring(message)("\f0[STATS]\f7: Current \f2game \f6statistics \f7for \f1%s\f7: \f4%i \f6frags \f7- \f4%i \f2deaths \f0(kpd: \f4%.3f\f0) \f7/ \f3accuracy: \f4%.3f%%",
                        colorname(cx),
                        cx->state.frags,
                        cx->state.deaths,
                        ((float)cx->state.frags/max(cx->state.deaths, 1)),
                        (float)(cx->state.damage*100/max(cx->state.shotdamage, 1))
                    );
                    if(m_teammode)
                    {
                        copystring(bak, message);
                        formatstring(message)("%s \f7/ \f4%i \f5teamkills", bak, cx->state.teamkills);
                    }
                    if(m_ctf || m_collect)
                    {
                        copystring(bak, message);
                        formatstring(message)("%s \f7/ \f2%s \f1scored: \f4%i", bak, (m_ctf && !m_collect) ? "flags" : "skulls", cx->state.flags);
                        if(!m_collect)
                        {
                            copystring(bak, message);
                            formatstring(message)("%s \f7/ \f2flags \f0stolen: \f4%i \f7/ \f2flags \f5returned: \f4%i", bak, cx->state.stolen, cx->state.returned);
                        }
                    }
                    sendmsg(ci, message);
                }
                break;
            }
            clientinfo *cx = getinfo(cn);
            if(!cx || cx->isspy) { sendmsgf(ci, "Unknown client number: %i", cn); continue; }
            formatstring(message)("\f0[STATS]\f7: Current \f2game \f6statistics \f7for \f1%s\f7: \f4%i \f6frags \f7- \f4%i \f2deaths \f0(kpd: \f4%.3f) \f7/ \f3accuracy: \f4%.3f%%",
                colorname(cx),
                cx->state.frags,
                cx->state.deaths,
                ((float)cx->state.frags/max(cx->state.deaths, 1)),
                (float)(cx->state.damage*100/max(cx->state.shotdamage, 1))
            );
            if(m_teammode)
            {
                copystring(bak, message);
                formatstring(message)("%s \f7/ \f4%i \f5teamkills", bak, cx->state.teamkills);
            }
            if(m_ctf || m_collect)
            {
                copystring(bak, message);
                formatstring(message)("%s \f7/ \f2%s \f1scored: \f4%i", bak, (m_ctf && !m_collect) ? "flags" : "skulls", cx->state.flags);
                if(!m_collect)
                {
                    copystring(bak, message);
                    formatstring(message)("%s \f7/ \f2flags \f0stolen: \f4%i \f7/ \f2flags \f5returned: \f4%i", bak, cx->state.stolen, cx->state.returned);
                }
            }
            sendmsg(ci, message);
        }
    })

    inline bool revbool(bool a)
        { return !a; }

    servcmd(emute, PRIV_MASTER, "<cn> [0/1]", "toggles edit mute for a client.", {
        char*array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0]) { sendmsg(ci, "Usage: #emute <cn> [0/1]"); return; }
        int cn = atoi(array[0]);
        if(cn == -1)
        {
            loopv(clients)
            {
                clientinfo *cx = clients[i];
                if(array[1])
                {
                    int a = atoi(array[1]);
                    cx->emute = (a!=0);
                }
                else cx->emute = revbool(cx->emute);
                sendmsgf(ci, "%s has been edit-%smuted.", colorname(cx), cx->emute?"":"un");
                sendmsgf(cx, "you have been edit-%smuted.", cx->emute?"":"un");
            }
        }
        clientinfo *cx = getinfo(cn);
        if(!cx) return;
        if(array[1])
        {
            int a = atoi(array[1]);
            cx->emute = (a!=0);
        }
        else cx->emute = revbool(cx->emute);
        sendmsgf(ci, "%s has been edit-%smuted.", colorname(cx), cx->emute?"":"un");
        sendmsgf(cx, "you have been edit-%smuted.", cx->emute?"":"un");
    })

    servcmd(sendto, PRIV_MASTER, "<cn[,cn2[,...]]>", "forces a client to getmap.", {
        char * Array[2];
        explodeString(args, Array, ' ', 2);
        if(!Array[0]) { sendmsg(ci, "usage: #sendto <cn[,cn2[,...]]>"); return; }
        char * cns[128];
        explodeString(Array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            if(cn == -1)
            {
                loopvj(clients)
                {
                    clientinfo *cx = clients[j];
                    if(!cx) continue;
                    if(!mapdata || cx->getmap) return;
                    sendservmsgf("\f0[INFO]\f7: Delivering the \f1map\f7 to \f2%s\f7.", colorname(cx));
                    if((cx->getmap = sendfile(cx->clientnum, 2, mapdata, "ri", N_SENDMAP)))
                        cx->getmap->freeCallback = freegetmap;
                    cx->needclipboard = totalmillis ? totalmillis : 1;
                }
                continue;
            }
            clientinfo *cx = getinfo(cn);
            if(!cx) sendmsgf(ci, "Unknown client number: %i", cn);
            else
            {
                if(!mapdata || cx->getmap) return;
                sendservmsgf("\f0[INFO]\f7: Delivering the \f1map\f7 to \f2%s\f7.", colorname(cx));
                if((cx->getmap = sendfile(cx->clientnum, 2, mapdata, "ri", N_SENDMAP)))
                    cx->getmap->freeCallback = freegetmap;
                cx->needclipboard = totalmillis ? totalmillis : 1;
            }
        }
    })

    void givepriv(clientinfo *ci, int priv)
    {
        packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
        if(!ci) return;
        defformatstring(msg)("\f1%s \f7%s %s%s%s",
            colorname(ci),
            priv == PRIV_NONE ? "relinquished" : "claimed",
            hidepriv && (priv > PRIV_AUTH || (priv == PRIV_NONE && ci->privilege > PRIV_AUTH)) ? "\f4invisible " : "",
            priv == PRIV_NONE ? privcolor(ci->privilege) : privcolor(priv),
            privname(priv == PRIV_NONE ? ci->privilege : priv)
        );
        bool val = priv == PRIV_NONE;
        if((hidepriv && priv > PRIV_AUTH) || ci->isspy)
        {
            sendmsg(ci, msg);
            if(!ci->isspy)
            {
                loopv(clients) if(((val && clients[i]->privilege >= priv) || (!val && clients[i]->privilege >= PRIV_ADMIN)) && clients[i] != ci) sendmsg(clients[i], msg);
            }
        }
        else
        {
            putint(p, N_SERVMSG);
            sendstring(msg, p);
        }
        putint(p, N_CURRENTMASTER);
        putint(p, mastermode);
        ci->privilege = priv;
        loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && !clients[i]->isspy)
        {
            if(clients[i]->privilege > PRIV_AUTH && hidepriv) continue;
            putint(p, clients[i]->clientnum);
            putint(p, clients[i]->privilege);
        }
        putint(p, -1);
        sendpacket(-1, 1, p.finalize());
        if(hidepriv)
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege == PRIV_MASTER || clients[j]->privilege == PRIV_AUTH)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
            sendpacket(-1, 1, z.finalize());
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
            {
                clientinfo *cx = clients[j];
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CURRENTMASTER);
                putint(q, mastermode);
                loopv(clients) if((clients[i]->privilege >= PRIV_MASTER && clients[i]->privilege <= cx->privilege && !clients[i]->isspy) || clients[i]->clientnum == cx->clientnum)
                {
                    putint(q, clients[i]->clientnum);
                    putint(q, clients[i]->privilege);
                }
                putint(q, -1);
                sendpacket(cx->clientnum, 1, q.finalize());
            }
        }
        else
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
            sendpacket(-1, 1, z.finalize());
        }
        checkpausegame();
    }

    servcmd(setpriv, PRIV_MASTER, "<cn[,cn2[,...]]> <none/master/auth/admin>", "gives the specified privileges to a client.", {
        char*array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0] || !array[1]) { sendmsg(ci, "usage: #setpriv <cn[,cn2[,...]]> <none/master/auth/admin>"); return; }
        int wantpriv = PRIV_NONE;
        if(!strcmp(array[1], "none")) wantpriv = PRIV_NONE;
        else if(!strcmp(array[1], "master")) wantpriv = PRIV_MASTER;
        else if(!strcmp(array[1], "auth")) wantpriv = PRIV_AUTH;
        else if(!strcmp(array[1], "admin")) wantpriv = PRIV_ADMIN;
        else { sendmsg(ci, "usage: #setpriv <cn[,cn2[,...]]> <none/master/auth/admin>"); return; }
        if(wantpriv > ci->privilege) { sendmsg(ci, "\f0[SETPRIV]\f7: \f3Permission \f2denied\f7."); return; } 
        char *cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            clientinfo *cx = getinfo(cn);
            if(!cx) { sendmsgf(ci, "\f0[SETPRIV]\f7: \f3Unknown \f7client \f1number\f7: \f0%i", cn); continue; }
            if(cx->privilege >= ci->privilege && cx->clientnum != ci->clientnum) { sendmsg(ci, "\f0[SETPRIV]\f7: Permission \f3denied\f7."); continue; }
            givepriv(cx, wantpriv);
            sendmsgf(cx, "\f0[SETPRIV]\f7: \f1%s \f7has given you %s%s \f7privileges.", colorname(ci), privcolor(wantpriv), privname(wantpriv));
        }
    })

    servcmd(givemaster, PRIV_MASTER, "<cn[,cn2[,...]]>", "gives master privileges to a client.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #givemaster <cn[,cn2[,...]]>"); return; }
        char *cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            clientinfo *cx = getinfo(cn);
            if(!cx) { sendmsgf(ci, "\f0[GIVEMASTER]\f7: \f3Unknown \f7client \f1number\f7: \f0%i", cn); continue; }
            if(cx->privilege >= ci->privilege && cx->clientnum != ci->clientnum) { sendmsg(ci, "\f0[GIVEMASTER]\f7: Permission \f3denied\f7."); continue; }
            givepriv(cx, PRIV_MASTER);
            sendmsgf(cx, "\f0[GIVEMASTER]\f7: \f1%s \f7has given you \f0master \f7privileges.", colorname(ci));
        }
    })

    servcmd(persist, PRIV_AUTH, "[0/1]", "Enables teams persisting.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(array[0])
        {
            int i = atoi(array[0]);
            persistteams = (i != 0) ? true : false;
        }
        else persistteams = revbool(persistteams);
        sendservmsgf("Team persisting has been %sabled.", persistteams ? "en" : "dis");
    })


    servcmd(mute, PRIV_AUTH, "<cn> [0/1]", "toggles text-message mute for a client.", {
        char*array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0]) { sendmsg(ci, "Usage: #mute <cn> [0/1]"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) return;
        if(array[1])
        {
            int a = atoi(array[1]);
            cx->mute = (a!=0);
        }
        else cx->mute = revbool(cx->mute);
        sendmsgf(ci, "%s has been %smuted.", colorname(cx), cx->mute?"":"un");
        sendmsgf(cx, "you have been %smuted.", cx->mute?"":"un");
    })

    servcmd(nmute, PRIV_AUTH, "<cn> [0/1]", "toggles name mute for a client.", {
        char*array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0]) { sendmsg(ci, "Usage: #nmute <cn> [0/1]"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) return;
        if(array[1])
        {
            int a = atoi(array[1]);
            cx->nmute = (a!=0);
        }
        else cx->nmute = revbool(cx->nmute);
        sendmsgf(ci, "%s has been name-%smuted.", colorname(cx), cx->nmute?"":"un");
        sendmsgf(cx, "you have been name-%smuted.", cx->nmute?"":"un");
    })

    bool smute = false;

    servcmd(smute, PRIV_AUTH, "[0/1]", "toggles text-message mute for all spectators.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) smute = revbool(smute);
        else
        {
            int a = atoi(array[0]);
            smute = (a!=0);
        }
        sendservmsgf("Text-message mute has been %sabled for all spectators.", smute?"en":"dis");
    })

    servcmd(persistb, PRIV_AUTH, "[0/1]", "Enables bots persisting.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(array[0])
        {
            int i = atoi(array[0]);
            persistbots = (i != 0) ? true : false;
        }
        else persistbots = revbool(persistbots);
        sendservmsgf("Bots persisting has been %sabled.", persistbots ? "en" : "dis");
    })

    servcmd(rename, PRIV_AUTH, "<cn[,cn2[,...]]>", "Renames one or multiple clients.", {
        char *array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0] || !array[1]) { sendmsg(ci, "usage: #rename <cn[,cn2[,...]]> <new name>"); return; }
        char *cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            clientinfo *cx = getinfo(atoi(cns[i]));
            if(!cx) { sendmsgf(ci, "\f0[RENAME]\f7: \f3Unknown \f2client \f1number\f7: \f0%i", atoi(cns[i])); continue; }
            if(cx != ci && cx->privilege >= ci->privilege) { sendmsg(ci, "\f0[RENAME]\f7: Permission \f3denied\f7."); continue; }
            if(!strcmp(cx->name, array[1])) continue;
            copystring(cx->name, (const char*)array[1]);
            uchar buf[MAXSTRLEN];
            ucharbuf b(buf, MAXSTRLEN);
            putint(b, N_SWITCHNAME);
            sendstring(cx->name, b);
            packetbuf p(MAXSTRLEN, ENET_PACKET_FLAG_RELIABLE);
            putint(p, N_CLIENT);
            putint(p, cx->clientnum);
            putint(p, b.len);
            p.put(buf, b.len);
            sendpacket(-1, 1, p.finalize());
            sendmsgf(cx, "\f0[RENAME]\f7: \f1You \f7have been \f2renamed \f7to \f1%s\f7.", array[1]);
        }
    })

    servcmd(autosendto, PRIV_AUTH, "[0/1]", "toggles automaitcally sendto after sendmap.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) autosendto = autosendto==1?0:1;
        else
        {
            int a = atoi(array[0]);
            autosendto = a==0?0:1;
        }
        sendservmsgf("\f0[INFO]\f7: \f1Automatically \f6sendto \f7at \f2sendmap \f4or at \f2connection \f7has been %sabled\f7..", autosendto?"\f0en":"\f3dis");
    })

    servcmd(getip, PRIV_AUTH, "<cn>", "displays a player's IP-Address.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #getip <cn>"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) { sendmsgf(ci, "unknown client num: %i", cn); return; }
#ifndef WIN32
        bool geolocation_is_enabled = false;
        int i = is_mod_loaded("geolocation");
        typedef const char * (* getclientlocationtype)(const char *);
        getclientlocationtype getclientlocation;
        if(i >= 0)
        {
            getclientlocation = (getclientlocationtype)getexternal((char*)"geolocation_client_location");
            if(getclientlocation) geolocation_is_enabled = true;
        }
#endif
        sendmsgf(ci, "\f0[INFO]\f7: \f1%s\f7's \f2IP-Address\f7: \f3%s\f7%s%s", colorname(cx), getclienthostname(cn),
#ifndef WIN32
            geolocation_is_enabled ? " " : "",
            geolocation_is_enabled ? getclientlocation(getclienthostname(cn)) : ""
#else
            "", ""
#endif
        );
    })

    servcmd(fspec, PRIV_AUTH, "<cn>", "forces a client to spectate.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #fspec <cn>"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) { sendmsgf(ci, "unknown client num: %i", cn); return; }
        cx->forcespec = true;
        if(cx->state.state==CS_ALIVE) suicide(cx);
        if(smode) smode->leavegame(cx);
        cx->state.state = CS_SPECTATOR;
        cx->state.timeplayed += lastmillis - cx->state.lasttimeplayed;
        if(!cx->local && !cx->privilege) aiman::removeai(cx);
        sendf(-1, 1, "ri3", N_SPECTATOR, cn, 1);
    })

    servcmd(funspec, PRIV_AUTH, "<cn>", "removes a client's forcespectate.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #fspec <cn>"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) { sendmsgf(ci, "unknown client num: %i", cn); return; }
        cx->forcespec = false;
        cx->state.state = CS_DEAD;
        cx->state.respawn();
        cx->state.lasttimeplayed = lastmillis;
        aiman::addclient(cx);
        if(cx->clientmap[0] || cx->mapcrc) checkmaps();
        sendf(-1, 1, "ri3", N_SPECTATOR, cn, 0);        
    })

    servcmd(intermission, PRIV_AUTH, "", "Starts the intermission.", {
        if(racemode) execute("clearsleep");
        startintermission();
        sendservmsgf("\f0[INFO]\f7: \f1%s \f7has \f3forced \f7the intermission.", colorname(ci));
    })

    servcmd(hidepriv, PRIV_ADMIN, "[0/1]", "toggles privilege hiding.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) hidepriv = revbool(hidepriv);
        else
        {
            int a = atoi(array[0]);
            hidepriv = (a!=0);
        }
        if(hidepriv)
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege == PRIV_MASTER || clients[j]->privilege == PRIV_AUTH)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
            sendpacket(-1, 1, z.finalize());
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
            {
                clientinfo *cx = clients[j];
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CURRENTMASTER);
                putint(q, mastermode);
                loopv(clients) if((clients[i]->privilege >= PRIV_MASTER && clients[i]->privilege <= cx->privilege && !clients[i]->isspy) || clients[i]->clientnum == cx->clientnum)
                {
                    putint(q, clients[i]->clientnum);
                    putint(q, clients[i]->privilege);
                }
                putint(q, -1);
                sendpacket(cx->clientnum, 1, q.finalize());
            }
        }
        else
        {
            packetbuf z(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(z, N_CURRENTMASTER);
            putint(z, mastermode);
            loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER && !clients[j]->isspy)
            {
                putint(z, clients[j]->clientnum);
                putint(z, clients[j]->privilege);
            }
            putint(z, -1);
        }
        sendmsgf(ci, "\f0[INFO]\f7: \f2Privileges \f7are %s \f0hidden\f7.", hidepriv ? "\f1now" : "\f3no more");
    })

    servcmd(wall, PRIV_ADMIN, "<message>", "Sends an anonymous message", {
        if(!args || !*args) { sendmsg(ci, "usage: #wall <message>"); return; }
        sendservmsgf(args);
    })

    servcmd(achat, PRIV_ADMIN, "<message>", "Sends a message to the admins-only chat.", {
        if(!args || !*args) { sendmsg(ci, "usage: #achat <message>"); return; }
        loopv(clients) if(clients[i]->privilege >= PRIV_ADMIN) sendmsgf(clients[i], "\f0[ADMINS-CHAT]\f7: \f1%s\f7: \f6%s", colorname(ci), args);
    })

    servcmd(ban, PRIV_ADMIN, "<cn> <time in minutes> [reason]", "kicks and bans a client with the specified ban duration.", {
        char *array[3];
        explodeString(args, array, ' ', 3);
        if(!array[0]||!array[1]) { sendmsg(ci, "usage: #ban <cn> <time in minutes> [reason]"); return; }
        int cn = atoi(array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) { sendmsgf(ci, "Unknown client number: %i", cn); return; }
        sendservmsgf("%s has banned %s for %i minutes%s%s",
            colorname(ci),
            colorname(cx),
            atoi(array[1]),
            array[2]?", reason: ":"",
            array[2]?array[2]:""
        );
        addban(getclientip(cn), atoi(array[1])*60*1000);
        disconnect_client(cn, DISC_IPBAN);
    })

    servcmd(unban, PRIV_ADMIN, "<id>", "unbans a banned client.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #unban <id>. You can find the ids of the banned clients with #listbans."); return; }
        int j = atoi(array[0]);
        loopv(bannedips) if(i==(j-1)) { int j = allowedips.length(); allowedips.add(); allowedips[j]=bannedips[i].ip; bannedips.remove(i); }
        sendmsgf(ci, "The ban with the ID %i has been succsessfully unbanned.", j);
    })

    servcmd(listbans, PRIV_ADMIN, "", "lists all banned clients.", {
        loopv(bannedips)
        {
            sendmsgf(ci, "ID %i, IP-Address: %i.%i.%i.%i, Expires: %i minutes", 
                i+1,
                bannedips[i].ip&0xFF, (bannedips[i].ip>>8)&0xFF, (bannedips[i].ip>>16)&0xFF, (bannedips[i].ip>>24)&0xFF,
                (bannedips[i].expire-totalmillis)/60/1000
            );
        }
    })


    servcmd(pban, PRIV_ADMIN, "<cn> [reason]", "kicks and permamently bans a client.", {
        char * Array[2];
        explodeString(args, Array, ' ', 2);
        if(!Array[0]) { sendmsg(ci, "usage: #pban <cn> [reason]"); return; }
        int cn = atoi(Array[0]);
        clientinfo *cx = getinfo(cn);
        if(!cx) sendmsgf(ci, "Unknown client number: %i", cn);
        else
        {
            if(cx->privilege >= ci->privilege) { sendmsg(ci, "Permission denied."); return; }
            addpban(getclienthostname(cx->clientnum), Array[1]?:"Unknown");
            sendservmsgf("\f0[INFO]\f7: \f1%s \f7has \f0added \f1%s \f5(\f2%s\f5) \f7to the list of \f3permamently \f2banned \f7clients for \f2%s\f7.", colorname(ci), colorname(cx), getclienthostname(cx->clientnum), Array[1] ? Array[1] : "an unknown reason");
            disconnect_client(cx->clientnum, DISC_IPBAN);
        }
    })

    servcmd(rpban24, PRIV_ADMIN, "<ip (e.g. 123.123.123)> [reason]", "permamently bans a /24 IP range.", {
        char * Array[2];
        explodeString(args, Array, ' ', 2);
        if(!Array[0]) { sendmsg(ci, "usage: #rpban24 <ip (e.g. 123.123.123)> [reason]"); return; }
        else addrpban24(Array[0], Array[1]?:"Unknown");
    })

    servcmd(rpban16, PRIV_ADMIN, "<ip (e.g. 123.123)> [reason]", "permamently bans a /16 IP range.", {
        char * Array[2];
        explodeString(args, Array, ' ', 2);
        if(!Array[0]) { sendmsg(ci, "usage: #rpban16 <ip (e.g. 123.123)> [reason]"); return; }
        else addrpban16(Array[0], Array[1]?:"Unknown");
    })

    servcmd(rpban8, PRIV_ADMIN, "<ip (e.g. 123)> [reason]", "permamently bans a /8 IP range.", {
        char * Array[2];
        explodeString(args, Array, ' ', 2);
        if(!Array[0]) { sendmsg(ci, "usage: #rpban8 <ip (e.g. 123)> [reason]"); return; }
        else addrpban8(Array[0], Array[1]?:"Unknown");
    })

    servcmd(unpban, PRIV_ADMIN, "<id>", "unbans a permamenlty banned client.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #unpban <id>. You can find the ids of the pbanned clients with #listpbans."); return; }
        int j = atoi(array[0]);
        bool removed = true;
        loopv(pbans) if(i==(j-1)) { removed = true; pbans.remove(i); }
        sendmsgf(ci, "\f0[INFO]\f7: The \f3PBan \f7with the \f1ID \f2%i \f7has been %ssuccsessfully \f7removed.", j, removed ? "\f0" : "\f6un");
    })

    servcmd(unrpban, PRIV_ADMIN, "<id>", "unbans a permamently banned range.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #unrpban <id>. You can find the ids of the pbanned clients with #listpbans."); return; }
        int j = atoi(array[0]);
        bool removed = true;
        int x = 1;
        loopv(pbans) if(!pbans[i].hide) x++;
        string ip_range;
        int type = 0;
        loopv(rpbans) if(i==(j-x)) { type = rpbans[i].type; copystring(ip_range, (const char *)rpbans[i].Range); removed = true; rpbans.remove(i); }
        switch(type)
        {
            case 24:
                for(int i = 0; i < 255; i++)
                {
                    defformatstring(curaddr)("%s.%i", ip_range, i);
                    loopv(pbans) if(!strcmp((const char *)pbans[i].IP, (const char *)curaddr)) pbans.remove(i);
                }
                break;
            case 16:
                for(int i = 0; i < 255; i++)
                {
                    for(int j = 0; j < 255; j++)
                    {
                        defformatstring(curaddr)("%s.%i.%i", ip_range, i, j);
                        loopv(pbans) if(!strcmp((const char *)pbans[i].IP, (const char *)curaddr)) pbans.remove(i);
                    }
                }
                break;
            case 8:
                for(int i = 0; i < 255; i++)
                {
                    for(int j = 0; j < 255; j++)
                    {
                        for(int z = 0; z < 255; z++)
                        {
                            defformatstring(curaddr)("%s.%i.%i.%i", ip_range, i, j, z);
                            loopv(pbans) if(!strcmp((const char *)pbans[i].IP, (const char *)curaddr)) pbans.remove(i);
                        }
                    }
                }
                break;
        }
        sendmsgf(ci, "\f0[INFO]\f7: The \f3RPBan \f7with the \f1ID \f2%i \f7has been %ssuccsessfully \f7removed.", j, removed ? "\f0" : "\f6un");
    })

    servcmd(listpbans, PRIV_ADMIN, "", "lists all permemently banned clients.", {
        bool nopbans = true;
        int id = 1;
        loopv(pbans)
        {
            if(pbans[i].hide) continue;
            sendmsgf(ci, "\f0[INFO]\f7: \f1ID \f2%i\f7, \f6IP-Address: \f3%s\f7, \f5Reason: \f7%s.", 
                id,
                pbans[i].IP,
                pbans[i].Reason[0]?pbans[i].Reason:"Unknown"
            );
            nopbans = false;
            id++;
        }
        loopv(rpbans)
        {
            string range;
            switch(rpbans[i].type)
            {
                case 24:
                    formatstring(range)("%s.0/24", rpbans[i].Range);
                    break;
                case 16:
                    formatstring(range)("%s.0.0/16", rpbans[i].Range);
                    break;
                case 8:
                    formatstring(range)("%s.0.0.0/8", rpbans[i].Range);
                    break;
            }
            sendmsgf(ci, "\f0[INFO]\f7: \f1ID \f2%i\f7, \f6Range: \f3%s\f7, \f5Reason: \f7%s.",
                id,
                range,
                rpbans[i].Reason?rpbans[i].Reason:"Unknown"
            );
            nopbans = false;
            id++;
        }
        if(nopbans)
            sendmsg(ci, "\f0[INFO]\f7: \f2At the moment\f7, there are no \f3PBans\f7.");
    })

    servcmd(giveadmin, PRIV_ADMIN, "<cn[,cn2[,...]]>", "gives admin privileges to a client.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #giveadmin <cn[,cn2[,...]]>"); return; }
        char *cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            clientinfo *cx = getinfo(cn);
            if(!cx) { sendmsgf(ci, "\f0[GIVEADMIN]\f7: \f3Unknown \f7client \f1number\f7: \f0%i", cn); continue; }
            if(cx->privilege >= ci->privilege && cx->clientnum != ci->clientnum) { sendmsg(ci, "\f0[GIVEADMIN]\f7: Permission \f3denied\f7."); continue; }
            givepriv(cx, PRIV_ADMIN);
            sendmsgf(cx, "\f0[GIVEADMIN]\f7: \f1%s \f7has given you \f6admin \f7privileges.", colorname(ci));
        }
    })

    servcmd(revokepriv, PRIV_ADMIN, "<cn[,cn2[,...]]>", "revokes a client's privileges.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #revokepriv <cn[,cn2[,...]]>"); return; }
        char *cns[128];
        explodeString(array[0], cns, ',', 128);
        for(int i = 0; cns[i]; i++)
        {
            int cn = atoi(cns[i]);
            clientinfo *cx = getinfo(cn);
            if(!cx) { sendmsgf(ci, "\f0[REVOKEPRIV]\f7: \f3Unknown \f7client \f1number\f7: \f0%i", cn); continue; }
            if(cx->privilege >= ci->privilege && cx->clientnum != ci->clientnum) { sendmsg(ci, "\f0[REVOKEPRIV]\f7: Permission \f3denied\f7."); continue; }
            int oldpriv = cx->privilege;
            givepriv(cx, PRIV_NONE);
            sendmsgf(cx, "\f0[REVOKEPRIV]\f7: \f1%s \f7has \f3revoken \f7your %s%s \f7privileges.", colorname(ci), privcolor(oldpriv), privname(oldpriv));
        }
    })

    servcmd(spy, PRIV_ADMIN, "[0/1]", "Toggles spy mode.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        bool newvalue;
        if(!array[0]) newvalue = ci->isspy ? false : true;
        else
        {
            int i = atoi(array[0]);
            newvalue = (i!=0) ? true : false;
        }
        if(newvalue)
        {
            if(ci->state.state!=CS_SPECTATOR)
            {
                if(ci->state.state==CS_ALIVE) suicide(ci);
                if(smode) smode->leavegame(ci);
                ci->state.timeplayed += lastmillis - ci->state.lasttimeplayed;
            }
            aiman::removeai(ci);
            ci->forcespec = true;
            if(!hidepriv)
            {
                packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                defformatstring(tmp)("%s relinquished %s", colorname(ci), privname(ci->privilege));
                putint(p, N_SERVMSG);
                sendstring(tmp, p);
                putint(p, N_CURRENTMASTER);
                putint(p, mastermode);
                loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && (clients[i]->privilege <= PRIV_AUTH || !hidepriv) && !clients[i]->isspy)
                {
                    putint(p, clients[i]->clientnum);
                    putint(p, clients[i]->privilege);
                }
                putint(p, -1);
                sendpacket(-1, 1, p.finalize());
                if(hidepriv)
                {
                    loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
                    {
                        clientinfo *cx = clients[j];
                        packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                        putint(q, N_CURRENTMASTER);
                        putint(q, mastermode);
                        loopv(clients) if((clients[i]->privilege >= PRIV_MASTER && clients[i]->privilege <= cx->privilege && !clients[i]->isspy) || clients[i]->clientnum == cx->clientnum)
                        {
                            putint(q, clients[i]->clientnum);
                            putint(q, clients[i]->privilege);
                        }
                        putint(q, -1);
                        sendpacket(cx->clientnum, 1, q.finalize());
                    }
                }
                else
                {
                    packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                    putint(q, N_CURRENTMASTER);
                    putint(q, mastermode);
                    loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && !clients[i]->isspy)
                    {
                        putint(q, clients[i]->clientnum);
                        putint(q, clients[i]->privilege);
                    }
                    putint(q, -1);
                    sendpacket(ci->ownernum, 1, q.finalize());
                }
            }
            sendf(-1, 1, "rxi2", ci->clientnum, N_CDIS, ci->clientnum);
            sendf(ci->clientnum, 1, "ri3", N_SPECTATOR, ci->clientnum, 1);
            sendmsg(ci, "\f3{SPY} \f1You have joined the spy-mode.");
        }
        else
        {
            packetbuf p(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
            putint(p, N_INITCLIENT);
            putint(p, ci->clientnum);
            sendstring(ci->name, p);
            sendstring(ci->team, p);
            putint(p, ci->playermodel);
            sendpacket(-1, 1, p.finalize());
            aiman::addclient(ci);
            ci->forcespec = false;
            sendf(-1, 1, "riii", N_SPECTATOR, ci->clientnum, ci->state.state==CS_SPECTATOR ? 1 : 0);
#ifndef WIN32
            int i = is_mod_loaded("geolocation");
            if(i >= 0)
            {
                char *z[16];
                z[0] = (char*)colorname(ci);
                z[1] = (char*)getclienthostname(ci->clientnum);
                modules[i].function(z);
            }
#endif
            if(hidepriv)
            {
                loopvj(clients) if(clients[j]->privilege >= PRIV_MASTER)
                {
                    clientinfo *cx = clients[j];
                    packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                    putint(q, N_CURRENTMASTER);
                    putint(q, mastermode);
                    loopv(clients) if((clients[i]->privilege >= PRIV_MASTER && clients[i]->privilege <= cx->privilege && !clients[i]->isspy) || clients[i]->clientnum == cx->clientnum)
                    {
                        putint(q, clients[i]->clientnum);
                        putint(q, clients[i]->privilege);
                    }
                    putint(q, -1);
                    sendpacket(cx->clientnum, 1, q.finalize());
                }
            }
            else
            {
                packetbuf q(MAXTRANS, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CURRENTMASTER);
                putint(q, mastermode);
                loopv(clients) if(clients[i]->privilege >= PRIV_MASTER && !clients[i]->isspy)
                {
                    putint(q, clients[i]->clientnum);
                    putint(q, clients[i]->privilege);
                }
                putint(q, -1);
                sendpacket(ci->clientnum, 1, q.finalize());
            }
            sendmsg(ci, "\f3{SPY} \f1You have left the spy-mode.");
        }
        ci->isspy = newvalue;
    })

    ICOMMAND(wall, "s", (const char *s), {
        if(!s||!*s) return;
        sendservmsg(s);
    })

    servcmd(racemode, PRIV_ADMIN, "[0/1]", "Toggles race mode", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) racemode = revbool(racemode);
        else
        {
            int a = atoi(array[0]);
            racemode = (a!=0);
        }
        sendservmsgf("\f0[INFO]\f7: Race mode has been %sabled.", racemode?"en":"dis");
        if(racemode)
        {
            execute("lrinitialize");
            curlr = 0;
            loopv(clients)
            {
                clients[i]->emute = true;
            }
        }
        else execute("lruninitialize");
    })

    servcmd(halt, PRIV_OWNER, "", "Closes the server.", {
        quit = true;
    })

    servcmd(exec, PRIV_OWNER, "<Cubescript code>", "Runs a cubescript code.", {
        char*array[1];
        explodeString(args, array, ' ', 1);
        if(!array[0]) { sendmsg(ci, "usage: #exec <cubescript code>"); return; }
        execute(array[0]);
        loopv(clients)
        {
            clientinfo *cx = clients[i];
            if(cx->privilege==PRIV_OWNER) sendmsgf(cx, "\f0[EXEC]\f7: \f1%s \f7has runned the following \f2CubeScript \f6code\f7: \f3%s", colorname(ci), array[0]);
        }
    })

#ifndef WIN32
    servcmd(load, PRIV_OWNER, "<module>", "loads a module.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #load <module>"); return; }
        char chrs[16] = {};
        switch(_load(array[0], chrs))
        {
            case 1:
                sendmsg(ci, "This module already has been loaded.");
                return;
            case 2:
                sendmsgf(ci, "Module loading failed (%s).", dlerror());
                return;
            case 3:
                sendmsg(ci, "Could not find mod_init.");
                return;
            case 4:
                sendmsg(ci, "Could not find mod_func.");
                return;
            default:
                sendmsg(ci, "Module succsessfully loaded.");
                break;
        }
        sendservmsgf("\f0[MODULE]\f7: \f1%s \f7has \f0loaded \f7the \f6%s \f2module\f7.", colorname(ci), array[0]);
    })

    servcmd(unload, PRIV_OWNER, "<module>", "unloads a module.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #unload <module>"); return; }
        switch(_unload(array[0]))
        {
            case 1:
                sendmsg(ci, "This module hasn't been initialized yet.");
                return;
            case 2:
                sendmsg(ci, "Could not find mod_close.");
                return;
            default:
                sendmsg(ci, "Module succsessfully unloaded.");
                break;
        }
        sendservmsgf("\f0[MODULE]\f7: \f1%s \f7has \f3unloaded \f7the \f6%s \f2module\f7.", colorname(ci), array[0]);
    })

    servcmd(reload, PRIV_OWNER, "<module>", "reloads a module.", {
        char*array[2];
        explodeString(args, array, ' ', 2);
        if(!array[0]) { sendmsg(ci, "usage: #reload <module>"); return; }
        char chrs[16] = {};
        switch(_reload(array[0], chrs))
        {
            case 1:
                sendmsgf(ci, "An error occured while reloading the module %s.", array[0]);
                return;
            default:
                sendmsg(ci, "Module succsessfully reloaded.");
                break;
        }
        sendservmsgf("\f0[MODULE]\f7: \f1%s \f7has \f1reloaded \f7the \f6%s \f2module\f7.", colorname(ci), array[0]);
    })
#endif

    ICOMMAND(pban, "ss", (const char *ip, const char *reason), {
        addpban(ip, reason);
    });

    ICOMMAND(pbanr24, "ss", (const char *range, const char *reason), {
        addrpban24(range, reason);
    })

    ICOMMAND(pbanr16, "ss", (const char *range, const char *reason), {
        addrpban16(range, reason);
    })

    ICOMMAND(pbanr8, "ss", (const char *range, const char *reason), {
        addrpban24(range, reason);
    })

    void close()
    {
        stream *f = openutf8file("pban.cfg", "w");
        if(f)
        {
            f->printf("// Automatically generated by SM-Server - do not edit.\n");
            f->printf("// Contains a list of permamently banned clients.\n");
            loopv(pbans) if(!pbans[i].hide) f->printf("pban %s \"%s\"\n", pbans[i].IP, pbans[i].Reason);
            loopv(rpbans) f->printf("pbanr%i %s \"%s\"\n", rpbans[i].type, rpbans[i].Range, rpbans[i].Reason);
            delete f;
        }
        stream *ff = openutf8file("flagruns.cfg", "w");
        if(ff)
        {
            ff->printf("// Automatically generated by SM-Server - do not edit.\n");
            ff->printf("// Contains a list of flagruns.\n");
            loopv(frs) f->printf("flagrun %s %s %i %i\n", frs[i].scorer, frs[i].map, frs[i].scoremillis, frs[i].mode);
            delete ff;
        }
        stream *fff = openutf8file("race.cfg", "w");
        if(fff)
        {
            fff->printf("// Automatically generated by SM-Server - do not exit.\n");
            fff->printf("// Contains a list of raceruns.\n");
            loopv(raceruns) fff->printf("addbestracen %s %s %i\n", raceruns[i].name, raceruns[i].map, raceruns[i].millis);
            delete fff;
        }
    }

#define addextn(name, sname, function) template <int N> struct sname; template <> struct sname<__LINE__> { static bool init; }; bool sname<__LINE__>::init = addexternal((char*)#name, (void*)function);
#define addext(name, function) addextn(name, __gfunc_##name, function)

    addext(sendservmsgf, sendservmsgf)

    ICOMMAND(fspec, "ii", (int *value, int *cn), {
        if(!value || !cn) return;
        clientinfo *cx = getinfo(*cn);
        if(!cx) return;
        if(*value == 1)
        {
            cx->forcespec = true;
            if(cx->state.state==CS_ALIVE) suicide(cx);
            if(smode) smode->leavegame(cx);
            cx->state.state = CS_SPECTATOR;
            cx->state.timeplayed += lastmillis - cx->state.lasttimeplayed;
            if(!cx->local && !cx->privilege) aiman::removeai(cx);
            sendf(-1, 1, "ri3", N_SPECTATOR, *cn, 1);
        }
        else
        {
            cx->forcespec = false;
            cx->state.state = CS_DEAD;
            cx->state.respawn();
            cx->state.lasttimeplayed = lastmillis;
            aiman::addclient(cx);
            if(cx->clientmap[0] || cx->mapcrc) checkmaps();
            sendf(-1, 1, "ri3", N_SPECTATOR, *cn, 0);  
        }
    })

    void spec5sec(clientinfo *ci)
    {
        if(!ci) return;
        string command;
        if(ci->spectimes >= 2)
        {
            formatstring(command)("fspec 1 %i", ci->clientnum);
            sendservmsgf("\f0[RACE-INFO]\f7: \f1%s \f5(%i) \f7has been spectated for the \f2entire race\f7 (\f3caught cheating \f13 \f3times\f7).", ci->name, ci->clientnum);
            execute(command);
        }
        else
        {
            if(ci->spectimes == 1) ci->islooser = true;
            sendservmsgf("\f0[RACE-INFO]\f7: \f1%s \f5(%i) \f7has been spectated for \f15 \f7seconds (\f3caught cheating\f7).", ci->name, ci->clientnum);
            formatstring(command)("c%i = [fspec 1 %i; sleep 5000 [fspec 0 %i]]; c%i", ci->clientnum, ci->clientnum, ci->clientnum, ci->clientnum);
            execute(command);
            ci->spectimes++;
        }
    }

    void parsepacket(int sender, int chan, packetbuf &p)     // has to parse exactly each byte of the packet
    {
        if(sender<0 || p.packet->flags&ENET_PACKET_FLAG_UNSEQUENCED || chan > 2) return;
        char text[MAXTRANS];
        int type;
        clientinfo *ci = sender>=0 ? getinfo(sender) : NULL, *cq = ci, *cm = ci;
        if(ci && !ci->connected)
        {
            if(chan==0) return;
            else if(chan!=1) { disconnect_client(sender, DISC_MSGERR); return; }
            else while(p.length() < p.maxlen) switch(checktype(getint(p), ci))
            {
                case N_CONNECT:
                {
                    getstring(text, p);
                    filtertext(text, text, false, MAXNAMELEN);
                    if(!text[0]) copystring(text, "unnamed");
                    copystring(ci->name, text, MAXNAMELEN+1);
                    ci->playermodel = getint(p);

                    string password, authdesc, authname;
                    getstring(password, p, sizeof(password));
                    getstring(authdesc, p, sizeof(authdesc));
                    getstring(authname, p, sizeof(authname));
                    int disc = allowconnect(ci, password);
                    if(disc)
                    {
                        if(disc == DISC_LOCAL || !serverauth[0] || strcmp(serverauth, authdesc) || !tryauth(ci, authname, authdesc))
                        {
                            disconnect_client(sender, disc);
                            return;
                        }
                        ci->connectauth = disc;
                    }
                    else
                    {
                        if(!ispban(getclienthostname(ci->clientnum))) connected(ci);
                        else { disconnect_client(sender, DISC_IPBAN); return; }
                    }
                    break;
                }

                case N_AUTHANS:
                {
                    string desc, ans;
                    getstring(desc, p, sizeof(desc));
                    uint id = (uint)getint(p);
                    getstring(ans, p, sizeof(ans));
                    if(!answerchallenge(ci, id, ans, desc)) 
                    {
                        disconnect_client(sender, ci->connectauth);
                        return;
                    }
                    break;
                }

                case N_PING:
                    getint(p);
                    break;

                default:
                    disconnect_client(sender, DISC_MSGERR);
                    return;
            }
            return;
        }
        else if(chan==2)
        {
            receivefile(sender, p.buf, p.maxlen);
            return;
        }

        if(p.packet->flags&ENET_PACKET_FLAG_RELIABLE) reliablemessages = true;
        #define QUEUE_AI clientinfo *cm = cq;
        #define QUEUE_MSG { if(cm && (!cm->local || demorecord || hasnonlocalclients())) while(curmsg<p.length()) cm->messages.add(p.buf[curmsg++]); }
        #define QUEUE_BUF(body) { \
            if(cm && (!cm->local || demorecord || hasnonlocalclients())) \
            { \
                curmsg = p.length(); \
                { body; } \
            } \
        }
        #define QUEUE_INT(n) QUEUE_BUF(putint(cm->messages, n))
        #define QUEUE_UINT(n) QUEUE_BUF(putuint(cm->messages, n))
        #define QUEUE_STR(text) QUEUE_BUF(sendstring(text, cm->messages))
        int curmsg;
        while((curmsg = p.length()) < p.maxlen) switch(type = checktype(getint(p), ci))
        {
            case N_POS:
            {
                int pcn = getuint(p); 
                p.get(); 
                uint flags = getuint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                vec pos;
                loopk(3)
                {
                    int n = p.get(); n |= p.get()<<8; if(flags&(1<<k)) { n |= p.get()<<16; if(n&0x800000) n |= -1<<24; }
                    pos[k] = n/DMF;
                }
                loopk(3) p.get();
                int mag = p.get(); if(flags&(1<<3)) mag |= p.get()<<8;
                int dir = p.get(); dir |= p.get()<<8;
                vec vel = vec((dir%360)*RAD, (clamp(dir/360, 0, 180)-90)*RAD).mul(mag/DVELF);
                if(flags&(1<<4))
                {
                    p.get(); if(flags&(1<<5)) p.get();
                    if(flags&(1<<6)) loopk(2) p.get();
                }
                if(cp)
                {
                    if((!ci->local || demorecord || hasnonlocalclients()) && (cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING))
                    {
                        if(!ci->local && !m_edit && max(vel.magnitude2(), (float)fabs(vel.z)) >= 180)
                            cp->setexceeded();
                        cp->position.setsize(0);
                        while(curmsg<p.length()) cp->position.add(p.buf[curmsg++]);
                    }
                    if(smode && cp->state.state==CS_ALIVE) smode->moved(cp, cp->state.o, cp->gameclip, pos, (flags&0x80)!=0);
                    cp->state.o = pos;
                    cp->gameclip = (flags&0x80)!=0;
                }
                break;
            }

            case N_TELEPORT:
            {
                int pcn = getint(p), teleport = getint(p), teledest = getint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                if(cp && (!ci->local || demorecord || hasnonlocalclients()) && (cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING))
                {
                    flushclientposition(*cp);
                    sendf(-1, 0, "ri4x", N_TELEPORT, pcn, teleport, teledest, cp->ownernum); 
                }
                break;
            }

            case N_JUMPPAD:
            {
                int pcn = getint(p), jumppad = getint(p);
                clientinfo *cp = getinfo(pcn);
                if(cp && pcn != sender && cp->ownernum != sender) cp = NULL;
                if(cp && (!ci->local || demorecord || hasnonlocalclients()) && (cp->state.state==CS_ALIVE || cp->state.state==CS_EDITING))
                {
                    cp->setpushed();
                    flushclientposition(*cp);
                    sendf(-1, 0, "ri3x", N_JUMPPAD, pcn, jumppad, cp->ownernum);
                }
                break;
            }
                
            case N_FROMAI:
            {
                int qcn = getint(p);
                if(qcn < 0) cq = ci;
                else
                {
                    cq = getinfo(qcn);
                    if(cq && qcn != sender && cq->ownernum != sender) cq = NULL;
                }
                break;
            }

            case N_EDITMODE:
            {
                int val = getint(p);
                if(!ci->local && !m_edit) { ac(ci, EDITMODE); return; };
                if(val ? ci->state.state!=CS_ALIVE && ci->state.state!=CS_DEAD : ci->state.state!=CS_EDITING) break;
                if(racemode) { spec5sec(ci); return; }
                if(smode)
                {
                    if(val) smode->leavegame(ci);
                    else smode->entergame(ci);
                }
                if(val)
                {
                    ci->state.editstate = ci->state.state;
                    ci->state.state = CS_EDITING;
                    ci->events.setsize(0);
                    ci->state.rockets.reset();
                    ci->state.grenades.reset();
                }
                else ci->state.state = ci->state.editstate;
                QUEUE_MSG;
                break;
            }

            case N_MAPCRC:
            {
                getstring(text, p);
                int crc = getint(p);
                if(!ci) break;
                if(strcmp(text, smapname))
                {
                    if(ci->clientmap[0])
                    {
                        ci->clientmap[0] = '\0';
                        ci->mapcrc = 0;
                    }
                    else if(ci->mapcrc > 0) ci->mapcrc = 0;
                    break;
                }
                copystring(ci->clientmap, text);
                ci->mapcrc = text[0] ? crc : 1;
                checkmaps();
                break;
            }

            case N_CHECKMAPS:
                checkmaps(sender);
                break;

            case N_TRYSPAWN:
                if(!ci || !cq || cq->state.state!=CS_DEAD || cq->state.lastspawn>=0 || (smode && !smode->canspawn(cq)) || ci->isspy) break;
                if(!ci->clientmap[0] && !ci->mapcrc)
                {
                    ci->mapcrc = -1;
                    checkmaps();
                }
                if(cq->state.deadflush)
                {
                    flushevents(cq, cq->state.deadflush);
                    cq->state.respawn();
                }
                cleartimedevents(cq);
                sendspawn(cq);
                break;

            case N_GUNSELECT:
            {
                int gunselect = getint(p);
                if(!cq || cq->state.state!=CS_ALIVE) break;
                cq->state.gunselect = gunselect >= GUN_FIST && gunselect <= GUN_PISTOL ? gunselect : GUN_FIST;
                QUEUE_AI;
                QUEUE_MSG;
                break;
            }

            case N_SPAWN:
            {
                int ls = getint(p), gunselect = getint(p);
                if(!cq || (cq->state.state!=CS_ALIVE && cq->state.state!=CS_DEAD) || ls!=cq->state.lifesequence || cq->state.lastspawn<0 || ci->isspy) break;
                cq->state.lastspawn = -1;
                cq->state.state = CS_ALIVE;
                cq->state.gunselect = gunselect >= GUN_FIST && gunselect <= GUN_PISTOL ? gunselect : GUN_FIST;
                cq->exceeded = 0;
                if(smode) smode->spawned(cq);
                QUEUE_AI;
                QUEUE_BUF({
                    putint(cm->messages, N_SPAWN);
                    sendstate(cq->state, cm->messages);
                });
                break;
            }

            case N_SUICIDE:
            {
                if(cq) cq->addevent(new suicideevent);
                break;
            }

            case N_SHOOT:
            {
                shotevent *shot = new shotevent;
                shot->id = getint(p);
                shot->millis = cq ? cq->geteventmillis(gamemillis, shot->id) : 0;
                shot->gun = getint(p);
                loopk(3) shot->from[k] = getint(p)/DMF;
                loopk(3) shot->to[k] = getint(p)/DMF;
                int hits = getint(p);
                loopk(hits)
                {
                    if(p.overread()) break;
                    hitinfo &hit = shot->hits.add();
                    hit.target = getint(p);
                    hit.lifesequence = getint(p);
                    hit.dist = getint(p)/DMF;
                    hit.rays = getint(p);
                    loopk(3) hit.dir[k] = getint(p)/DNF;
                }
                if(cq) 
                {
                    cq->addevent(shot);
                    cq->setpushed();
                }
                else delete shot;
                break;
            }

            case N_EXPLODE:
            {
                explodeevent *exp = new explodeevent;
                int cmillis = getint(p);
                exp->millis = cq ? cq->geteventmillis(gamemillis, cmillis) : 0;
                exp->gun = getint(p);
                exp->id = getint(p);
                int hits = getint(p);
                loopk(hits)
                {
                    if(p.overread()) break;
                    hitinfo &hit = exp->hits.add();
                    hit.target = getint(p);
                    hit.lifesequence = getint(p);
                    hit.dist = getint(p)/DMF;
                    hit.rays = getint(p);
                    loopk(3) hit.dir[k] = getint(p)/DNF;
                }
                if(cq) cq->addevent(exp);
                else delete exp;
                break;
            }

            case N_ITEMPICKUP:
            {
                int n = getint(p);
                if(!cq) break;
                pickupevent *pickup = new pickupevent;
                pickup->ent = n;
                cq->addevent(pickup);
                break;
            }

            case N_TEXT:
            {
//                QUEUE_AI;
//                QUEUE_MSG;
                getstring(text, p);
                if(text[0] == '#')
                {
                    parsecommand(&text[1], cq);
                    return;
                }
                filtertext(text, text);
                if(cq->mute) return;
                if(cq->isspy)
                {
                    sendservmsgf("\f3{REMOTE} \f7%s\f1: %s", colorname(cq), text);
                    return;
                }
                if(cq->state.state==CS_SPECTATOR && smute)
                {
                    loopv(clients)
                    {
                        clientinfo *cx = clients[i];
                        if(cx==cq || cx->state.state!=CS_SPECTATOR || cx->state.aitype != AI_NONE) continue;
                        sendf(cx->clientnum, 1, "riis", N_TEXT, cq->clientnum, text);
                    }
                    return;
                }
                QUEUE_AI;
                QUEUE_INT(N_TEXT);
                QUEUE_STR(text);
                if(isdedicatedserver() && cq) logoutf("%s: %s", colorname(cq), text);
                break;
            }

            case N_SAYTEAM:
            {
                getstring(text, p);
                if(!ci || !cq) return;
                if(cq->isspy)
                {
                    loopv(clients)
                    {
                        clientinfo *t = clients[i];
                        if(!t->isspy) continue;
                        sendmsgf(t, "\f3{REMOTE-CHAT} \f7%s\f1: %s", colorname(cq), text);
                    }
                    return;
                }
                if(cq->mute) return;
                if(cq->state.state==CS_SPECTATOR)
                {
                    loopv(clients)
                    {
                        clientinfo *t = clients[i];
                        if(t==cq || t->state.state!=CS_SPECTATOR || t->state.aitype != AI_NONE) continue;
                        sendf(t->clientnum, 1, "riis", N_SAYTEAM, cq->clientnum, text);
                    }
                    return;
                }
                if(!m_teammode || !cq->team[0]) return;
                loopv(clients)
                {
                    clientinfo *t = clients[i];
                    if(t==cq || t->state.state==CS_SPECTATOR || t->state.aitype != AI_NONE || strcmp(cq->team, t->team)) continue;
                    sendf(t->clientnum, 1, "riis", N_SAYTEAM, cq->clientnum, text);
                }
                if(isdedicatedserver() && cq) logoutf("%s <%s>: %s", colorname(cq), cq->team, text);
                break;
            }

            case N_SWITCHNAME:
            {
//                QUEUE_MSG;
                if(ci->nmute) return;
                getstring(text, p);
                filtertext(ci->name, text, false, MAXNAMELEN);
                if(!ci->name[0]) copystring(ci->name, "unnamed");
                QUEUE_INT(N_SWITCHNAME);
                QUEUE_STR(ci->name);
                break;
            }

            case N_SWITCHMODEL:
            {
                ci->playermodel = getint(p);
                QUEUE_MSG;
                break;
            }

            case N_SWITCHTEAM:
            {
                getstring(text, p);
                filtertext(text, text, false, MAXTEAMLEN);
                if(m_teammode && text[0] && strcmp(ci->team, text) && (!smode || smode->canchangeteam(ci, ci->team, text)) && addteaminfo(text))
                {
                    if(ci->state.state==CS_ALIVE) suicide(ci);
                    copystring(ci->team, text);
                    aiman::changeteam(ci);
                    sendf(-1, 1, "riisi", N_SETTEAM, sender, ci->team, ci->state.state==CS_SPECTATOR ? -1 : 0);
                }
                break;
            }

            case N_MAPVOTE:
            {
                getstring(text, p);
                filtertext(text, text, false);
                int reqmode = getint(p);
                vote(text, reqmode, sender);
                break;
            }

            case N_ITEMLIST:
            {
                if((ci->state.state==CS_SPECTATOR && !ci->privilege && !ci->local) || !notgotitems || strcmp(ci->clientmap, smapname)) { while(getint(p)>=0 && !p.overread()) getint(p); break; }
                int n;
                while((n = getint(p))>=0 && n<MAXENTS && !p.overread())
                {
                    server_entity se = { NOTUSED, 0, false };
                    while(sents.length()<=n) sents.add(se);
                    sents[n].type = getint(p);
                    if(canspawnitem(sents[n].type))
                    {
                        if(m_mp(gamemode) && delayspawn(sents[n].type)) sents[n].spawntime = spawntime(sents[n].type);
                        else sents[n].spawned = true;
                    }
                }
                notgotitems = false;
                break;
            }

            case N_EDITF: 
            case N_EDITT:
            case N_EDITM:
            case N_FLIP:
            case N_ROTATE:
            case N_REPLACE:
            case N_DELCUBE:
            {
                int size = server::msgsizelookup(type);
                if(size <= 0) { ac(ci, MESSAGESIZE, size); return; }
                loopi(size - 1) getint(p);
                if(!m_edit) { ac(ci, EDITMSG); return; }
                if(ci->emute) return;
                if(cq && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }

            case N_EDITENT:
            {
                int i = getint(p);
                loopk(3) getint(p);
                int type = getint(p);
                loopk(5) getint(p);
                if(!m_edit) { ac(ci, EDITENT); return; }
                if(ci->emute) return;
                if(!ci || ci->state.state==CS_SPECTATOR) break;
                QUEUE_MSG;
                bool canspawn = canspawnitem(type);
                if(i<MAXENTS && (sents.inrange(i) || canspawnitem(type)))
                {
                    server_entity se = { NOTUSED, 0, false };
                    while(sents.length()<=i) sents.add(se);
                    sents[i].type = type;
                    if(canspawn ? !sents[i].spawned : (sents[i].spawned || sents[i].spawntime))
                    {
                        sents[i].spawntime = canspawn ? 1 : 0;
                        sents[i].spawned = false;
                    }
                }
                break;
            }

            case N_EDITVAR:
            {
                int type = getint(p);
                getstring(text, p);
                switch(type)
                {
                    case ID_VAR: getint(p); break;
                    case ID_FVAR: getfloat(p); break;
                    case ID_SVAR: getstring(text, p);
                }
                if(ci && ci->state.state!=CS_SPECTATOR) QUEUE_MSG;
                break;
            }

            case N_PING:
                sendf(sender, 1, "i2", N_PONG, getint(p));
                break;

            case N_CLIENTPING:
            {
                int ping = getint(p);
                if(ci)
                {
                    ci->ping = ping;
                    loopv(ci->bots) ci->bots[i]->ping = ping;
                }
                if(!ci->isspy) QUEUE_MSG;
                break;
            }

            case N_MASTERMODE:
            {
                int mm = getint(p);
                if((ci->privilege || ci->local) && mm>=MM_OPEN && mm<=MM_PRIVATE)
                {
                    if((ci->privilege>=PRIV_ADMIN || ci->local) || (mastermask&(1<<mm)))
                    {
                        mastermode = mm;
                        allowedips.shrink(0);
                        if(mm>=MM_PRIVATE)
                        {
                            loopv(clients) allowedips.add(getclientip(clients[i]->clientnum));
                        }
                        sendf(-1, 1, "rii", N_MASTERMODE, mastermode);
                        //sendservmsgf("mastermode is now %s (%d)", mastermodename(mastermode), mastermode);
                    }
                    else
                    {
                        defformatstring(s)("mastermode %d is disabled on this server", mm);
                        sendf(sender, 1, "ris", N_SERVMSG, s);
                    }
                }
                break;
            }

            case N_CLEARBANS:
            {
                if(ci->privilege || ci->local)
                {
                    bannedips.shrink(0);
                    sendservmsgf("\f0[INFO]\f7: All \f3bans \f7have been \f0cleared \f7by \f1%s\f7.", colorname(ci));
                }
                break;
            }

            case N_KICK:
            {
                int victim = getint(p);
                getstring(text, p);
                filtertext(text, text);
                trykick(ci, victim, text);
                break;
            }

            case N_SPECTATOR:
            {
                int spectator = getint(p), val = getint(p);
                if(!ci->privilege && !ci->local && (spectator!=sender || (ci->state.state==CS_SPECTATOR && (mastermode>=MM_LOCKED || ci->forcespec)))) break;
                clientinfo *spinfo = (clientinfo *)getclientinfo(spectator); // no bots
                if(!spinfo || (spinfo->state.state==CS_SPECTATOR ? val : !val)) break;

                if(spinfo->state.state!=CS_SPECTATOR && val)
                {
                    if(spinfo->state.state==CS_ALIVE) suicide(spinfo);
                    if(smode) smode->leavegame(spinfo);
                    spinfo->state.state = CS_SPECTATOR;
                    spinfo->state.timeplayed += lastmillis - spinfo->state.lasttimeplayed;
                    if(!spinfo->local && !spinfo->privilege) aiman::removeai(spinfo);
                }
                else if(spinfo->state.state==CS_SPECTATOR && !val)
                {
                    spinfo->state.state = CS_DEAD;
                    spinfo->state.respawn();
                    spinfo->state.lasttimeplayed = lastmillis;
                    aiman::addclient(spinfo);
                    if(spinfo->clientmap[0] || spinfo->mapcrc) checkmaps();
                }
                sendf(-1, 1, "ri3", N_SPECTATOR, spectator, val);
                if(!val && !hasmap(spinfo)) rotatemap(true);
                break;
            }

            case N_SETTEAM:
            {
                int who = getint(p);
                getstring(text, p);
                filtertext(text, text, false, MAXTEAMLEN);
                if(!ci->privilege && !ci->local) break;
                clientinfo *wi = getinfo(who);
                if(!m_teammode || !text[0] || !wi || !strcmp(wi->team, text)) break;
                if((!smode || smode->canchangeteam(wi, wi->team, text)) && addteaminfo(text))
                {
                    if(wi->state.state==CS_ALIVE) suicide(wi);
                    copystring(wi->team, text, MAXTEAMLEN+1);
                }
                aiman::changeteam(wi);
                sendf(-1, 1, "riisi", N_SETTEAM, who, wi->team, 1);
                break;
            }

            case N_FORCEINTERMISSION:
                if(ci->local && !hasnonlocalclients()) startintermission();
                break;

            case N_RECORDDEMO:
            {
                int val = getint(p);
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER) && !ci->local) break;
                if(!maxdemos || !maxdemosize) 
                {
                    sendf(ci->clientnum, 1, "ris", N_SERVMSG, "the server has disabled demo recording");
                    break;
                }
                demonextmatch = val!=0;
                sendservmsgf("demo recording is %s for next match", demonextmatch ? "enabled" : "disabled");
                break;
            }

            case N_STOPDEMO:
            {
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER) && !ci->local) break;
                stopdemo();
                break;
            }

            case N_CLEARDEMOS:
            {
                int demo = getint(p);
                if(ci->privilege < (restrictdemos ? PRIV_ADMIN : PRIV_MASTER) && !ci->local) break;
                cleardemos(demo);
                break;
            }

            case N_LISTDEMOS:
                if(!ci->privilege && !ci->local && ci->state.state==CS_SPECTATOR) break;
                listdemos(sender);
                break;

            case N_GETDEMO:
            {
                int n = getint(p);
                if(!ci->privilege && !ci->local && ci->state.state==CS_SPECTATOR) break;
                senddemo(ci, n);
                break;
            }

            case N_GETMAP:
                if(!mapdata) sendf(sender, 1, "ris", N_SERVMSG, "no map to send");
                else if(ci->getmap) sendf(sender, 1, "ris", N_SERVMSG, "already sending map");
                else
                {
                    sendservmsgf("[%s is getting the map]", colorname(ci));
                    if((ci->getmap = sendfile(sender, 2, mapdata, "ri", N_SENDMAP)))
                        ci->getmap->freeCallback = freegetmap;
                    ci->needclipboard = totalmillis ? totalmillis : 1;
                }
                break;

            case N_NEWMAP:
            {
                int size = getint(p);
                if(!ci->privilege && !ci->local && ci->state.state==CS_SPECTATOR) break;
                if(size>=0)
                {
                    smapname[0] = '\0';
                    resetitems();
                    notgotitems = false;
                    if(smode) smode->newmap();
                }
                QUEUE_MSG;
                break;
            }

            case N_SETMASTER:
            {
                int mn = getint(p), val = getint(p);
                getstring(text, p);
                if(mn != ci->clientnum)
                {
                    if(!ci->privilege && !ci->local) break;
                    clientinfo *minfo = (clientinfo *)getclientinfo(mn);
                    if(!minfo || (!ci->local && minfo->privilege >= ci->privilege) || (val && minfo->privilege)) break;
                    setmaster(minfo, val!=0, "", NULL, NULL, PRIV_MASTER, true);
                }
                else setmaster(ci, val!=0, text);
                // don't broadcast the master password
                break;
            }

            case N_ADDBOT:
            {
                aiman::reqadd(ci, getint(p));
                break;
            }

            case N_DELBOT:
            {
                aiman::reqdel(ci);
                break;
            }

            case N_BOTLIMIT:
            {
                int limit = getint(p);
                if(ci) aiman::setbotlimit(ci, limit);
                break;
            }

            case N_BOTBALANCE:
            {
                int balance = getint(p);
                if(ci) aiman::setbotbalance(ci, balance!=0);
                break;
            }

            case N_AUTHTRY:
            {
                string desc, name;
                getstring(desc, p, sizeof(desc));
                getstring(name, p, sizeof(name));
                tryauth(ci, name, desc);
                break;
            }

            case N_AUTHKICK:
            {
                string desc, name;
                getstring(desc, p, sizeof(desc));
                getstring(name, p, sizeof(name));
                int victim = getint(p);
                getstring(text, p);
                filtertext(text, text);
                int authpriv = PRIV_AUTH;
                if(desc[0])
                {
                    userinfo *u = users.access(userkey(name, desc));
                    if(u) authpriv = u->privilege; else break;
                }
                if(trykick(ci, victim, text, name, desc, authpriv, true) && tryauth(ci, name, desc))
                {
                    ci->authkickvictim = victim;
                    ci->authkickreason = newstring(text);
                } 
                break;
            }

            case N_AUTHANS:
            {
                string desc, ans;
                getstring(desc, p, sizeof(desc));
                uint id = (uint)getint(p);
                getstring(ans, p, sizeof(ans));
                answerchallenge(ci, id, ans, desc);
                break;
            }

            case N_PAUSEGAME:
            {
                int val = getint(p);
                if(ci->privilege < (restrictpausegame ? PRIV_ADMIN : PRIV_MASTER) && !ci->local) break;
                pausegame(val > 0, ci);
                break;
            }

            case N_GAMESPEED:
            {
                int val = getint(p);
                if(ci->privilege < (restrictgamespeed ? PRIV_ADMIN : PRIV_MASTER) && !ci->local) break;
                changegamespeed(val, ci);
                break;
            }

            case N_COPY:
                ci->cleanclipboard();
                ci->lastclipboard = totalmillis ? totalmillis : 1;
                goto genericmsg;

            case N_PASTE:
                if(ci->state.state!=CS_SPECTATOR) sendclipboard(ci);
                goto genericmsg;
    
            case N_CLIPBOARD:
            {
                int unpacklen = getint(p), packlen = getint(p); 
                ci->cleanclipboard(false);
                if(ci->state.state==CS_SPECTATOR)
                {
                    if(packlen > 0) p.subbuf(packlen);
                    break;
                }
                if(packlen <= 0 || packlen > (1<<16) || unpacklen <= 0) 
                {
                    if(packlen > 0) p.subbuf(packlen);
                    packlen = unpacklen = 0;
                }
                packetbuf q(32 + packlen, ENET_PACKET_FLAG_RELIABLE);
                putint(q, N_CLIPBOARD);
                putint(q, ci->clientnum);
                putint(q, unpacklen);
                putint(q, packlen); 
                if(packlen > 0) p.get(q.subbuf(packlen).buf, packlen);
                ci->clipboard = q.finalize();
                ci->clipboard->referenceCount++;
                break;
            } 

            case N_SERVCMD:
                getstring(text, p);
                parsecommand(text, ci);
                break;
                     
            #define PARSEMESSAGES 1
            #include "capture.h"
            #include "ctf.h"
            #include "collect.h"
            #undef PARSEMESSAGES

            case -1:
                disconnect_client(sender, DISC_MSGERR);
                return;

            case -2:
                disconnect_client(sender, DISC_OVERFLOW);
                return;

            default: genericmsg:
            {
                int size = server::msgsizelookup(type);
                if(size<=0) { ac(ci, MESSAGESIZE, size); disconnect_client(sender, DISC_MSGERR); return; }
                loopi(size-1) getint(p);
                if(ci && cq && (ci != cq || ci->state.state!=CS_SPECTATOR)) { QUEUE_AI; QUEUE_MSG; }
                break;
            }
        }
    }

    int laninfoport() { return SAUERBRATEN_LANINFO_PORT; }
    int serverinfoport(int servport) { return servport < 0 ? SAUERBRATEN_SERVINFO_PORT : servport+1; }
    int serverport(int infoport) { return infoport < 0 ? SAUERBRATEN_SERVER_PORT : infoport-1; }
    const char *defaultmaster() { return "sauerbraten.org"; }
    int masterport() { return SAUERBRATEN_MASTER_PORT; }
    int numchannels() { return 3; }

    VAR(serverhideip, 0, 1, 1);

    #include "extinfo.h"

    void serverinforeply(ucharbuf &req, ucharbuf &p)
    {
        if(!getint(req))
        {
            extserverinforeply(req, p);
            return;
        }

        putint(p, numclients(-1, false, true));
        putint(p, gamepaused || gamespeed != 100 ? 7 : 5);                   // number of attrs following
        putint(p, PROTOCOL_VERSION);    // generic attributes, passed back below
        putint(p, gamemode);
        putint(p, m_timed ? max((gamelimit - gamemillis)/1000, 0) : 0);
        putint(p, maxclients);
        putint(p, serverpass[0] ? MM_PASSWORD : (!m_mp(gamemode) ? MM_PRIVATE : (mastermode || mastermask&MM_AUTOAPPROVE ? mastermode : MM_AUTH)));
        if(gamepaused || gamespeed != 100)
        {
            putint(p, gamepaused ? 1 : 0);
            putint(p, gamespeed);
        }
        sendstring(smapname, p);
        sendstring(serverdesc, p);
        sendserverinforeply(p);
    }

    bool servercompatible(char *name, char *sdec, char *map, int ping, const vector<int> &attr, int np)
    {
        return attr.length() && attr[0]==PROTOCOL_VERSION;
    }

    #include "aiman.h"
}
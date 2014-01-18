CXXFLAGS= -O3 -fomit-frame-pointer -ffast-math -march=native
override CXXFLAGS+= -Wall -fsigned-char -fno-exceptions -fno-rtti

PLATFORM= $(shell uname -s)
PLATFORM_PREFIX= native

INCLUDES= -Ishared -Iengine -Ifpsgame -Ienet/include

STRIP=
ifeq (,$(findstring -g,$(CXXFLAGS)))
ifeq (,$(findstring -pg,$(CXXFLAGS)))
  STRIP=strip
endif
endif

MV=mv

ifneq (,$(findstring MINGW,$(PLATFORM)))
WINDRES= windres
ifneq (,$(findstring 64,$(PLATFORM)))
ifneq (,$(findstring CROSS,$(PLATFORM)))
  CXX=x86_64-w64-mingw32-g++
  WINDRES=x86_64-w64-mingw32-windres
ifneq (,$(STRIP))
  STRIP=x86_64-w64-mingw32-strip
endif
endif
WINLIB=lib64
WINBIN=../bin64
override CXX+= -m64
override WINDRES+= -F pe-x86-64
else
ifneq (,$(findstring CROSS,$(PLATFORM)))
  CXX=i686-w64-mingw32-g++
  WINDRES=i686-w64-mingw32-windres
ifneq (,$(STRIP))
  STRIP=i686-w64-mingw32-strip
endif
endif
WINLIB=lib
WINBIN=../bin
override CXX+= -m32
override WINDRES+= -F pe-i386
endif
ifneq (,$(findstring TDM,$(PLATFORM)))
STD_LIBS=
else
STD_LIBS= -static-libgcc -static-libstdc++
endif
endif

ifneq (,$(findstring MINGW,$(PLATFORM)))
SERVER_INCLUDES= -DSTANDALONE $(INCLUDES) -Iinclude
SERVER_LIBS= -mwindows $(STD_LIBS) -L$(WINBIN) -L$(WINLIB) -lzlib1 -lenet -lws2_32 -lwinmm
MASTER_LIBS= $(STD_LIBS) -L$(WINBIN) -L$(WINLIB) -lzlib1 -lenet -lws2_32 -lwinmm
else
SERVER_INCLUDES= -DSTANDALONE $(INCLUDES)
SERVER_LIBS= -Lenet -lenet -lz -ldl
MASTER_LIBS= $(SERVER_LIBS)
endif
SERVER_OBJS= \
	shared/crypto-standalone.o \
	shared/stream-standalone.o \
	shared/tools-standalone.o \
	engine/command-standalone.o \
	engine/server-standalone.o \
	engine/worldio-standalone.o \
	fpsgame/entities-standalone.o \
	fpsgame/server-standalone.o

MASTER_OBJS= \
	shared/crypto-standalone.o \
	shared/stream-standalone.o \
	shared/tools-standalone.o \
	engine/command-standalone.o \
	engine/master-standalone.o

ifeq ($(PLATFORM),SunOS)
SERVER_LIBS+= -lsocket -lnsl
endif

default: all

# all: server master
all: server 

clean:
	-$(RM) $(SERVER_OBJS) $(MASTER_OBJS) sauer_server sauer_master

%.h.gch: %.h
	$(CXX) $(CXXFLAGS) -x c++-header -o $(subst .h.gch,.tmp.h.gch,$@) $(subst .h.gch,.h,$@)
	$(MV) $(subst .h.gch,.tmp.h.gch,$@) $@

%-standalone.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $(subst -standalone.o,.cpp,$@)

$(SERVER_OBJS): CXXFLAGS += $(SERVER_INCLUDES)
$(filter-out $(SERVER_OBJS),$(MASTER_OBJS)): CXXFLAGS += $(SERVER_INCLUDES)

ifneq (,$(findstring MINGW,$(PLATFORM)))
server: $(SERVER_OBJS)
	$(WINDRES) -I vcpp -i vcpp/mingw.rc -J rc -o vcpp/mingw.res -O coff
	$(CXX) $(CXXFLAGS) -o $(WINBIN)/sauer_server.exe vcpp/mingw.res $(SERVER_OBJS) $(SERVER_LIBS)

master: $(MASTER_OBJS)
	$(CXX) $(CXXFLAGS) -o $(WINBIN)/sauer_master.exe $(MASTER_OBJS) $(MASTER_LIBS)

install: all
else
server:	libenet $(SERVER_OBJS)
	$(CXX) $(CXXFLAGS) -o sauer_server $(SERVER_OBJS) $(SERVER_LIBS) -rdynamic
	
master: libenet $(MASTER_OBJS)
	$(CXX) $(CXXFLAGS) -o sauer_master $(MASTER_OBJS) $(MASTER_LIBS)  

install: all
	cp sauer_server	../sauer_server
#	cp sauer_master	../sauer_master
ifneq (,$(STRIP))
	$(STRIP) ../sauer_server
#	$(STRIP) ../sauer_master
endif
endif

enet/libenet.a:
	$(MAKE) -C enet
libenet: enet/libenet.a

enet/clean:
	$(MAKE) -C enet
cleanenet: enet/clean

_modules/geolocation:
	$(CC) -g -O2 -Wall -fPIC -c modules/geolocation/main.c -o modules/geolocation/main.o
	@mkdir -p ../modules/
	$(CC) -g -O2 -shared modules/geolocation/main.o -lGeoIP -o ../modules/mod_geolocation.so

modules/geolocation: _modules/geolocation

depend:
	makedepend -a -o-standalone.o -Y -DSTANDALONE -Ishared -Iengine -Ifpsgame $(subst -standalone.o,.cpp,$(SERVER_OBJS))
	makedepend -a -o-standalone.o -Y -DSTANDALONE -Ishared -Iengine -Ifpsgame $(subst -standalone.o,.cpp,$(filter-out $(SERVER_OBJS), $(MASTER_OBJS)))

engine/engine.h.gch: shared/cube.h.gch
fpsgame/game.h.gch: shared/cube.h.gch

# DO NOT DELETE

shared/cube.h.gch: shared/tools.h shared/geom.h shared/ents.h
shared/cube.h.gch: shared/command.h shared/iengine.h shared/igame.h
engine/engine.h.gch: shared/cube.h shared/tools.h shared/geom.h shared/ents.h
engine/engine.h.gch: shared/command.h shared/iengine.h shared/igame.h
engine/engine.h.gch: engine/world.h engine/octa.h engine/lightmap.h
engine/engine.h.gch: engine/bih.h engine/texture.h engine/model.h
engine/engine.h.gch: engine/varray.h
fpsgame/game.h.gch: shared/cube.h shared/tools.h shared/geom.h shared/ents.h
fpsgame/game.h.gch: shared/command.h shared/iengine.h shared/igame.h
fpsgame/game.h.gch: fpsgame/ai.h

shared/crypto-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/crypto-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/crypto-standalone.o: shared/igame.h
shared/stream-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/stream-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/stream-standalone.o: shared/igame.h
shared/tools-standalone.o: shared/cube.h shared/tools.h shared/geom.h
shared/tools-standalone.o: shared/ents.h shared/command.h shared/iengine.h
shared/tools-standalone.o: shared/igame.h
engine/command-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/command-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/command-standalone.o: shared/iengine.h shared/igame.h engine/world.h
engine/server-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/server-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/server-standalone.o: shared/iengine.h shared/igame.h engine/world.h
engine/worldio-standalone.o: engine/engine.h shared/cube.h shared/tools.h
engine/worldio-standalone.o: shared/geom.h shared/ents.h shared/command.h
engine/worldio-standalone.o: shared/iengine.h shared/igame.h engine/world.h
fpsgame/entities-standalone.o: fpsgame/game.h shared/cube.h shared/tools.h
fpsgame/entities-standalone.o: shared/geom.h shared/ents.h shared/command.h
fpsgame/entities-standalone.o: shared/iengine.h shared/igame.h fpsgame/ai.h
fpsgame/server-standalone.o: fpsgame/game.h shared/cube.h shared/tools.h
fpsgame/server-standalone.o: shared/geom.h shared/ents.h shared/command.h
fpsgame/server-standalone.o: shared/iengine.h shared/igame.h fpsgame/ai.h
fpsgame/server-standalone.o: fpsgame/capture.h fpsgame/ctf.h
fpsgame/server-standalone.o: fpsgame/collect.h fpsgame/extinfo.h
fpsgame/server-standalone.o: fpsgame/aiman.h

engine/master-standalone.o: shared/cube.h shared/tools.h shared/geom.h
engine/master-standalone.o: shared/ents.h shared/command.h shared/iengine.h
engine/master-standalone.o: shared/igame.h

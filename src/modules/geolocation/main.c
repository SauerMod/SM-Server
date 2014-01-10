#include "GeoIP.h"
#include "GeoIPCity.h"

typedef void *(* getexternaltype)(char *);
typedef void (* sendservmsgftype)(const char *, ...);

sendservmsgftype sendservmsgf;
getexternaltype  get_external;

char _gidb_co_f_[260] = "GeoIP.dat";
GeoIP *_gidb_co_ = 0; // GeoIP Country DB
char _gidb_ci_f_[260] = "GeoLiteCity.dat";
GeoIP *_gidb_ci_ = 0; // GeoIP City DB
int showcity = 1;
int gicache  = 0;

void mod_func(char *args[16])
{
	if(!_gidb_co_) return;
	const char *clientname = (const char*)args[0];
	const char *ipaddress  = (const char*)args[1];
	const char *country = 0;
	const char *city = 0;
	char message[260];
	country = GeoIP_country_name_by_addr(_gidb_co_, ipaddress);
	if(showcity && _gidb_ci_ && !strstr(country, "Proxy"))
	{
		GeoIPRecord* _gire_ci_ = GeoIP_record_by_addr(_gidb_ci_, ipaddress);
		if(_gire_ci_ && _gire_ci_->city && _gire_ci_->city[0])
		{
			city = _gire_ci_->city;
			snprintf(message, sizeof(message), "\f7Client \fs\f1%s\fr has connected from \fs\f6%s\fr, \fs\f2%s\fr.",
				clientname,
				city,
				country
			);
		}
		else snprintf(message, sizeof(message), "\f7Client \fs\f1%s\fr has connected from \fs\f2%s\fr.",
			clientname,
			country
		);
	}
	else snprintf(message, sizeof(message), "\f7Client \fs\f1%s\fr has connected from \fs\f2%s\fr.",
		clientname,
		country
	);
	sendservmsgf(message);
}

void mod_init(void *getexternal, void *setexternal, char args[16])
{
	/*int i;
	for(i = 0; i < 16; i++)
	{
		if(args[i])
		{
			switch(args[i])
			{
				case 'n':
				{
					showcity = 0;
					break;
				}
				case 'c':
				{
					gicache = 1;
					break;
				}
			}
		}
	}*/ // not needed for now.
	_gidb_co_ = GeoIP_open(_gidb_co_f_, gicache ? GEOIP_MEMORY_CACHE : GEOIP_STANDARD);
	_gidb_ci_ = GeoIP_open(_gidb_ci_f_, gicache ? GEOIP_INDEX_CACHE  : GEOIP_STANDARD);
	get_external = (getexternaltype)getexternal;
	sendservmsgf = (sendservmsgftype)get_external("sendservmsgf");
}

void mod_close()
{
	if(_gidb_co_)
	{
		GeoIP_delete(_gidb_co_);
		_gidb_co_ = 0;
	}
	if(_gidb_ci_)
	{
		GeoIP_delete(_gidb_ci_);
		_gidb_ci_ = 0;
	}
}
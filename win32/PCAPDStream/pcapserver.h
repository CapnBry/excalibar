#define DPACKET_HELO						0x01
#define DPACKET_LOG							0x02
#define DPACKET_CONNECTION_QUERY			0x03
#define DPACKET_CONNECTION_DETAILS			0x04
#define DPACKET_DAOC_CONNECTION_OPENED		0x05
#define DPACKET_DAOC_CONNECTION_CLOSED		0x06
#define DPACKET_DAOC_DATA					0x07
#define DPACKET_SET_PACKETFILTER			0x08
#define DPACKET_AUTH_CREDENTIALS			0x09
#define DPACKET_AUTH_RESPONSE				0x0a
#define DPACKET_SQUELCH						0x0b
#define DPACKET_RESUME						0x0c
#define DPACKET_KEYS						0x0d
#define DPACKET_MOUSE						0x0e

#define WIN32_LEAN_AND_MEAN
#define int_ntoa(x)	inet_ntoa(*((struct in_addr *)&x))
#define VERSIONSTRING		"0.7"
#define CAPTIONSTRING		"PCAP-DStream Server"
#define MAX_PENDING_CONNECTS	4  /* The backlog allowed for listen() */
#define MAX_MSG_LENGTH 1024
#define BUFSIZE 0xffff			   //65535 hoffe es reicht

#include <winsock2.h>
#include <iostream>
#include <Tlhelp32.h>
//#include "PCAP/nids.h"
extern "C" // gotta make extern "C" for a C library!
{
#include "nids.h"
}

#include "resource.h"
#include "Memory.h"
#include "Main.h"
#include "MsgProc.h"
#include "DStream.h"
#include "Sniffer.h"

using namespace Network;

unsigned short GetWord(unsigned char *data);
void TestSegSearch(const char* cmd_line);
void SelectDevice(HINSTANCE hInst);
bool GameRunning(void);
void SET_CHECK_BOOL(HWND hwnd,UINT control,bool bool_value);
bool GET_CHECK_BOOL(HWND hwnd,UINT control);
void SET_EDIT_STRING(HWND hwnd,UINT control,const std::string& std_str);
void GET_EDIT_STRING(HWND hwnd,UINT control,std::string& std_str) ;

DWORD GetPIDByWindowName(const char* name);

extern cMain *pMain;
extern cSniffer *pSniffer;
extern cDStream *pDStream;
extern bool bContinue;
extern unsigned short port;
extern unsigned short daoc_port;

extern std::string account_name;
extern bool euro_server;
extern bool promiscuous;

typedef void* (__cdecl*Aurelia_T)(void);
extern Aurelia_T Aurelia;
typedef void (__cdecl*Brutus_T)(void*);
extern Brutus_T Brutus;
typedef void (__cdecl*trolok45_T)(void*,void*,unsigned int);
extern trolok45_T trolok45;
typedef bool (__cdecl *abfallanfang_T)(void*,void*,const char*,HANDLE);
extern abfallanfang_T abfallanfang;
extern void* p;
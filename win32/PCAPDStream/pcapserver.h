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
#define VERSIONSTRING		"0.1"
#define CAPTIONSTRING		"PCAP-DStream Server"
#define MAX_PENDING_CONNECTS	4  /* The backlog allowed for listen() */
#define MAX_MSG_LENGTH 1024
#define BUFSIZE 0xffff			   //65535 hoffe es reicht

#include <winsock2.h>
#include <stdio.h>
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

extern cMain *pMain;
extern cSniffer *pSniffer;
extern cDStream *pDStream;
extern bool bContinue;
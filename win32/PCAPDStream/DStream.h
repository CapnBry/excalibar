#pragma once
#include "dserver.h"
extern "C" // gotta make extern "C" for a C library!
{
#include "nids.h"
}

namespace Network
{
class cDStreamMsg 
{
public:
	int x01();
	void x05();
	void x06();
	void x07(unsigned char origin, unsigned char protocol, short data_size,const BYTE *data);

	unsigned char msg[4096];
private:
	struct packet_header
	{
		char size[2];
		char code;
	};
	struct x01_packet
	{
		packet_header header;
		char signature[4];
		char version_no[4];
		char authentication_challenge;
	};
	struct x05_packet
	{
		packet_header header;
		char connectionid[4];
		char server_ip[4];
		char server_port[2];
		char client_ip[4];
		char client_port[2];
	};
	struct x06_packet
	{
		packet_header header;
		char connectionid[4];
		char server_ip[4];
		char server_port[2];
		char client_ip[4];
		char client_port[2];
	};
	struct x07_packet
	{
		packet_header header;
		char connectionid[4];
		char origin;
		char protocol;
		char data_size[2];
		char data;
	};
};

class cDStream : public cDStreamMsg
{
public:
	unsigned long connectionid;
	tuple4 addr;

	cDStream(void);	
	cMsgProc MsgProc;

	bool StartDStream();

    inline void SendToClient(const void* data,const size_t num_bytes){server.SendToClients(data,num_bytes);};
    
private:
    DServer server;
}; //end class cDStream

}; //end of namespace
#include "pcapserver.h"

using namespace Network;
 
//x01 - DStream protocol version (DPACKET_HELO)
int cDStreamMsg::x01()
{
	x01_packet *packet = (x01_packet *)&msg;
	unsigned char string[] = { 0x00, 0x20, 0x8C, 0x00, 0xA0, 0x39, 0x8C, 0x00, 0xC8 };
	int size = sizeof(x01_packet)+sizeof(string)-1;
	int version_no = 1;

	memcpy(&packet->header.size,&size,sizeof(short));
	packet->header.code = 0x01;
	memcpy(packet->signature,"DSTM",4);
	memcpy(&packet->version_no,&version_no,sizeof(long));	
	memcpy((void *)&packet->authentication_challenge,string,sizeof(string));

	pDStream->SendToClient(msg,size);
	return size;
}

/*x02 - Log message (DPACKET_LOG)
x03 - Request connection information (DPACKET_CONNECTION_QUERY)
x04 - DAoC connection information (DPACKET_CONNECTION_DETAILS)*/

//x05 - DAoC connection established (DPACKET_DAOC_CONNECTION_OPENED)
void cDStreamMsg::x05()
{
	x05_packet *packet = (x05_packet *)&msg;
	int size = sizeof(x05_packet);
	
	memcpy(&packet->header.size,&size,sizeof(short));
	packet->header.code = 0x05;	
	memcpy(&packet->connectionid,&pDStream->connectionid,sizeof(long));	
	memcpy(&packet->server_ip,&pDStream->addr.daddr,sizeof(long));	
	memcpy(&packet->server_port,&pDStream->addr.dest,sizeof(short));	
	memcpy(&packet->client_ip,&pDStream->addr.saddr,sizeof(long));	
	memcpy(&packet->client_port,&pDStream->addr.source,sizeof(short));	

	pDStream->SendToClient(msg,size);
}

//x06 - DAoC connection terminiated (DPACKET_DAOC_CONNECTION_CLOSED)
void cDStreamMsg::x06()
{
	x06_packet *packet = (x06_packet *)&msg;
	int size = sizeof(x06_packet);
	
	memcpy(&packet->header.size,&size,sizeof(short));
	packet->header.code = 0x06;	
	memcpy(&packet->connectionid,&pDStream->connectionid,sizeof(long));	
	memcpy(&packet->server_ip,&pDStream->addr.daddr,sizeof(long));	
	memcpy(&packet->server_port,&pDStream->addr.dest,sizeof(short));	
	memcpy(&packet->client_ip,&pDStream->addr.saddr,sizeof(long));	
	memcpy(&packet->client_port,&pDStream->addr.source,sizeof(short));	

	pDStream->SendToClient(msg,size);
}

//x07 - DAoC packet data (DPACKET_DAOC_DATA)
void cDStreamMsg::x07(unsigned char origin, 
					  unsigned char protocol, 
					  short data_size, 
					  const BYTE *data)
{
	x07_packet *packet = (x07_packet *)&msg;
	int size = sizeof(x07_packet) + data_size - 1;

	memcpy(&packet->header.size,&size,sizeof(short));
	packet->header.code = 0x07;	
	memcpy(&packet->connectionid,&pDStream->connectionid,sizeof(long));	
	packet->origin = origin;
	packet->protocol = protocol;
	memcpy(&packet->data_size,&data_size,sizeof(short));	
	memcpy((void *)&packet->data,(const void *)(data+2),data_size);

	pDStream->SendToClient(msg,size);
}

/*x08 - Set DAoC packet filter (DPACKET_SET_PACKETFILTER)
x09 - Authentication request (DPACKET_AUTH_CREDENTIALS)
x0a - Authentication response (DPACKET_AUTH_RESPONSE)
x0b - Squelch connection data (DPACKET_SQUELCH)
x0c - Resume connection data (DPACKET_RESUME)
x0d - Send keystrokes (DPACKET_KEYS)
x0e - Send mouse command (DPACKET_MOUSE) */

cDStream::cDStream(void)
{
	pDStream = this;
	connectionid=GetCurrentProcessId();
}

bool cDStream::StartDStream()
{
    DSERVER_PARAMS params;
    params.listen_port=9867;
    params.max_clients=10;
    return(server.Go(params));
} // end StartDStream
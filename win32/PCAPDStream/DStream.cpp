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
	iPort = 9867;
	connectionid=GetCurrentProcessId();
}

void cDStream::ServerThread()
{
	//int status;
	//char szMsg[MAX_MSG_LENGTH];	

	if(!Accept())
	{
		pMain->StatusUpdate("can't activate DStream server: Accept failed (%d)\r\n",WSAGetLastError());
		pDStream->CloseSocket();
		ExitThread(0);
	}

	pMain->StatusUpdate("DStream client connected\r\n");

	x01();	

/*	while(true) 
	{
		status = ReceiveFromClient(szMsg, MAX_MSG_LENGTH);

		if (status == SOCKET_ERROR) 
		{
			CloseSocket();
			ExitThread(0);
		}	 
	}*/
}

void cDStream::ServerThreadProc()
{
	pDStream->ServerThread();
}

bool cDStream::StartDStream()
{
	if(!CreateSocket())
	{
		pMain->StatusUpdate("can't activate DStream server: Create failed (%d)\r\n",WSAGetLastError());
		return false;
	}

	if(!BindSocket(iPort))
	{
		pMain->StatusUpdate("can't activate DStream server: Bind failed (%d)\r\n",WSAGetLastError());
		CloseSocket();
		return false;
	}

	if(!Listen())
	{
		pMain->StatusUpdate("can't activate DStream server: Listen failed (%d)\r\n",WSAGetLastError());
		CloseSocket();
		return false;
	}

	hServerThread = CreateThread(NULL,0,
		 						(LPTHREAD_START_ROUTINE)ServerThreadProc,
								0,0,0);
	if(hServerThread == NULL)
	{
		CloseSocket();
		return false;
	}

	pMain->StatusUpdate("DStream server activated and listening on port %d\r\n",iPort);
	return true;
}

//**** WinSocket Class ***************************************************************
WinSocket::WinSocket()
{	
	WSADATA WSAData;

	WSAStartup(MAKEWORD(1,1), &WSAData);
}

WinSocket::~WinSocket()
{
	WSACleanup();
}

bool WinSocket::Accept()
{
	client = accept(sock, NULL, NULL);
	if((signed)client < 0)
		return false;

	return true;
}

bool WinSocket::Connect()
{
	return connect(sock, (PSOCKADDR) &addr, sizeof(addr)) != SOCKET_ERROR;
}

int WinSocket::Send(const unsigned char* buf, int len)
{
	return send(sock,(const char*)buf,len,0);
}

int WinSocket::SendToClient(const unsigned char* buf, int len)
{
	return send(client,(const char*)buf,len,0);
}


int WinSocket::Receive(char* buf,int len)
{
	return recv(sock,buf,len,0);	
}

int WinSocket::ReceiveFromClient(char* buf,int len)
{
	return recv(client,buf,len,0);	
}

int WinSocket::Send(const char* fmt, ...)
{
	va_list marker;
	va_start(marker, fmt);

	char szBuf[1024*4];
	vsprintf(szBuf, fmt, marker);

	va_end(marker);

	return Send((unsigned char*)szBuf, strlen(szBuf));
}

bool WinSocket::BindSocket(int port)
{
	SOCKADDR_IN local_sin;

	local_sin.sin_family = AF_INET;
	local_sin.sin_addr.s_addr = INADDR_ANY;
	local_sin.sin_port = htons(port); 
	if(bind(sock,(struct sockaddr FAR *) &local_sin, sizeof(local_sin)) == SOCKET_ERROR) 
		return false;

	return true;
}

bool WinSocket::Listen()
{
	if(listen(sock, MAX_PENDING_CONNECTS ) < 0)
		return false;

	return true;
}

bool WinSocket::CreateSocket()
{
	sock = socket(AF_INET, SOCK_STREAM, 0);

	if(sock == INVALID_SOCKET)
		return false;

	return true;
}
bool WinSocket::CloseSocket()
{
	closesocket(sock);
	return true;
}
/*
bool WinSocket::ResolveAddr()
{
	PHOSTENT phe;

	ZeroMemory((void *)&addr,sizeof(addr));

	addr.sin_family = AF_INET;

	phe = gethostbyname(ServerName);
	if(phe == NULL)
		return false;

	memcpy((void *)&addr.sin_addr, phe->h_addr,phe->h_length);	

	addr.sin_port = htons(ServerPort);

	return true;
}*/
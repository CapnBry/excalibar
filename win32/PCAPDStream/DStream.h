namespace Network
{
class WinSocket
{
	public:
	WinSocket();
	~WinSocket();

	bool Listen();
	bool Accept();
	bool CreateSocket();
	bool CloseSocket();
	bool Connect();
	bool BindSocket(int port);
	int Send(const unsigned char* buf, int cbBuf);
	int SendToClient(const unsigned char* buf, int cbBuf);
	int Send(const char* fmt, ...);
	int Receive(char* buf,int len);
	int ReceiveFromClient(char* buf,int len);

	SOCKET sock,client;
	sockaddr_in addr;
}; //end class WinSocket

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

class cDStream : public WinSocket,
				 public cDStreamMsg
{
public:
	HANDLE hServerThread;
	int iPort;
	unsigned long connectionid;
	tuple4 addr;

	cDStream(void);	
	cMsgProc MsgProc;

	bool StartDStream();

	void ServerThread();
	static void ServerThreadProc();
}; //end class cDStream

}; //end of namespace
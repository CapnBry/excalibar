#pragma once
namespace Network
{
class cMsgProc
{
public:
	cMsgProc(void);	
	~cMsgProc(void);	

	bool HandleTCPPacket(char *data,int data_size,bool recv);
	bool HandleUDPPacket(char *data,int data_size,bool recv);
private:
	cMemory Mem;
	int CntPackets;
	bool wait4more;
	char *read_buf;
	unsigned long bufsize;

	bool udp_wait4more;
	char *udp_read_buf;
	unsigned long udp_bufsize;
};
};
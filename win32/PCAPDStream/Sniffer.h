#pragma once

class cSniffer
{
public:
	cSniffer();
	~cSniffer();

	bool sniffer_activ;
	bool connected;
	HANDLE hSnifferThread;
	static void SnifferThreadProc();
	static void tcp_callback(struct tcp_stream *a_tcp, void ** this_time_not_needed);
	static void udp_callback(struct tuple4 * addr, char *data, int len, struct ip * iph);
	bool StartSniffer();	
	bool IsDaocStream(struct tuple4 addr,bool udp);
	bool IsDaocStream(unsigned long network,struct tuple4 addr,bool udp);
};

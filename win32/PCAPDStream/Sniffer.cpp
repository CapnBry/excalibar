#include "pcapserver.h"

cSniffer::cSniffer(void)
{
	pSniffer = this;
	sniffer_activ = false;
	connected = false;
}

bool cSniffer::IsDaocStream(unsigned long network,tuple4 addr,bool udp)
{
	unsigned short port = 10622;

	if((addr.saddr & 0x00ffffff) == network)
	{
		if(udp)return true;
		if(addr.source == port)return true;
	}
			
	if((addr.daddr & 0x00ffffff) == network)
	{
		if(udp)return true;
		if(addr.dest == port)return true;
	}

	return false;
}

bool cSniffer::IsDaocStream(tuple4 addr,bool udp)
{
	unsigned net1 = 0x007bfcc1;		//euro server network	
	unsigned ser1 = 0x0b7bfcc1;
	unsigned net2 = 0x0010fed0;
	unsigned net3 = 0x001ae1d1;

	if(addr.daddr == ser1 || addr.saddr == ser1)
		return false;

	if(IsDaocStream(net1,addr,udp))
		return true;

	if(IsDaocStream(net2,addr,udp))
		return true;

	if(IsDaocStream(net3,addr,udp))
		return true;

	return false;
}

void cSniffer::tcp_callback(tcp_stream *a_tcp, void ** this_time_not_needed)
{	
	switch(a_tcp->nids_state)
	{
		case NIDS_JUST_EST:
		{
			if(pSniffer->connected)
				return;

			//pMain->StatusUpdate("New TCP connection\r\n");
			if(!pSniffer->IsDaocStream(a_tcp->addr,false))
			    {
    			//pMain->StatusUpdate("New TCP connection was not a DAoC connection\r\n");
				return;
				}

			pSniffer->connected = true;

			a_tcp->client.collect++;		// we want data received by a client
			a_tcp->server.collect++;		// and by a server, too
			a_tcp->server.collect_urg++;	// we want urgent data received by a server
			a_tcp->client.collect_urg++;	// if we don't increase this value,
											// we won't be notified of urgent data arrival

			//static 
			char buf[256];
			strcpy(buf, int_ntoa(a_tcp->addr.saddr));
			sprintf(buf + strlen (buf), ",%i --> ", a_tcp->addr.source);
			strcat(buf, int_ntoa (a_tcp->addr.daddr));
			sprintf(buf + strlen (buf), ",%i", a_tcp->addr.dest);
			pMain->StatusUpdate("DAoC connection found: %s\r\n",buf);

			memcpy((void *)&pDStream->addr,(void *)&a_tcp->addr,sizeof(tuple4));

			pDStream->x05();
			return;
		}
		case NIDS_CLOSE:
		{
			pSniffer->connected = false;
			// connection has been closed normally		
			pMain->StatusUpdate("DAoC connection has been closed\r\n");

			pDStream->x06();
			return;
		}
		case NIDS_RESET:
		{
			pSniffer->connected = false;
			// connection has been closed by RST
			pMain->StatusUpdate("DAoC connection has been closed by reset\r\n");

			pDStream->x06();
			return;
		}
		case NIDS_DATA:
		{					
			if(a_tcp->server.count_new)
			{	
				//for server				
				pDStream->MsgProc.HandleTCPPacket(a_tcp->server.data,a_tcp->server.count_new,false);			
			}   		
			else
			{
				//for client			
				pDStream->MsgProc.HandleTCPPacket(a_tcp->client.data,a_tcp->client.count_new,true);	
			}

			return;
		}
	}
	return;
}

void cSniffer::udp_callback(struct tuple4 * addr, char *data, int len, struct ip * iph)
{
	if(!pSniffer->connected)
		return;
	if(!pSniffer->IsDaocStream(*addr,true))
		return;	

    if(addr->saddr == pDStream->addr.saddr)
	{            
		//for server
		pDStream->MsgProc.HandleUDPPacket(data,len,false);
	}       
	else if(addr->saddr == pDStream->addr.daddr)
	{            
		//for client
		pDStream->MsgProc.HandleUDPPacket(data,len,true);	
	}
	else if(addr->daddr == pDStream->addr.saddr)
	{            
		//for client
		pDStream->MsgProc.HandleUDPPacket(data,len,true);	

	}
	else if(addr->daddr == pDStream->addr.daddr)
	{            
		//for server
		pDStream->MsgProc.HandleUDPPacket(data,len,false);
	}

	return;
}


void cSniffer::SnifferThreadProc()
{
	if (!nids_init())
	{
		pMain->StatusUpdate("WinPCAP error: %s\r\n",nids_errbuf);
		return;
	}

	SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_ABOVE_NORMAL);

	nids_register_tcp((void *)tcp_callback);
	nids_register_udp((void *)udp_callback);
	
	pMain->StatusUpdate("WinPCAP and UDP/TCP hook initialized\r\n");

    int time = 0;
    fd_set rset;
    struct timeval tv;

    int fd = nids_getfd();

    while(bContinue)
        {
        tv.tv_sec = 0;
        tv.tv_usec = 500000; // 500ms
        FD_ZERO (&rset);
        FD_SET (fd, &rset);
        // add any other fd we need to take care of
        if (select (fd + 1, &rset, 0, 0, &tv))
            {
            if (FD_ISSET(fd,&rset))  // need to test it if there are other
                {
                // fd in rset
                nids_next();
                /*
                if (!nids_next ())
                    {
                    Logger << "[Sniffer::Run] nids next returned 0\n";
                    }
                */
                }
            }
        } // end forever

	return;
}

bool cSniffer::StartSniffer()
{
	if(sniffer_activ)
		return false;

	hSnifferThread = CreateThread(NULL,0,
								 (LPTHREAD_START_ROUTINE)SnifferThreadProc,
								 0,0,0);

	sniffer_activ = true;

	return true;
}

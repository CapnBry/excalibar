#include "pcapserver.h"

cMsgProc::cMsgProc(void)
{
	CntPackets = 0;

	wait4more = false;
	bufsize = 0;
	read_buf = (char *)VirtualAlloc(NULL,BUFSIZE,MEM_COMMIT,PAGE_READWRITE);	
	ZeroMemory(read_buf,BUFSIZE);


	udp_wait4more = false;
	udp_bufsize = 0;
	udp_read_buf = (char *)VirtualAlloc(NULL,BUFSIZE,MEM_COMMIT,PAGE_READWRITE);	
	ZeroMemory(udp_read_buf,BUFSIZE);
}

cMsgProc::~cMsgProc(void)
{
	VirtualFree(read_buf,0,MEM_RELEASE);
	VirtualFree(udp_read_buf,0,MEM_RELEASE);
}

//Verwaltet/Entschlüsselt die ankommenden Packete
bool cMsgProc::HandleTCPPacket(char *data,int data_size,bool recv)
{
	unsigned int code,delta = 0,size = 0;

//es kann passieren das ein großes packet reinkommt und in der zeit ein ping
//request raus geht, dieses dann ignorieren
	if(wait4more && !recv)
		return 0;
		
//beim dritten packet den key aus dem speicher auslesen
		 if(CntPackets == 2)
	{
		Mem.GetKey();
		CntPackets++;		
		return 0;
	}
//ersten 3 packete ignorieren
	else if(CntPackets < 2)
	{
		CntPackets++;
		return 0;
	}

//hauptschleife um packete auszuwerten
	while(delta < data_size)
	{
		size = 0;
		size = GetWord((unsigned char *)data)+1;

		//´max_msg_size hardcoded		
		if(data_size == 1320 || data_size == 1460)
		{
		//Packet Bündel ist größer wie speicher, darf nicht vorkommen
		//falls doch fehlermeldung ausgeben
			if(bufsize+data_size > BUFSIZE)
				pMain->StatusUpdate("error: bufsize < (%d)\r\n",bufsize+data_size);

			memcpy((void *)(read_buf+bufsize),data,data_size);
			bufsize += data_size;
			wait4more = true;
			return 1;
		}
		//wenn letztes packet vom großen angekommen ist anfügen und auswerten
		if(wait4more)
		{
			if(bufsize+data_size > BUFSIZE)
				pMain->StatusUpdate("error: bufsize < (%d)\r\n",bufsize+data_size);

			memcpy((void *)(read_buf+bufsize),data,data_size);
			data = read_buf;
			size = GetWord((unsigned char *)data)+1;
			wait4more = false;
			data_size += bufsize;
			bufsize = 0;
		}

		//header size hinzufügen
		if(!recv)
			size += 9;
			
		//entschlüsselt das packet
		if(size > 0xffff || size > data_size)
		{
			pMain->StatusUpdate("error: corrupt packet size %d, ignore packet\r\n",size);
			return 0;
		}
		Mem.Decrypt((unsigned char *)(data+2),size);

		//vom server
		if(recv)
		{	
			pDStream->x07(0,0,size,data);
		}
		//vom client
		else
		{
			pDStream->x07(1,0,size,data);
		}
		
		//ein großes packet bündel auseinander nehmen mit delta offset
		delta += size + 2;	
		data += size + 2;
	}

	return false;
}



bool cMsgProc::HandleUDPPacket(char *data,int data_size,bool recv)
{
	unsigned int code,delta = 0,size = 0;

//es kann passieren das ein großes packet reinkommt und in der zeit ein ping
//request raus ge8ht, dieses dann ignorieren
	if(udp_wait4more && !recv)
		return 0;

//hauptschleife um packete auszuwerten
	while(delta < data_size)
	{
		size = 0;
		size = GetWord((unsigned char *)data)+1;

		//max msg size hardcoded, jaja ist lame so	
		if(data_size == 1320 || data_size == 1460)
		{
		//Packet Bündel ist größer wie speicher, darf nicht vorkommen
		//falls do2ch fehlermeldung ausgeben
			if(udp_bufsize+data_size > BUFSIZE)
				pMain->StatusUpdate("error: udp bufsize < (%d)\r\n",udp_bufsize+data_size);

			memcpy((void *)(udp_read_buf+udp_bufsize),data,data_size);
			udp_bufsize += data_size;
			udp_wait4more = true;
			return 1;
		}
		//wenn letztes packet vom großen angekommen ist anfügen und auswerten
		if(udp_wait4more)
		{
			if(udp_bufsize+data_size > BUFSIZE)
				pMain->StatusUpdate("error: udp bufsize < (%d)\r\n",udp_bufsize+data_size);

			memcpy((void *)(udp_read_buf+udp_bufsize),data,data_size);
			data = udp_read_buf;
			size = GetWord((unsigned char *)data)+1;
			udp_wait4more = false;
			data_size += udp_bufsize;
			udp_bufsize = 0;
		}
		//header size hinzufügen
		if(!recv)
			size += 9;
			
		//entschlüsselt das packet
		if(size > 0xffff || size > data_size)
		{
			pMain->StatusUpdate("error: corrupt udp packet size %d, ignore packet\r\n",size);
			return 0;
		}
		Mem.Decrypt((unsigned char *)(data+2),size);

		//vom server
		if(recv)
		{
			pDStream->x07(0,0,size,data);
		}
		//vom client
		else
		{			
			pDStream->x07(1,0,size,data);
		}
		
		//ein großes packet bündel auseinander nehmen mit delta offset
		delta += size + 2;	
		data += size + 2;
	}

	return false;
}
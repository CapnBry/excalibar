/******************************************************************************
PCAPDStream: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the PCAPDStream Developers

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
******************************************************************************/
#pragma once
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include "threads.h" // for base class

// predefine
class DServerClientConnection;

struct DSERVER_PARAMS
{
    DSERVER_PARAMS(){listen_port=0;max_clients=0;};
    USHORT listen_port;
    int max_clients;
}; // end struct DSERVER_PARAMS
class DServer : public Thread<DSERVER_PARAMS>
{
public:
    DServer();
    virtual ~DServer();

    inline bool IsInUse(void)const{return(ServerSock == INVALID_SOCKET ? false:true);};
    void SendToClients(const void* data,const size_t num_bytes);

protected:

private:
    bool Init(void);
    void Close(void);
    virtual DWORD Run(const bool& bContinue);
    bool OnAccept(SOCKET s,const struct sockaddr& addr);
    DServerClientConnection* GetFreeClient(void);

    DServer(const DServer& s); // disallow
    DServer& operator=(const DServer& s); // disallow
    
    SOCKET ServerSock; // the server listen socket
    DServerClientConnection* Clients;
}; // end class DServer

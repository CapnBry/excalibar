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
#include "DServer.h"
#include "DServerClient.h" // client def
#include "pcapserver.h" // for cDStream def

DServer::DServer() :
    ServerSock(INVALID_SOCKET),
    Clients(NULL)
{
    // init winsock
    WSADATA wsa;
    WSAStartup(MAKEWORD(2,0),&wsa);
} // end DServer()

DServer::~DServer()
{
    // close server
    Close();
    
    // disconnect all clients
    delete[] Clients;
    Clients=NULL;
    
    // cleanup winsock
    WSACleanup();
} // end ~DServer

DServerClientConnection* DServer::GetFreeClient(void)
{
    if(Clients==NULL)
        {
        return(NULL);
        }

    for(int i=0;i<GoParam.max_clients;++i)
        {
        if(!Clients[i].IsInUse())
            {
            return(&(Clients[i]));
            }
        }

    // done, didn't find one
    return(NULL);
} // end GetFreeClient

bool DServer::Init(void)
{
    // add clients
    if(Clients==NULL)
        {
        Clients=new DServerClientConnection[GoParam.max_clients];
        }
    
    // create socket
    // use first protocol in stream family -- should be tcp
    SOCKET s=socket(AF_INET,SOCK_STREAM,0);
    
    if(s==INVALID_SOCKET)
        {
        // ugh
        return(false);
        }

    // bind to any local addr and the server port
    SOCKADDR_IN addr;
    addr.sin_family=AF_INET;
    addr.sin_port=htons(GoParam.listen_port);
    addr.sin_addr.S_un.S_addr=INADDR_ANY;
    int err;

    err=bind(s,(const struct sockaddr *)&addr,sizeof(addr));

    if(err == SOCKET_ERROR)
        {
        closesocket(s);
        return(false);
        }
       
    // listen
    err=listen(s,5);
    
    if(err == SOCKET_ERROR)
        {
        closesocket(s);
        return(false);
        }
    
    // save
    ServerSock=s;
    
    pMain->StatusUpdate("DStream server initialized and listening on port %d\r\n",GoParam.listen_port);
    
    // done
    return(true);
} // end Init

void DServer::Close(void)
{
    if(!IsInUse())
        {
        // nothing to do
        return;
        }
        
    // close down send side
    shutdown(ServerSock,SD_SEND);

    // close down receive side
    shutdown(ServerSock,SD_RECEIVE);

    // close socket
    closesocket(ServerSock);

    // mark invalid
    ServerSock=INVALID_SOCKET;

    // done
    return;
} // end Close

DWORD DServer::Run(const bool& bContinue)
{
    if(!Init())
        {
        // woops!
        return(-1);
        }
    
    struct sockaddr addr;
    int addrlen=sizeof(addr);
    fd_set readfds;
    struct timeval timeout;
    timeout.tv_sec=1;
    timeout.tv_usec=0;

    while(bContinue)
        {
        FD_ZERO(&readfds);
        FD_SET(ServerSock,&readfds);
        
        // wait for client connection
        int err=select(1,&readfds,NULL,NULL,&timeout);
        switch(err)
            {
            case 0:
                // no error, and no new connections
                break;
            
            case SOCKET_ERROR:
                // uh-oh
                return(-1);
                break;

            default:
                // have new client, process it
                SOCKET new_client=accept(ServerSock,&addr,&addrlen);
                if(new_client!=INVALID_SOCKET)
                    {
                    OnAccept(new_client,addr);
                    }
                else
                    {
                    std::cerr << "select returned >0 but accept returned " << new_client << "\n";
                    }
                break;
            } // end switch on select() status
        } // end forever
    
    // done
    return(0);
} // end Run

bool DServer::OnAccept(SOCKET s,const struct sockaddr& addr)
{
    DServerClientConnection* client=GetFreeClient();
    
    if(client!=NULL)
        {
        pMain->StatusUpdate("New DStream client connected\r\n");
        
        // connect client
        DSERVER_CLIENT_PARAMS param;
        param.s=s;
        param.addr=addr;
        client->Go(param);
        pDStream->x01();
        return(true);
        }
    else
        {
        // close down send side
        shutdown(s,SD_SEND);
        // close down receive side
        shutdown(s,SD_RECEIVE);
        // close socket
        closesocket(s);        
        // woops
        return(false);
        } // end else woops
} // end OnAccept

void DServer::SendToClients(const void* data,const size_t num_bytes)
{
    for(int i=0;i<GoParam.max_clients;++i)
        {
        if(Clients[i].IsInUse())
            {
            // send
            Clients[i].SendData(data,num_bytes);
            } // end if client is active
        } // end for all clients
} // end SendToClients
/******************************************************************************
Cheyenne: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the Cheyenne Developers

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

#include <sstream> // for string stream defs
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include "..\Utils\threads.h" // for thread base class
#include "..\Sharenet\SharenetAPI.h" // for api class

template<class API_IMPL> class MiniShareServer : public Thread
{
public:
    MiniShareServer(const unsigned short server_port=10001) : 
        ServerPort(server_port)
    {
        Client=new API_IMPL;
        
        // init to invalid
        ServerSock=INVALID_SOCKET;
        
        // startup winsock
        WSADATA wsa;
        WSAStartup(MAKEWORD(2,0),&wsa);
    } // end MiniShareServer

    virtual ~MiniShareServer()
    {
        // stop client
        Client->Stop();
        
        // delete client
        delete Client;

        // close server
        CloseServer();
        
        // cleanup winsock
        WSACleanup();
    } // end ~MiniShareServer
        
    bool IsInUse(void)const{return(ServerSock!=INVALID_SOCKET);};
    bool IsClientInUse(void)const
    {
        return(Client->IsInUse());
    }
    
    std::string GetStatusString(void)const
    {
        if(IsInUse())
            {
            std::ostringstream ss;
            ss << "listening on port " << ServerPort;
            return(ss.str());
            }
        else
            {
            return(Client->GetStatusString());
            }
    } // end GetStatusString
    bool QueueOutputMessage(const void* data,const unsigned int length)
    {
        return(Client->QueueOutputMessage(data,length));
    }

protected:
private:
    bool InitServer(void)
    {
        if(IsInUse())
            {
            //::Logger << "[MiniShareServer::Init] Already initialized!\n";
            return(true);
            }
         
        // create socket
        // use first protocol in stream family -- should be tcp
        SOCKET s=socket(AF_INET,SOCK_STREAM,0);
        
        if(s==INVALID_SOCKET)
            {
            // ugh
            //::Logger << "[MiniShareServer::Init] unable to create socket!\n";
            return(false);
            }

        // bind to any local addr and the server port
        SOCKADDR_IN addr;
        addr.sin_family=AF_INET;
        addr.sin_port=htons(ServerPort);
        addr.sin_addr.S_un.S_addr=INADDR_ANY;
        int err;

        err=bind(s,(const struct sockaddr *)&addr,sizeof(addr));

        if(err == SOCKET_ERROR)
            {
            closesocket(s);
            //::Logger << "[MiniShareServer::Init] unable to bind to local address!\n";
            return(false);
            }
           
        // listen
        err=listen(s,5);
        
        if(err == SOCKET_ERROR)
            {
            closesocket(s);
            //::Logger << "[MiniShareServer::Init] unable to listen to local address!\n";
            return(false);
            }
        
        // set non-blocking
        u_long argp=1;
        ioctlsocket(s,FIONBIO,&argp);
        
        //::Logger << "[MiniShareServer::Init] listening on " << addr << std::endl;
        
        // save
        ServerSock=s;
        
        // done
        return(true);
    } // end InitServer
    void CloseServer(void)
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
        
        // set to invalid
        ServerSock=INVALID_SOCKET;
    } // end CloseServer
    
    virtual DWORD Run(const bool& bContinue) // thread API virtual override
    {
        //::Logger << "[MiniShareServer::Run] starting in thread id " << GetCurrentThreadId() << std::endl;
        
        // start client with our context parameter
        Client->Go(GoParam);
        
        bool bHaveClient=false;
        
        while(bContinue)
            {
            if(!bHaveClient)
                {
                // init server
                InitServer();
                while(bContinue)
                    {
                    struct sockaddr_in addr;
                    int addr_sz=sizeof(addr);
                    SOCKET s=accept(ServerSock,(sockaddr*)&addr,&addr_sz);

                    // see if we got a connection
                    if(s != INVALID_SOCKET)
                        {
                        // set blocking
                        u_long argp=0;
                        ioctlsocket(s,FIONBIO,&argp);

                        // we have a new socket
                        //::Logger << "[MiniShareServer::Run] new connection on " << addr << "\n";
                        
                        // open connection
                        bHaveClient=Client->Open(s);
                        
                        // close server
                        CloseServer();

                        // break out of server loop
                        break;
                        } // end if we accepted a connection
                    else
                        {
                        Sleep(100);
                        } // end else we did not accept a connection
                    } // end server loop forever
                } // end if not have client
            else
                {
                while(bContinue && Client->IsInUse())
                    {
                    Sleep(100);
                    } // end client loop forever
                // clear flag
                bHaveClient=false;
                } // end else have client
            } // end forever

        //::Logger << "[MiniShareServer::Run] cleaning up...\n";

        // make sure these are stopped
        CloseServer();
        Client->Stop();
        
        //::Logger << "[MiniShareServer::Run] exiting!\n";

        // done
        return(0);
    } // end Run
    
    MiniShareServer(const MiniShareServer& s);// disallow
    MiniShareServer& operator=(const MiniShareServer& s); // disallow

    SOCKET ServerSock;
    const unsigned short ServerPort;
    
    SharenetAPI* Client;
}; // end class MiniShareServer


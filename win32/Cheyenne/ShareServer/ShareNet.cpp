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
#include "global.h"
#include "sharenet.h"
#include <list>

class AlertableWaitSingleFunctor
{
public:
    typedef DWORD return_type;
    DWORD operator()(HANDLE hEvent,unsigned int timeout_ms) const
    {
        return(WaitForSingleObjectEx(hEvent,timeout_ms,TRUE));
    }
}; // end class WaitSingleFunctor

ShareNetManager::ShareNetManager() :
    ServerPort(10001),
    ServerSock(INVALID_SOCKET)
{
    WSADATA wsa;
    WSAStartup(MAKEWORD(2,0),&wsa);
    
} // end ShareNetManager
ShareNetManager::~ShareNetManager()
{
    Close();
    
    for(int i=0;i<max_clients;++i)
        {
        Logger << "[ShareNetManager::~ShareNetManager] waiting on stop for client["<<i<<"]" << std::endl;
        // stop all clients
        Clients[i].Stop();
        }

    WSACleanup();
} // end ~ShareNetManager

void ShareNetManager::Close(void)
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

bool ShareNetManager::Init(void)
{
    // create socket
    // use first protocol in stream family -- should be tcp
    SOCKET s=socket(AF_INET,SOCK_STREAM,0);
    
    if(s==INVALID_SOCKET)
        {
        // ugh
        Logger << "[ShareNetManager::Init] unable to create socket!\n";
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
        Logger << "[ShareNetManager::Init] unable to bind to local address!\n";
        return(false);
        }
       
    // listen
    err=listen(s,5);
    
    if(err == SOCKET_ERROR)
        {
        closesocket(s);
        Logger << "[ShareNetManager::Init] unable to listen to local address!\n";
        return(false);
        }
    
    // set non-blocking
    u_long argp=1;
    ioctlsocket(s,FIONBIO,&argp);
    
    Logger << "[ShareNetManager::Init] listening on " << addr << std::endl;
    
    // save
    ServerSock=s;
    
    // done
    return(true);
} // end Init

DWORD ShareNetManager::Run(const bool& bContinue)
{
    if(!Init())
        {
        Logger << "[ShareNetManager::Run] failed to initialize!\n";
        return(-1);
        }
       
    // storage for signals
    MultiWaitSignal signals;
    
    // start clients and save signals
    for(int i=0;i<max_clients;++i)
        {
        Clients[i].Go();
        signals.AddSignalToList(Clients[i].GetInputBuffer().GetSignalReference());
        }
    
    while(bContinue)
        {
        struct sockaddr_in addr;
        int addr_sz=sizeof(addr);
        SOCKET s=accept(ServerSock,(sockaddr*)&addr,&addr_sz);
        
        if(s != INVALID_SOCKET)
            {
            // set blocking
            u_long argp=0;
            ioctlsocket(s,FIONBIO,&argp);

            // we have a new socket
            Logger << "[ShareNetManager::Run] new connection on " << addr << std::endl;
            
            for(int i=0;i<max_clients;++i)
                {
                // find client that is free -- this is plenty fast as long
                // as max_clients is fairly small
                if(!Clients[i].IsInUse())
                    {
                    // start a client connection with the new socket
                    Clients[i].Open(s);
                    break;
                    }
                }
            if(i==max_clients)
                {
                // we did not find a free client
                Logger << "[ShareNetManager::Run] unable to find a free client to handle the connection, closing it.\n";
                closesocket(s);
                }
            } // end if new connection
            
        //Logger << "[ShareNetManager::Run] maintaining clients\n";
        MaintainClients(signals);
        } // end forever

    // asynchronous stop clients, we don't need to wait on their exiting
    for(int i=0;i<max_clients;++i)
        {
        Clients[i].AsynchronousStop();
        }

    return(0);
} // end Run

bool ShareNetManager::MaintainClients(MultiWaitSignal& signals)
{
    MultiWaitSignal::wait_any_result_type wait_result;
    
    // wait
    wait_result=signals.WaitAny(1000);
    
    // see if we got anything
    if(wait_result.first != NULL)
        {
        // at least 1 is set, and process it
        //Logger << "[ShareNetManager::MaintainClients] got data from Clients[" << wait_result.second << "]\n";
        
        // see how big it needs to be
        unsigned short sz=0;
        unsigned char data[TempBufferSize];
        
        // lock client's buffer
        Clients[wait_result.second].ModifyInputBuffer().Lock();
        
        // peek for size
        if(Clients[wait_result.second].ModifyInputBuffer().Peek(&sz,sizeof(sz)))
            {
            // extract complete message: sz is the size of the message + sizeof(unsigned short)
            if(Clients[wait_result.second].ModifyInputBuffer().Extract(&data[0],sz))
                {
                if(sz==sizeof(sz))
                    {
                    // this is a zero-length "ping", update the receiver with that
                    // info and do NOT pass it along to the other clients
                    Clients[wait_result.second].SavePingTime();
                    }
                else
                    {
                    for(int i=0;i<max_clients;++i)
                        {
                        // make sure we dont send back to the originator and 
                        // that we only send data to connected clients
                        if(i!=wait_result.second && Clients[i].IsInUse())
                            {
                            /*
                            Logger << "[ShareNetManager::MaintainClients] replicating "
                                    << sz 
                                    << " bytes to Clients[" << i << "]\n";
                            */
                            
                            // send data to client
                            Clients[i].QueueOutputMessage(&data[0],sz);
                            } // end echo check
                        } // end for all signals
                    //data[sz]='\0';
                    //Logger << &data[2] << std::endl;
                    } // end else not a ping
                } // end if we have an entire message available
            else
                {
                // reset the data signal if we need more
                // data. The signal will be set again when more data
                // arrives
                Clients[wait_result.second].ModifyInputBuffer().GetSignalReference().reset();
                } // end else we don't have an entire message available
            } // end if we got a size
        
        // unlock buffer
        Clients[wait_result.second].ModifyInputBuffer().Unlock();
        return(true);
        } // end if at least 1 signal was set
    else
        {
        // return false to indicate that there was nothing to do
        return(false);
        }
} // end MaintainClients

VOID CALLBACK ShareNetClientData::ReadCompletionRoutine
    (
    DWORD dwErrorCode,
    DWORD dwNumberOfBytesTransfered,
    LPOVERLAPPED lpOverlapped
    )
{
    // extract original info: Having to do this
    // is HIDEOUS, but thats the way Microsoft wrote their
    // completion routines: no easy way to store a decent
    // "context" parameter.
    ShareNetClientData* pMe=(ShareNetClientData*)lpOverlapped->hEvent;
    
    //Logger << "[ShareNetClientData::ReadCompletionRoutine] size=" << dwNumberOfBytesTransfered << std::endl;
    
    // if we get here and there's no data, assume something bad happened
    if(dwNumberOfBytesTransfered == 0)
        {
        Logger << "[ShareNetClientData::ReadCompletionRoutine] closing!" << std::endl;
        pMe->Close();
        return;
        }
    
    //Logger << "[ShareNetClientData::ReadCompletionRoutine] got " 
              //<< dwNumberOfBytesTransfered << " bytes" << std::endl;
    // we got some data,
    // maintain the statistics and put it on the input buffer
    pMe->IntervalBytes += dwNumberOfBytesTransfered;
    pMe->ModifyInputBuffer().Insert(&(pMe->ReadBuf[0]),dwNumberOfBytesTransfered);
    
    // see if we are still running
    if(pMe->IsInUse())
        {
        // start another read
        pMe->DoInputMaintenance();
        }

    // done
    return;
} // end ShareNetClientDataReadCompletionRoutine

bool ShareNetClientData::DoOutputMaintenance(void)
{
    // send in 2k chunks
    char buf[TempBufferSize];
    
    unsigned int size=min(sizeof(buf),GetOutputBuffer().Size());

    // copy the data to temp buffer
    ModifyOutputBuffer().Peek(&buf[0],size);

    // send
    unsigned int err=send(GetSocket(),&buf[0],size,0);

    if(err!=0 && err!=SOCKET_ERROR)
        {
        // we sent at least something,
        // take it off the output queue
        ModifyOutputBuffer().Extract(&buf[0],err);
        
        // update statistics
        IntervalBytes+=err;
        }
    
    // done, return true to indicate that there is 
    // something else to do, false to indicate
    // that the output queue is empty
    return(GetOutputBuffer().Size() != 0);
}

bool ShareNetClientData::DoInputMaintenance(void)
{
    // grab in 2k chunks
    
    ZeroMemory(&ReadOverlapped,sizeof(ReadOverlapped));
    // store original info: Having to do this
    // is HIDEOUS, but thats the way Microsoft wrote their
    // completion routines: no easy way to store a decent
    // "context" parameter.
    ReadOverlapped.hEvent=(HANDLE)this;
    SetLastError(ERROR_SUCCESS);
    BOOL status=ReadFileEx
        (
        HANDLE(GetSocket()),
        &ReadBuf[0],
        sizeof(ReadBuf),
        &ReadOverlapped,
        ShareNetClientData::ReadCompletionRoutine
        );

    if(status)
        {
        // done: read in progress
        //Logger << "[ShareNetClientData::DoInputMaintenance] read started with status " << GetLastError() << std::endl;
        return(true);
        }
    
    // close the socket
    Logger << "[ShareNetClientData::DoInputMaintenance] read failed to start with status " << GetLastError() << std::endl;
    Close();
    
    // done
    return(false);
}

DWORD ShareNetClientData::Run(const bool& bContinue)
{
    // init to now
    SetLastPingTime(::Clock.Current());
    
    while(bContinue)
        {
        while(!IsInUse() && bContinue)
            {
            Sleep(1000);
            }
        
        if(!bContinue)
            {
            break;
            }
            
        // start read
        DoInputMaintenance();
        
        while(IsInUse() && bContinue)
            {
            // wait for output buffer to have data in it
            switch(GetOutputBuffer().Wait(AlertableWaitSingleFunctor(),1000))
                {
                case WAIT_OBJECT_0:
                    // there is data to be sent
                    DoOutputMaintenance();
                    break;
                    
                case WAIT_IO_COMPLETION:
                    //Logger << "[ShareNetClientData::Run] I/O Completion!" << std::endl;
                case WAIT_TIMEOUT:
                default:
                    //Logger << "[ShareNetClientData::Run] relooping..." << std::endl;
                    break;
                } // end switch wait result
            
            // check for ping time
            const CheyenneTime CurrentTime(Clock.Current());
            if(CurrentTime - GetLastPingTime() > MaxPingAge)
                {
                // we must receive a "ping" (NULL message) every MaxPingAge seconds:
                // we have not received that, so we close
                Logger << "[ShareNetClientData::Run] ping timeout, closing!\n";
                Close();
                }
            
            if(CurrentTime - GetLastPingSendTime() > PingInterval)
                {
                // we must send a "ping" (NULL message) every PingInterval seconds:
                // it is now time to send that
                SetLastPingSendTime(CurrentTime);
                SendPing();
                }
            } // end while IsInUse()            
        } // end forever
    
    // done
    return(0);
} // end ShareNetClientData::Run
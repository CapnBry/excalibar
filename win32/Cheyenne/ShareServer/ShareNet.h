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

#include "threads.h"
#include "..\Utils\locks.h"
#include "buffer.h"

const int max_clients=20;
const CheyenneTime MaxPingAge(30.0);
const CheyenneTime PingInterval(20.0);
const unsigned int TempBufferSize(2048);

class ShareNetClientData : public Thread
{
friend class ShareNetManager;
public:
    ShareNetClientData()
    {
        SetSocket(INVALID_SOCKET);
        IntervalBytes=0;
    }

    ~ShareNetClientData()
    {
        Close();
    }

    bool IsInUse(void)const
    {
        return(GetSocket() == INVALID_SOCKET ? false:true);
    }

    int GetLastInputError(void)const{return(GetInputBuffer().GetLastError());};
    int GetLastOutputError(void)const{return(GetOutputBuffer().GetLastError());};
    bool Open(SOCKET s)
    {
        if(IsInUse())
            {
            // hmph.
            return(false);
            }

        AutoLock al(MyLock);
        
        // flush buffers
        ModifyInputBuffer().Flush();
        ModifyOutputBuffer().Flush();

        int sz=sizeof(SOCKADDR_IN);

        // save local
        ModifyLocalAddr().sin_family=AF_INET;
        getsockname(s,(struct sockaddr *)&ModifyLocalAddr(),&sz);

        // save remote
        ModifyRemoteAddr().sin_family=AF_INET;
        getpeername(s,(struct sockaddr *)&ModifyRemoteAddr(),&sz);
        
        // make sure remote connection is "legal" -- it 
        // must be in the allowed host list!
        if(Config.IsHostAllowed(GetRemoteAddr()))
            {
            // save socket
            SetSocket(s);

            // save ping time
            SetLastPingTime(Clock.Current());
            SetLastPingSendTime(Clock.Current());

            // done: success
            return(true);
            }
        else
            {
            // remote address is not allowed
            // close socket and return false
            closesocket(s);
            Logger << "[ShareNetClientData::Open] address \""
                   << GetRemoteAddr().sin_addr << "\""
                   << " is not in the allowed hosts list!\n";
            return(false);
            }
    } // end Open
    void Close(void)
    {
        if(!IsInUse())
            {
            return;
            }

        AutoLock al(MyLock);

        // close down send side
        shutdown(GetSocket(),SD_SEND);

        char temp[1024];
        int cnt=0;
        int status=recv(GetSocket(),temp,sizeof(temp),0);

        while(status!=0 && status != SOCKET_ERROR && cnt<10)
            {
            status=recv(GetSocket(),temp,sizeof(temp),0);
            ++cnt;
            }

        // close down receive side
        shutdown(GetSocket(),SD_RECEIVE);

        // close socket
        closesocket(GetSocket());

        // mark invalid
        SetSocket(INVALID_SOCKET);

        // flush buffers
        ModifyInputBuffer().Flush();
        ModifyOutputBuffer().Flush();
    } // end Close

    bool QueueOutputMessage(const void* data,const unsigned int length)
    {
        // this function can block the calling thread!
        
        if(!IsInUse())
            {
            return(false);
            }
        
        // the buffer provides its own thread safety
        if(ModifyOutputBuffer().Insert(data,length))
            {
            // attempt to send immediately
            //DoOutputMaintenance();
            return(true);
            }
        else
            {
            return(false);
            }
    } // end QueueOutputMessage

    unsigned int ExtractInputMessage(void* data,const unsigned int max_len)
    {
        // in case this function is called 
        // twice from 2 threads, we need to
        // lock ourselves

        AutoLock al(MyLock);

        unsigned short short_len;

        if(ModifyInputBuffer().Peek(&short_len,sizeof(short_len)))
            {
            // ok, we have a size, now check to see if we have
            // all the data too, and that it will fit
            // in the supplied buffer

            // resize to unsigned int
            unsigned int len=short_len;

            if(GetInputBuffer().Size() >= len && max_len >= len)
                {
                // we have enough data to build a message
                // so extract it -- this call is guaranteed
                // to succeed

                ModifyInputBuffer().Extract(data,len);

                return(len);
                } // end if we have at least a whole message and it will fit
            } // end if msg size available on buffer

        // return failure (no data available or not enough space)
        return(0);
    } // end ExtractInputMessage

    DWORD GetAndResetIntervalBytes(void)const
    {
        DWORD bytes=IntervalBytes;
        IntervalBytes=0;
        return(bytes);
    }
    
protected:
private:
    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        );

    bool DoOutputMaintenance(void);
    bool DoInputMaintenance(void);
    
    // this one takes "C" arguments to make it simple :/
    // all parameters are in HOST order
    bool Open(const char* local_addr,unsigned short local_port,const char* remote_addr,unsigned short remote_port)
    {
        if(IsInUse())
            {
            return(false);
            }

        AutoLock al(MyLock);

        // prep addresses: local first
        ModifyLocalAddr().sin_family=AF_INET;
        ModifyLocalAddr().sin_port=htons(local_port);
        
        unsigned long ul_addr=inet_addr(local_addr);
        HOSTENT* host;

        if(ul_addr==INADDR_NONE)
            {
            // no suitable address found, try looking it up
            host=gethostbyname(local_addr);

            if(host)
                {
                // take first one in the list
                ul_addr=*((unsigned long*)host->h_addr_list[0]);
                }
            else
                {
                // just use INADDR_ANY
                ul_addr=INADDR_ANY;
                }
            }

        // set in structure
        ModifyLocalAddr().sin_addr.S_un.S_addr=ul_addr;

        // prep addresses: remote next
        ModifyRemoteAddr().sin_family=AF_INET;
        ModifyRemoteAddr().sin_port=htons(remote_port);
        
        ul_addr=inet_addr(remote_addr);

        if(ul_addr==INADDR_NONE)
            {
            // no suitable address found, try looking it up
            host=gethostbyname(remote_addr);

            if(host)
                {
                // take first one in the list
                ul_addr=*((unsigned long*)host->h_addr_list[0]);
                }
            else
                {
                // must have a valid remote address!
                Logger << "[ShareNetClientData::Open] unable to find remote address!\n";
                return(false);
                }
            }

        // set in structure
        ModifyRemoteAddr().sin_addr.S_un.S_addr=ul_addr;

        // create socket
        // use first protocol in stream family -- should be tcp
        SOCKET s = socket(AF_INET,SOCK_STREAM,0);

        if(s==INVALID_SOCKET)
            {
            // ugh
            return(false);
            }

        int err;

        // bind to local
        err=bind(s,(const struct sockaddr *)&GetLocalAddr(),sizeof(SOCKADDR_IN));

        if(err == SOCKET_ERROR)
            {
            closesocket(s);
            Logger << "[ShareNetClientData::Open] unable to bind to local address!\n";
            return(false);
            }

        // connect
        err=connect(s,(const struct sockaddr *)&GetRemoteAddr(),sizeof(SOCKADDR_IN));

        if(err==SOCKET_ERROR)
            {
            closesocket(s);
            Logger << "[ShareNetClientData::Open] unable to bind to local address!\n";
            return(false);
            }

        // save socket
        SetSocket(s);
    
        // save ping time
        SetLastPingTime(Clock.Current());
        SetLastPingSendTime(Clock.Current());
        
        // done!
        return(true);
    } // end Open

    virtual DWORD Run(const bool& bContinue);
    void SavePingTime(void)
    {
        SetLastPingTime(Clock.Current());
    } // end SavePingTime
    void SendPing(void)
    {
        unsigned short sz;
        sz=sizeof(sz);
        QueueOutputMessage(&sz,sz);
    } // end SendPing

    ShareNetClientData(const ShareNetClientData& s); // disallow
    ShareNetClientData& operator=(const ShareNetClientData& s); // disallow

    DECL_MEMBER(SOCKET,Socket);
    DECL_MEMBER(SOCKADDR_IN,LocalAddr);
    DECL_MEMBER(SOCKADDR_IN,RemoteAddr);
    DECL_MEMBER(buffer_space::Buffer,InputBuffer);
    DECL_MEMBER(buffer_space::Buffer,OutputBuffer);
    DECL_MEMBER(CheyenneTime,LastPingTime);
    DECL_MEMBER(CheyenneTime,LastPingSendTime);

    mutable MutexLock MyLock;
    
    // statistics
    mutable DWORD IntervalBytes; // mutable because it is conceptually const
    
    // read data
    OVERLAPPED ReadOverlapped;
    char ReadBuf[TempBufferSize]; // only accessed from the DoInputMaintenance and read completion
                                  // functions, but must be a non-static class member

}; // end class ShareNetClientData

class ShareNetManager : public Thread
{
public:
    ShareNetManager();
    virtual ~ShareNetManager();

    bool IsInUse(void)const
    {
        return(ServerSock == INVALID_SOCKET ? false:true);
    }

    template<typename FUNC> void EnumerateConnectedClients(FUNC func)const
    {
        for(int i=0;i<max_clients;++i)
            {
            if(Clients[i].IsInUse())
                {
                func(Clients[i]);
                }
            }
    }

    inline void SetServerPort(unsigned short sp)
    {
        if(!IsInUse())
            {
            ServerPort=sp;
            }
    }
protected:
private:
    virtual DWORD Run(const bool& bContinue);
    bool Init(void);
    void Close(void);
    bool MaintainClients(MultiWaitSignal& signals);

    SOCKET ServerSock;
    unsigned short ServerPort;
    ShareNetClientData Clients[max_clients];
};

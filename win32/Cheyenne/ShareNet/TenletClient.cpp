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
#include "telnetclient.h"
#include <sstream>
#include "..\Utils\Times.h"
#include "..\Utils\Logger.h"

// reference a clock & logger (this must be instantiated in 
// a translation unit somewhere)
extern CheyenneClock Clock;
extern logger_t Logger;

class AlertableWaitSingleFunctor
{
public:
    typedef DWORD return_type;
    DWORD operator()(HANDLE hEvent,unsigned int timeout_ms) const
    {
        return(WaitForSingleObjectEx(hEvent,timeout_ms,TRUE));
    }
}; // end class WaitSingleFunctor

VOID CALLBACK TelnetClientData::ReadCompletionRoutine
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
    TelnetClientData* pMe=(TelnetClientData*)lpOverlapped->hEvent;
    
    //::Logger << "[ShareNetClientData::ReadCompletionRoutine] got " << dwNumberOfBytesTransfered << " bytes\n";
    
    // if we get here and there's no data, assume something bad happened
    if(dwNumberOfBytesTransfered == 0)
        {
        Logger << "[TelnetClientData::ReadCompletionRoutine] closing! " << WSAGetLastError() << "\n";
        pMe->Close();
        return;
        }
    
    // we got some data,
    // put it on the input buffer
    pMe->ModifyInputBuffer().Insert(&(pMe->ReadBuf[0]),dwNumberOfBytesTransfered);
    
    // telnet connection: we echo back what we get
    //pMe->QueueOutputMessage(&(pMe->ReadBuf[0]),(unsigned short)dwNumberOfBytesTransfered);
    
    // see if we are still running
    if(pMe->IsInUse())
        {
        // start another read
        pMe->DoInputMaintenance();
        }

    // done
    return;
} // end TelnetClientData::ReadCompletionRoutine

bool TelnetClientData::DoOutputMaintenance(void)
{
    // send in 2k chunks
    char buf[2048];
    
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
        }
    
    //::Logger << "[ShareNetClientData::DoOutputMaintenance] sent " << err << " bytes\n";
    
    // done, return true to indicate that there is 
    // something else to do, false to indicate
    // that the output queue is empty
    return(GetOutputBuffer().Size() != 0);
}

bool TelnetClientData::DoInputMaintenance(void)
{
    // lock input buffer -- this is to prevent us from looping
    // in the while loop if we receive a partial message: 
    // the while loop resets the data signal if it needs more
    // data. The signal will be set again when more data
    // arrives
    ModifyInputBuffer().Lock();
    
    // build messages from the input buffer
    unsigned char data[TempBufferSize];
    
    const char telnet_cr=10;
    const char telnet_lf=13;
    unsigned int sz=min(TempBufferSize-1,GetInputBuffer().Size());
    
    while(ModifyInputBuffer().Peek(&data[0],sz))
        {
        // find CR/LF
        for(unsigned int count=0;count<sz;++count)
            {
            if(data[count] == telnet_cr || data[count] == telnet_lf)
                {
                // count is cr or lf, stop here
                break;
                }
            } // end for all characters
        
        if(count != sz)
            {
            // remove the data from the buffer
            ModifyInputBuffer().Extract(&data[0],sz);
            
            // zero out cr/lf
            data[count]='\0';
            data[count+1]='\0'; // TempBufferSize-1 takes care of buffer overflow ;)
            
            std::stringstream ss;
            unsigned int stream_count=0;
            while(stream_count < count)
                {
                if(data[stream_count] != 8)
                    {
                    // not backspace, add to stream
                    ss << data[stream_count];
                    }
                else
                    {
                    // backspace, remove last char
                    ss.seekp(-1,std::ios::cur);
                    } // end else backspace
                    
                // go to next
                ++stream_count;
                } // end while chars are left to extract
            
            // make string
            std::string* msg=new std::string(ss.str());
            
            // check for quit command
            if(*msg=="quit")
                {
                delete msg;
                Close();
                }
            else
                {
                // send message
                MessageOutputFifo->Push(msg);
                }
            } // end if we have an entire message available 
        else
            {
            // reset data signal
            ModifyInputBuffer().GetSignalReference().reset();
            break;
            } // end else not enough data was available
        } // end while we got a size 
    
    // unlock input buffer
    ModifyInputBuffer().Unlock();
    
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
        TelnetClientData::ReadCompletionRoutine
        );

    if(status)
        {
        // done: read in progress
        //Logger << "[ShareNetClientData::DoInputMaintenance] read started with status " << GetLastError() << "\n";
        return(true);
        }
    
    // close the socket
    Logger << "[TelnetClientData::DoInputMaintenance] read failed to start with status " << GetLastError() << "\n";
    Close();
    
    // done
    return(false);
}

DWORD TelnetClientData::Run(const bool& bContinue)
{
    Logger << "[TelnetClientData::Run] thread id " << GetCurrentThreadId() << " created\n";
    
    // recover the go param to get the message fifos --
    std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>* param;

    param=static_cast<std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>*>(GoParam);
    MessageInputFifo=param->first;
    MessageOutputFifo=param->second;
    
    // done with this -- it was allocated for us when we started
    // and we took ownership
    delete param;

    // do forever
    while(bContinue)
        {
        while(!IsInUse() && bContinue)
            {
            // process input fifo
            while(std::string* msg=MessageInputFifo->Pop())
                {
                // we have no client, discard all messages
                // done with this
                delete msg;
                } // end while stuff is on the fifo
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
            // process input fifo
            while(std::string* msg=MessageInputFifo->Pop())
                {
                // put on output
                QueueOutputMessage(msg->c_str(),msg->length());
                
                // done with this
                delete msg;
                } // end while stuff is on the fifo
                
            // wait for output buffer to have data in it
            switch(GetOutputBuffer().Wait(AlertableWaitSingleFunctor(),100))
                {
                case WAIT_OBJECT_0:
                    // there is data to be sent
                    DoOutputMaintenance();
                    break;
                    
                case WAIT_IO_COMPLETION:
                    //Logger << "[ShareNetClientData::Run] I/O Completion!\n";
                case WAIT_TIMEOUT:
                default:
                    //Logger << "[ShareNetClientData::Run] relooping..." << "\n";
                    break;
                } // end switch wait result
            
            } // end while IsInUse()            
        } // end forever
    
    // done
    return(0);
} // end TelnetClientData::Run

bool TelnetClientData::Open
    (
    const std::string& remote_addr,
    const std::string& remote_port
    )
{
    std::stringstream ss;
    
    ss << remote_port;
    
    unsigned short port;
    ss >> port;
    
    return(Open(remote_addr.c_str(),port));
} // end Open(std string)

bool TelnetClientData::Open
    (
    const char* remote_addr,
    unsigned short remote_port
    )
{
    if(IsInUse())
        {
        ::Logger << "[TelnetClientData::Open] already in use!\n";
        return(false);
        }

    AutoLock al(MyLock);

    // prep addresses: remote next
    ModifyRemoteAddr().sin_family=AF_INET;
    ModifyRemoteAddr().sin_port=htons(remote_port);
    
    unsigned long ul_addr=inet_addr(remote_addr);
    
    if(ul_addr==INADDR_NONE)
        {
        // no suitable address found, try looking it up
        HOSTENT* host=gethostbyname(remote_addr);

        if(host)
            {
            // take first one in the list
            ul_addr=*((unsigned long*)host->h_addr_list[0]);
            }
        else
            {
            // must have a valid remote address!
            Logger << "[TelnetClientData::Open] unable to find remote address!\n";
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
        Logger << "[TelnetClientData::Open] error " << WSAGetLastError() << " on socket()\n";
        return(false);
        }

    int err;

    // connect
    err=connect(s,(const struct sockaddr *)&GetRemoteAddr(),sizeof(SOCKADDR_IN));

    if(err==SOCKET_ERROR)
        {
        closesocket(s);
        Logger << "[TelnetClientData::Open] unable to connect to remote address!\n";
        return(false);
        }

    // call Open overload to finish up the initialization
    // there is some duplication of effort here, but its not
    // a big deal
    
    return(Open(s));
} // end Open w/C args

bool TelnetClientData::Open(SOCKET s)
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
    
    // set OOB inline
    char opt=1;
    setsockopt(s,SOL_SOCKET,SO_OOBINLINE,&opt,sizeof(opt));
    
    // save socket
    SetSocket(s);
    
    // finally, request help to be printed
    MessageOutputFifo->Push(new std::string("help"));
    
    // done: success
    return(true);
} // end Open w/SOCKET

bool TelnetClientData::QueueOutputMessage(const void* data,const unsigned short length)
{
    // this function can block the calling thread!
    
    // length MUST NOT include the prepended size (that is a function of ShareNet, not any client of ShareNet)!
    
    if(!IsInUse())
        {
        //::Logger << "[ShareNetClientData::QueueOutputMessage] not connected!\n";
        return(false);
        }
    
    // the buffer provides its own thread safety, but
    // since it is important the the size immediately
    // precede the data it references, we lock the buffer
    // ourselves

    if(ModifyOutputBuffer().Insert(data,length))
        {
        // attempt to send immediately
        //DoOutputMaintenance();
        ModifyOutputBuffer().Unlock();
        return(true);
        }
    else
        {
        ::Logger << "[TelnetClientData::QueueOutputMessage] can't add message!\n";
        ModifyOutputBuffer().Unlock();
        return(false);
        }
} // end QueueOutputMessage

void TelnetClientData::Close(void)
{
    if(!IsInUse())
        {
        return;
        }

    AutoLock al(MyLock);

    // cancel pending I/O
    //CancelIo(GetSocket());
    
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

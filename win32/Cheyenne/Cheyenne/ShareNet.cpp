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
#include <sstream>

class AlertableWaitSingleFunctor
{
public:
    typedef DWORD return_type;
    DWORD operator()(HANDLE hEvent,unsigned int timeout_ms) const
    {
        return(WaitForSingleObjectEx(hEvent,timeout_ms,TRUE));
    }
}; // end class WaitSingleFunctor

template <typename MSG_T> CheyenneMessage* ShareMsgPopulate
    (
    const unsigned char* opcode_msg_ptr // points to the opcode in the message
    )
{
    // make new message, constructor fills in the opcode
    MSG_T* msg=new MSG_T;
    // alias into message implementation
    const typename MSG_T::impl_t* data=reinterpret_cast<const typename MSG_T::impl_t*>(&opcode_msg_ptr[sizeof(share_opcodes::opcode_t)]);
    // use assignment operator to store data
    msg->data = *data;
    // done
    return(msg);
}

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
    
    //::Logger << "[ShareNetClientData::ReadCompletionRoutine] got " << dwNumberOfBytesTransfered << " bytes\n";
    
    // if we get here and there's no data, assume something bad happened
    if(dwNumberOfBytesTransfered == 0)
        {
        Logger << "[ShareNetClientData::ReadCompletionRoutine] closing! " << WSAGetLastError() << "\n";
        pMe->Close();
        return;
        }
    
    // we got some data,
    // put it on the input buffer
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

bool ShareNetClientData::DoInputMaintenance(void)
{
    // lock input buffer -- this is to prevent us from looping
    // in the while loop if we receive a partial message: 
    // the while loop resets the data signal if it needs more
    // data. The signal will be set again when more data
    // arrives
    ModifyInputBuffer().Lock();
    
    // build messages from the input buffer
    unsigned short sz;
    while(ModifyInputBuffer().Peek(&sz,sizeof(sz)))
        {
        unsigned char data[TempBufferSize];
        if(ModifyInputBuffer().Extract(&data[0],sz))
            {
            // ping check
            if(sz==sizeof(sz))
                {
                // this is a zero-length "ping", update with that
                // info and do NOT pass it along
                SavePingTime();
                }
            else
                {
                // handle a sharemessage
                //::Logger << "[ShareNetClientData::DoInputMaintenance] message size=" << sz << ":\n";
                //for(int i=0;i<sz;++i)
                    //{
                    //::Logger << unsigned int(data[i]) << "\n";
                    //}
                BuildShareMessage(&data[0],sz);
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
        ShareNetClientData::ReadCompletionRoutine
        );

    if(status)
        {
        // done: read in progress
        //Logger << "[ShareNetClientData::DoInputMaintenance] read started with status " << GetLastError() << "\n";
        return(true);
        }
    
    // close the socket
    Logger << "[ShareNetClientData::DoInputMaintenance] read failed to start with status " << GetLastError() << "\n";
    Close();
    
    // done
    return(false);
}

DWORD ShareNetClientData::Run(const bool& bContinue)
{
    Logger << "[ShareNetClientData::Run] thread id " << GetCurrentThreadId() << " created\n";
    
    // recover the go param to get the output message fifo --
    // this fifo is used to queue messages FROM the network
    // somebody else must pop the off (e.g. the database in Cheyenne)!
    MessageOutputFifo=static_cast<tsfifo<CheyenneMessage*>*>(GoParam);

    // do forever
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
                    //Logger << "[ShareNetClientData::Run] I/O Completion!\n";
                case WAIT_TIMEOUT:
                default:
                    //Logger << "[ShareNetClientData::Run] relooping..." << "\n";
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

bool ShareNetClientData::Open
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

bool ShareNetClientData::Open
    (
    const char* remote_addr,
    unsigned short remote_port
    )
{
    if(IsInUse())
        {
        ::Logger << "[ShareNetClientData::Open] already in use!\n";
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
        Logger << "[ShareNetClientData::Open] error " << WSAGetLastError() << " on socket()\n";
        return(false);
        }

    int err;

    // connect
    err=connect(s,(const struct sockaddr *)&GetRemoteAddr(),sizeof(SOCKADDR_IN));

    if(err==SOCKET_ERROR)
        {
        closesocket(s);
        Logger << "[ShareNetClientData::Open] unable to connect to remote address!\n";
        return(false);
        }

    // call Open overload to finish up the initialization
    // there is some duplication of effort here, but its not
    // a big deal
    
    return(Open(s));
} // end Open w/C args

bool ShareNetClientData::Open(SOCKET s)
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
    
    // save socket
    SetSocket(s);

    // save ping time
    SetLastPingTime(Clock.Current());
    SetLastPingSendTime(Clock.Current());

    // done: success
    return(true);
} // end Open w/SOCKET

bool ShareNetClientData::QueueOutputMessage(const void* data,const unsigned short length)
{
    // this function can block the calling thread!
    
    // length MUST NOT include the prepended size (that is a function of ShareNet, not any client of ShareNet)!
    
    if(!IsInUse())
        {
        ::Logger << "[ShareNetClientData::QueueOutputMessage] not connected!\n";
        return(false);
        }
    
    // the buffer provides its own thread safety, but
    // since it is important the the size immediately
    // precede the data it references, we lock the buffer
    // ourselves

    // insert size
    unsigned short sz=length+sizeof(sz);
    
    ModifyOutputBuffer().Lock();
    if(!ModifyOutputBuffer().Insert(&sz,sizeof(sz)))
        {
        // woops
        ModifyOutputBuffer().Unlock();
        ::Logger << "[ShareNetClientData::QueueOutputMessage] can't add size!\n";
        return(false);
        }
    
    if(ModifyOutputBuffer().Insert(data,length))
        {
        // attempt to send immediately
        //DoOutputMaintenance();
        ModifyOutputBuffer().Unlock();
        return(true);
        }
    else
        {
        ::Logger << "[ShareNetClientData::QueueOutputMessage] can't add message!\n";
        // remove the size we already added to the buffer
        ModifyOutputBuffer().Extract(&sz,sizeof(sz));
        ModifyOutputBuffer().Unlock();
        return(false);
        }
} // end QueueOutputMessage

unsigned int ShareNetClientData::ExtractInputMessage(void* data,const unsigned int max_len)
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

void ShareNetClientData::Close(void)
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

void ShareNetClientData::BuildShareMessage
    (
    const unsigned char* buf,
    const unsigned short len
    )
{
    // first 2 bytes are the size of the entire message --
    // which was passed in as the len
    // third byte is the opcode
    // alias pointer to opcode and go from there
    const unsigned char* message=&buf[sizeof(unsigned short)];
    share_opcodes::c_opcode_t opcode=message[0];
    
    switch(opcode)
        {
        case share_opcodes::request_full_update:
            MessageOutputFifo->Push
                (
                ShareMsgPopulate<sharemessages::request_full_update>(message)
                );
            break;
            
        case share_opcodes::full_update:
            MessageOutputFifo->Push
                (
                ShareMsgPopulate<sharemessages::full_update>(message)
                );
            break;
            
        case share_opcodes::heartbeat_update:
            MessageOutputFifo->Push
                (
                ShareMsgPopulate<sharemessages::heartbeat_update>(message)
                );
            break;
            
        case share_opcodes::threshold_update:
            MessageOutputFifo->Push
                (
                ShareMsgPopulate<sharemessages::threshold_update>(message)
                );
            break;

        case share_opcodes::visibility_update:
            MessageOutputFifo->Push
                (
                ShareMsgPopulate<sharemessages::visibility_update>(message)
                );
            break;
            
        default:
            Logger << "[ShareNetClientData::BuildShareMessage] unknown opcode: " 
                   << unsigned int(opcode) << "\n";
            break;
        }; // end switch opcode
    
    // done
    return;
} // end BuildShareMessage

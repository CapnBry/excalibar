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
#define NOMINMAX
#include <winsock2.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "..\Utils\CodeUtils.h" // for general utilities
#include "..\Utils\threads.h" // for thread defs
#include "..\Utils\buffer.h" // for buffer defs

/*
DStreamConnection is a template class that wraps a thread around
a data-handling function.

HandleData() will be called from the context
of a thread that is created when Go() is called, so if synchronization
is needed, you must provide it. I suggest one of the lock objects 
in Utils\Locks.h. See tons of Cheyenne code for an example :).

Alternatively, the Run() function can be modified to execute repeatedly
in the context of the calling thread. If this is done, then there is 
no reason to derive DStreamConnection from Thread.

Sample of operation:

    void HandleData(const dstream::dstream_header* hdr,const void* payload)
    {
        // only store daoc data
        if(hdr->command_id==dstream_opcodes::DPACKET_DAOC_DATA)
            {
            // recover type info from original payload
            const dstream::DPACKET_DAOC_DATA* msg=reinterpret_cast<const dstream::DPACKET_DAOC_DATA*>(payload);
            
            // We now have the header for the DStream packet and the DAoC data payload in msg -- 
            // msg has the original sniffed contents of the message, and can tell us
            // if it is tcp or udp, and transmitted from the server or the client.
            
            // The data field of msg literally is the unencrypted contents of the message
            // as acquired by the DStream server. It's length is msg->data_size
            }
    } // end HandleData

    typedef void(__cdecl* HANDLEDATA_T)(const dstream::dstream_header*,const void*);
    DStreamConnection<HANDLEDATA_T> DStream(HandleData); // our network connection

    if(!DStream.Open(addr,port))
        {
        std::cerr << "Unable to connect to " << addr << ":" << port << ", exiting.\n";
        goto Exit;
        }
    else
        {
        DStream.Go();
        Sleep(100); // sleep makes the order of the log messages appear "sensible" :)
        std::cout << "Connected to " << addr << ":" << port << ".\n";
        }
    
    while(!kbhit()){Sleep(100);}

*/


namespace dstream_opcodes
{
// type defs for the types used here
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned char uchar;
typedef int int32;

const uchar DPACKET_HELO=0x01; // - DStream protocol version (DPACKET_HELO)
const uchar DPACKET_LOG=0x02; //  - Log message (DPACKET_LOG) 
const uchar DPACKET_CONNECTION_QUERY=0x03; // - Request connection information (DPACKET_CONNECTION_QUERY) 
const uchar DPACKET_CONNECTION_DETAILS=0x04; // - DAoC connection information (DPACKET_CONNECTION_DETAILS) 
const uchar DPACKET_DAOC_CONNECTION_OPENED=0x05; //  - DAoC connection established (DPACKET_DAOC_CONNECTION_OPENED) 
const uchar DPACKET_DAOC_CONNECTION_CLOSED=0x06; //  - DAoC connection terminiated (DPACKET_DAOC_CONNECTION_CLOSED) 
const uchar DPACKET_DAOC_DATA=0x07; // - DAoC packet data (DPACKET_DAOC_DATA) 
const uchar DPACKET_SET_PACKETFILTER=0x08; //  - Set DAoC packet filter (DPACKET_SET_PACKETFILTER) 
const uchar DPACKET_AUTH_CREDENTIALS=0x09; //  - Authentication request (DPACKET_AUTH_CREDENTIALS) 
const uchar DPACKET_AUTH_RESPONSE=0x0a; //  - Authentication response (DPACKET_AUTH_RESPONSE) 
const uchar DPACKET_SQUELCH=0x0b; //  - Squelch connection data (DPACKET_SQUELCH) 
const uchar DPACKET_RESUME=0x0c; // - Resume connection data (DPACKET_RESUME) 
const uchar DPACKET_KEYS=0x0d; // - Send keystrokes (DPACKET_KEYS)
const uchar DPACKET_MOUSE=0x0e; // - Send mouse command (DPACKET_MOUSE)
}; // end namespace dstream_opcodes

namespace dstream
{
// type defs for the types used here
typedef dstream_opcodes::uint16 uint16;
typedef dstream_opcodes::uint32 uint32;
typedef dstream_opcodes::uchar uchar;
typedef dstream_opcodes::int32 int32;

#pragma pack(push,ds,1)

struct dstream_header { 
  uint16 total_length;  // includes sizeof(dstream_header) 
  uchar command_id; 
};


struct DPACKET_HELO
{
    char signature[4];  // string = DSTM 
    int32 version_no; 
    uchar authentication_required;  // 0 no, 1 yes 
};

struct DPACKET_LOG
{
uchar log_level;
char message[1];
};

struct DPACKET_CONNECTION_QUERY
{
uint32 connectionid;  // 0 = list all connections 
};

struct daoc_connection_details
{  // All ips and ports are in network byte order 
uint32 connectionid;
uint32 server_ip;
uint16 server_port;
uint32 client_ip;
uint16 client_port;
};

struct DPACKET_CONNECTION_DETAILS
{
uchar count;
daoc_connection_details details[1];
};

struct DPACKET_DAOC_CONNECTION_OPENED
{
daoc_connection_details details;
};

struct DPACKET_DAOC_CONNECTION_CLOSED
{
daoc_connection_details details;
};

struct DPACKET_DAOC_DATA
{
uint32 connectionid;
uchar origin;  // 0 from server, 1 from client 
uchar protocol;   // 0 for TCP, 1 for UDP 
uint16 data_size; 
uchar data[1];
};

struct DPACKET_SET_PACKETFILTER
{
};
struct DPACKET_AUTH_CREDENTIALS
{
};

struct DPACKET_AUTH_RESPONSE
{
uchar response_code;  // 0 failed, 1 success 
char response_text[1];
};

struct DPACKET_SQUELCH
{
uint32 connectionid;
};

struct DPACKET_RESUME
{
uint32 connectionid;
};

struct DPACKET_KEYS
{
uint32 connectionid;
char keys[1];
}; 

struct DPACKET_MOUSE
{
uint32 connectionid;
char mousecommand[1];
};

#pragma pack(pop,ds)

}; // end namespace dstream

template<typename HANDLER_T>class DStreamConnection : public Thread
{
public:
    DStreamConnection(HANDLER_T _handler) : TempBufferSize(2048),Handler(_handler)
    {
        // init to invalid
        SetSocket(INVALID_SOCKET);
        bHeloValidated=false;
        ZeroMemory(&ModifyLocalAddr(),sizeof(SOCKADDR_IN));
        ZeroMemory(&ModifyRemoteAddr(),sizeof(SOCKADDR_IN));
        
        // create
        ReadBuf=new unsigned char[TempBufferSize];

        // startup winsock
        WSADATA wsa;
        WSAStartup(MAKEWORD(2,0),&wsa);
        } // end DStreamConnection
        
    virtual ~DStreamConnection()
    {
        // make sure disconnected
        Close();
        
        // make sure stopped
        Stop();
        
        // delete 
        delete[] ReadBuf;
        
        // cleanup
        WSACleanup();
    } // end ~DStreamConnection
    
    bool IsConnected(void)const{return(!(GetSocket()==INVALID_SOCKET));};
    std::string GetStatusString(void)const{return(IsConnected()?"connected":"disconnected");};
    // entry point for transmitting data
    bool Transmit(const dstream::dstream_header* const packet)
    {
        // put on output buffer, return status
        return(ModifyOutputBuffer().Insert(packet,packet->total_length));
    } // end Transmit
    
    bool Open(const std::string& remote_addr,const std::string& remote_port)
    {
        std::stringstream ss;
        
        ss << remote_port;
        
        unsigned short port;
        ss >> port;
        
        return(Open(remote_addr.c_str(),port));
    } // end Open(std string)

    // this one takes "C" arguments to make it simple :/
    // all parameters are in HOST order
    bool Open(const char* remote_addr,unsigned short remote_port)
    {
        if(IsConnected())
            {
            std::cerr << "already in use!\n";
            return(false);
            }

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
                std::cerr << "unable to find remote address, error " << WSAGetLastError() << "\n";
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
            std::cerr << "error " << WSAGetLastError() << " on socket()\n";
            return(false);
            }

        // connect
        int err=connect(s,(const struct sockaddr *)&GetRemoteAddr(),sizeof(SOCKADDR_IN));

        if(err==SOCKET_ERROR)
            {
            closesocket(s);
            std::cerr << "unable to connect to remote address, error " << WSAGetLastError() << "\n";
            return(false);
            }

        // call Open overload to finish up the initialization
        // there is some duplication of effort here, but its not
        // a big deal
        
        return(Open(s));
    } // end Open w/C args
    
    bool Open(SOCKET s)
    {
        if(IsConnected())
            {
            // hmph.
            return(false);
            }

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
        
        // done: success
        return(true);
    } // end Open w/SOCKET

    void Close(void)
    {
        if(!IsConnected())
            {
            // done
            return;
            }

        // close down send side
        shutdown(GetSocket(),SD_SEND);

        // wait for input clear
        fd_set read_fds;
        ZeroMemory(&read_fds,sizeof(read_fds));
        TIMEVAL tv;
        tv.tv_sec=1; // wait 1 second
        tv.tv_usec=0;
        FD_SET(GetSocket(),&read_fds);
        select(0,&read_fds,NULL,NULL,&tv);
        if(FD_ISSET(GetSocket(),&read_fds))
            {
            // attempt to read as much as we can
            
            // make non-blocking
            ioctlsocket(GetSocket(),FIONBIO,(u_long*)1);
            char temp[1024];
            int cnt=0;
            int status=1;

            while(status!=0 && status != SOCKET_ERROR && cnt<10)
                {
                status=recv(GetSocket(),temp,sizeof(temp),0);
                ++cnt;
                }
            // make blocking again
            ioctlsocket(GetSocket(),FIONBIO,(u_long*)0);
            } // end if socket readable

        // close down receive side
        shutdown(GetSocket(),SD_RECEIVE);

        // close socket
        closesocket(GetSocket());

        // mark invalid
        SetSocket(INVALID_SOCKET);

        // flush buffers
        ModifyInputBuffer().Flush();
        ModifyOutputBuffer().Flush();

        // set to false
        bHeloValidated=false;
        
        // done
        return;
    } // end Close
    
protected:
    virtual void ProcessInput(buffer_space::Buffer& Input)
    {
        // first, see if we have a header
        if(Input.Size() < sizeof(dstream::dstream_header))
            {
            // no header yet, bail out
            return;
            }
        
        // get the header (peek for now, until we verify the size)
        dstream::dstream_header hdr;
        Input.Peek(&hdr,sizeof(hdr));
        
        if(Input.Size() < hdr.total_length)
            {
            // sanity check: hdr.total_length better be "sane"
            if(hdr.total_length > 32768)
                {
                // single 32k message? Hmmm....we're gonna bail on this connection
                std::cout << "Header total length is too big, closing connection. Is the server okay?\n";
                Close();
                }
            // we don't have the whole message yet, bail out
            return;
            }
        
        // at this point, we have the whole message, handle it here
        // re-get header with extract
        Input.Extract(&hdr,sizeof(hdr));

        // extract payload into new buffer
        const unsigned int payload_len=hdr.total_length-sizeof(hdr);
        unsigned char* payload=new unsigned char[payload_len];
        Input.Extract(payload,hdr.total_length-sizeof(hdr));
        
        // if this is not a helo packet, make sure
        // we have validated the helo packet already
        // if not, then ignore
        if(bHeloValidated || hdr.command_id == dstream_opcodes::DPACKET_HELO)
            {
            // gotta use reinterpret cast here (or C-style cast) since
            // dstream::* doesn't inherit from unsigned char ;)
            switch(hdr.command_id)
                {
                case dstream_opcodes::DPACKET_HELO:
                    {
                    dstream::DPACKET_HELO* msg=reinterpret_cast<dstream::DPACKET_HELO*>(payload);
                    std::cout << "got HELO packet:\n";
                    std::cout << "\t" << msg->signature[0] << msg->signature[1] << msg->signature[2] << msg->signature[3] << "\n"
                        << "\tversion=" << msg->version_no << "\n"
                        << "\tauthentication required=" << (msg->authentication_required?"yes":"no") << "\n";
                    
                    if(!ValidateHelo(*msg))
                        {
                        std::cerr << "ValidateHelo() failed! Disconnecting!\n";
                        // close
                        Close();
                        }

                    // set to true
                    bHeloValidated=true;
                    }
                    break;
                    
                case dstream_opcodes::DPACKET_LOG:
                    std::cout << "got LOG packet:\n";
                    break;
                    
                case dstream_opcodes::DPACKET_CONNECTION_QUERY:
                    std::cout << "got CONNECTION QUERY packet:\n";
                    // how does the client get this message??!?
                    break;

                case dstream_opcodes::DPACKET_CONNECTION_DETAILS:
                    {
                    dstream::DPACKET_CONNECTION_DETAILS* msg=reinterpret_cast<dstream::DPACKET_CONNECTION_DETAILS*>(payload);
                    
                    SOCKADDR_IN addr;
                    
                    std::cout << "got CONNECTION DETAILS packet:\n";
                    std::cout << unsigned int(msg->count) << " connections:\n";
                    for(dstream::uchar count=0;count<msg->count;++count)
                        {
                        std::cout << "\tConnection " << unsigned int(count) << ":\n"
                            << "\tconnection id=" << msg->details[count].connectionid << "\n";
                        
                        addr.sin_addr.S_un.S_addr=msg->details[count].server_ip;
                        addr.sin_port=msg->details[count].server_port;
                        std::cout << "\tserver=" << addr << "\n";

                        addr.sin_addr.S_un.S_addr=msg->details[count].client_ip;
                        addr.sin_port=msg->details[count].client_port;
                        std::cout << "\tclient=" << addr << "\n";
                        }
                    }
                    break;

                case dstream_opcodes::DPACKET_DAOC_CONNECTION_OPENED:
                    {
                    std::cout << "got CONNECTION OPENED packet:\n";
                    
                    dstream::DPACKET_DAOC_CONNECTION_OPENED* msg=reinterpret_cast<dstream::DPACKET_DAOC_CONNECTION_OPENED*>(payload);
                    SOCKADDR_IN addr;
                    std::cout << "\tconnection id=" << msg->details.connectionid << "\n";
                    
                    addr.sin_addr.S_un.S_addr=msg->details.server_ip;
                    addr.sin_port=msg->details.server_port;
                    std::cout << "\tserver=" << addr << "\n";

                    addr.sin_addr.S_un.S_addr=msg->details.client_ip;
                    addr.sin_port=msg->details.client_port;
                    std::cout << "\tclient=" << addr << "\n";
                    
                    // save it
                    Handler(&hdr,msg);
                    }
                    break;

                case dstream_opcodes::DPACKET_DAOC_CONNECTION_CLOSED:
                    {
                    std::cout << "got CONNECTION CLOSED packet:\n";
                    
                    dstream::DPACKET_DAOC_CONNECTION_OPENED* msg=reinterpret_cast<dstream::DPACKET_DAOC_CONNECTION_OPENED*>(payload);
                    SOCKADDR_IN addr;
                    std::cout << "\tconnection id=" << msg->details.connectionid << "\n";
                    
                    addr.sin_addr.S_un.S_addr=msg->details.server_ip;
                    addr.sin_port=msg->details.server_port;
                    std::cout << "\tserver=" << addr << "\n";

                    addr.sin_addr.S_un.S_addr=msg->details.client_ip;
                    addr.sin_port=msg->details.client_port;
                    std::cout << "\tclient=" << addr << "\n";

                    // save it
                    Handler(&hdr,msg);
                    }
                    break;

                case dstream_opcodes::DPACKET_DAOC_DATA:
                    {
                    dstream::DPACKET_DAOC_DATA* msg=reinterpret_cast<dstream::DPACKET_DAOC_DATA*>(payload);
                    // save it
                    Handler(&hdr,msg);
                    }
                    break;

                case dstream_opcodes::DPACKET_SET_PACKETFILTER:
                    std::cout << "got SET PACKETFILTER packet:\n";
                    // how does a client get this message?!??
                    break;

                case dstream_opcodes::DPACKET_AUTH_CREDENTIALS:
                    std::cout << "got AUTH CREDENTIALS packet:\n";
                    break;

                case dstream_opcodes::DPACKET_AUTH_RESPONSE:
                    std::cout << "got AUTH RESPONSE packet:\n";
                    break;
                
                case dstream_opcodes::DPACKET_SQUELCH:
                    std::cout << "got PACKET SQUELCH packet:\n";
                    break;

                case dstream_opcodes::DPACKET_RESUME:
                    std::cout << "got PACKET RESUME packet:\n";
                    break;

                default:
                    // get the rest message
                    std::cout << "unhandled opcode (" << unsigned int(hdr.command_id) << ")\n";
                    break;
                } // end switch opcode
            } // end helo validation check
        
        // delete payload
        delete[] payload;
        
        // done
        return;
    } // end ProcessInput
    
private:
    virtual DWORD Run(const bool& bContinue)
    {
        std::cout << "thread started with ID " << GetCurrentThreadId() << "\n";
        
        while(bContinue)
            {
            while(!IsConnected() && bContinue)
                {
                Sleep(500);
                }
            
            if(!bContinue)
                {
                break;
                }
            
            // start read
            DoInputMaintenance();
            
            // loop until we disconnect or terminate
            while(IsConnected() && bContinue)
                {
                // wait for output buffer to have data in it
                switch(GetOutputBuffer().Wait(AlertableWaitSingleFunctor(),100))
                    {
                    case WAIT_OBJECT_0:
                        // there is data to be sent
                        DoOutputMaintenance();
                        break;
                        
                    case WAIT_IO_COMPLETION:
                    case WAIT_TIMEOUT:
                    default:
                        break;
                    } // end switch wait result
                } // end while IsConnected()            
            } // end forever    
            
        std::cout << "thread exiting with ID " << GetCurrentThreadId() << "\n";
        
        // done
        return(0);
    } // end Run

    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        )
    {
        // extract original info: Having to do this
        // is HIDEOUS, but thats the way Microsoft wrote their
        // completion routines: no easy way to store a decent
        // "context" parameter.
        DStreamConnection* pMe=(DStreamConnection*)lpOverlapped->hEvent;
        
        // if we get here and there's no data, assume something bad happened
        if(dwNumberOfBytesTransfered == 0)
            {
            std::cerr << "closing, error " << WSAGetLastError() << "\n";
            pMe->Close();
            return;
            }
        
        // we got some data,
        // put it on the input buffer
        pMe->ModifyInputBuffer().Insert(&(pMe->ReadBuf[0]),dwNumberOfBytesTransfered);
        
        // see if we are still running
        if(pMe->IsConnected())
            {
            // start another read
            pMe->DoInputMaintenance();
            }
        else
            {
            std::cerr << "not connected, not restarting read\n";
            }

        // done
        return;
    } // end DStreamConnection::ReadCompletionRoutine

    bool DoOutputMaintenance(void)
    {
        // send in 2k chunks
        char buf[2048];
        
        unsigned int size=min((unsigned int)sizeof(buf),GetOutputBuffer().Size());

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
        
        // done, return true to indicate that there is 
        // something else to do, false to indicate
        // that the output queue is empty
        return(GetOutputBuffer().Size() != 0);
    } // end DoOutputMaintenance

    bool DoInputMaintenance(void)
    {
        // lock input buffer -- this is to prevent us from looping
        // in the while loop if we receive a partial message: 
        // the while loop resets the data signal if it needs more
        // data. The signal will be set again when more data
        // arrives
        ModifyInputBuffer().Lock();
        
        // build messages from the input buffer
        if(GetInputBuffer().Size() != 0)
            {
            // call handler function
            ProcessInput(ModifyInputBuffer());
            }

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
            DStreamConnection::ReadCompletionRoutine
            );

        if(status)
            {
            // done: read in progress
            return(true);
            }
        
        // close the socket
        std::cerr << "read failed to start with status " << GetLastError() << "\n";
        Close();
        
        // done
        return(false);
    } // end DoInputMaintenance

    
    bool ValidateHelo(const dstream::DPACKET_HELO& helo)const
    {
        // first, check signature
        if(helo.signature[0] != 'D' ||
        helo.signature[1] != 'S' ||
        helo.signature[2] != 'T' ||
        helo.signature[3] != 'M')
            {
            // signature check failed
            std::cerr << "signature check failed\n";
            return(false);
            }
        
        // check version
        if(helo.version_no < 1 || helo.version_no > 2)
            {
            std::cerr << "version check failed: expected 1 or 2, got " << helo.version_no << "\n";
            return(false);
            }
        
        // ignore the authentication stuff
        
        // done
        return(true);
    } // end ValidateHelo    
    
    bool bHeloValidated;
    
    DECL_MEMBER_PRIVATE(SOCKET,Socket);
    DECL_MEMBER_VIEW(SOCKADDR_IN,LocalAddr);
    DECL_MEMBER_VIEW(SOCKADDR_IN,RemoteAddr);
    DECL_MEMBER_PRIVATE(buffer_space::Buffer,InputBuffer);
    DECL_MEMBER_PRIVATE(buffer_space::Buffer,OutputBuffer);

    // read variables
    OVERLAPPED ReadOverlapped;
    const unsigned int TempBufferSize;
    unsigned char* ReadBuf; // only accessed from the DoInputMaintenance and read completion
                        // functions, but must be a non-static class member
                        
    HANDLER_T Handler;

}; // end class DStreamConnection

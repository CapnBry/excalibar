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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <sstream> // for string stream defs
#include <map> // for std::map
#include "..\Utils\CodeUtils.h" // for various code utilities
#include "..\Utils\Logger.h" // for the logger def
#include "..\Utils\buffer.h" // for the buffer defs
#include "..\Utils\threads.h" // for thread class definitions
#include "..\Sniffer\daocconnection.h" // for daoc connection definition
#include "DStreamConnection.h"

// reference a logger (this must be instantiated in 
// a translation unit somewhere)
extern logger_t Logger;

// initialize this
const unsigned int DStreamConnection::TempBufferSize=2048;

DStreamConnection::DStreamConnection()
{
    // init to invalid
    SetSocket(INVALID_SOCKET);
    bHeloValidated=false;
    ZeroMemory(&ModifyLocalAddr(),sizeof(SOCKADDR_IN));
    ZeroMemory(&ModifyRemoteAddr(),sizeof(SOCKADDR_IN));
    
    // init to null
    fifo=0;
    
    // create
    ReadBuf=new unsigned char[TempBufferSize];

    // startup winsock
    WSADATA wsa;
    WSAStartup(MAKEWORD(2,0),&wsa);
} // end DStreamConnection

DStreamConnection::~DStreamConnection()
{
    // make sure disconnected (this also cleans up the SniffMap)
    Close();
    
    // make sure stopped
    Stop();
    
    // delete 
    delete[] ReadBuf;
    
    // cleanup
    WSACleanup();
    
} // end ~DStreamConnection

bool DStreamConnection::Open
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

bool DStreamConnection::Open
    (
    const char* remote_addr,
    unsigned short remote_port
    )
{
    if(IsConnected())
        {
        LOG_FUNC << "already in use!\n";
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
            LOG_FUNC << "unable to find remote address, error " << WSAGetLastError() << "\n";
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
        LOG_FUNC << "error " << WSAGetLastError() << " on socket()\n";
        return(false);
        }

    int err;

    // connect
    err=connect(s,(const struct sockaddr *)&GetRemoteAddr(),sizeof(SOCKADDR_IN));

    if(err==SOCKET_ERROR)
        {
        closesocket(s);
        LOG_FUNC << "unable to connect to remote address, error " << WSAGetLastError() << "\n";
        return(false);
        }

    // call Open overload to finish up the initialization
    // there is some duplication of effort here, but its not
    // a big deal
    
    return(Open(s));
} // end Open w/C args

bool DStreamConnection::Open(SOCKET s)
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

void DStreamConnection::Close(void)
{
    if(!IsConnected())
        {
        // done
        return;
        }

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

    // delete all SniffMap entries
    while(SniffMap.begin() != SniffMap.end())
        {
        // delete a single entry
        delete SniffMap.begin()->second;
        
        SniffMap.erase(SniffMap.begin());
        }
    
    // set to false
    bHeloValidated=false;
    
    // done
    return;
} // end Close

DWORD DStreamConnection::Run(const bool& bContinue)
{
    LOG_FUNC << "thread started with ID " << GetCurrentThreadId() << "\n";
    
    // save fifo
    fifo=reinterpret_cast<tsfifo<CheyenneMessage*>*>(GoParam);
    
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
        
    LOG_FUNC << "thread exiting with ID " << GetCurrentThreadId() << "\n";
    
    // done
    return(0);
} // end Run

bool DStreamConnection::DoOutputMaintenance(void)
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

bool DStreamConnection::DoInputMaintenance(void)
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
    LOG_FUNC << "read failed to start with status " << GetLastError() << "\n";
    Close();
    
    // done
    return(false);
} // end DoInputMaintenance

VOID CALLBACK DStreamConnection::ReadCompletionRoutine
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
    DStreamConnection* pMe=(DStreamConnection*)lpOverlapped->hEvent;
    
    // if we get here and there's no data, assume something bad happened
    if(dwNumberOfBytesTransfered == 0)
        {
        LOG_FUNC << "closing, error " << WSAGetLastError() << "\n";
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
        LOG_FUNC << "not connected, not restarting read\n";
        }

    // done
    return;
} // end DStreamConnection::ReadCompletionRoutine

void DStreamConnection::ProcessInput(buffer_space::Buffer& Input)
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
            LOG_FUNC << "Header total length is too big, closing connection. Is the server okay?\n";
            Close();
            }
        // we don't have the whole message yet, bail out
        return;
        }
    
    // at this point, we have the whole message, handle it here
    // re-get header with extract
    Input.Extract(&hdr,sizeof(hdr));

    // extract payload
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
                LOG_FUNC << "got HELO packet:\n";
                Logger << "\t" << msg->signature[0] << msg->signature[1] << msg->signature[2] << msg->signature[3] << "\n"
                    << "\tversion=" << msg->version_no << "\n"
                    << "\tauthentication required=" << (msg->authentication_required?"yes":"no") << "\n";
                
                if(!ValidateHelo(*msg))
                    {
                    LOG_FUNC << "ValidateHelo() failed! Disconnecting!\n";
                    // close
                    Close();
                    }

                // set to true
                bHeloValidated=true;
                }
                break;
                
            case dstream_opcodes::DPACKET_LOG:
                LOG_FUNC << "got LOG packet:\n";
                break;
                
            case dstream_opcodes::DPACKET_CONNECTION_QUERY:
                LOG_FUNC << "got CONNECTION QUERY packet:\n";
                // how does the client get this message??!?
                break;

            case dstream_opcodes::DPACKET_CONNECTION_DETAILS:
                {
                dstream::DPACKET_CONNECTION_DETAILS* msg=reinterpret_cast<dstream::DPACKET_CONNECTION_DETAILS*>(payload);
                
                SOCKADDR_IN addr;
                
                LOG_FUNC << "got CONNECTION DETAILS packet:\n";
                Logger << unsigned int(msg->count) << " connections:\n";
                for(dstream::uchar count=0;count<msg->count;++count)
                    {
                    Logger << "\tConnection " << unsigned int(count) << ":\n"
                        << "\tconnection id=" << msg->details[count].connectionid << "\n";
                    
                    addr.sin_addr.S_un.S_addr=msg->details[count].server_ip;
                    addr.sin_port=msg->details[count].server_port;
                    Logger << "\tserver=" << addr << "\n";

                    addr.sin_addr.S_un.S_addr=msg->details[count].client_ip;
                    addr.sin_port=msg->details[count].client_port;
                    Logger << "\tclient=" << addr << "\n";
                    }
                }
                break;

            case dstream_opcodes::DPACKET_DAOC_CONNECTION_OPENED:
                {
                LOG_FUNC << "got CONNECTION OPENED packet:\n";
                
                dstream::DPACKET_DAOC_CONNECTION_OPENED* msg=reinterpret_cast<dstream::DPACKET_DAOC_CONNECTION_OPENED*>(payload);
                SOCKADDR_IN addr;
                Logger << "\tconnection id=" << msg->details.connectionid << "\n";
                
                addr.sin_addr.S_un.S_addr=msg->details.server_ip;
                addr.sin_port=msg->details.server_port;
                Logger << "\tserver=" << addr << "\n";

                addr.sin_addr.S_un.S_addr=msg->details.client_ip;
                addr.sin_port=msg->details.client_port;
                Logger << "\tclient=" << addr << "\n";
                
                // add to SniffMap
                AddConnection(msg->details.connectionid);
                }
                break;

            case dstream_opcodes::DPACKET_DAOC_CONNECTION_CLOSED:
                {
                LOG_FUNC << "got CONNECTION CLOSED packet:\n";
                
                dstream::DPACKET_DAOC_CONNECTION_OPENED* msg=reinterpret_cast<dstream::DPACKET_DAOC_CONNECTION_OPENED*>(payload);
                SOCKADDR_IN addr;
                Logger << "\tconnection id=" << msg->details.connectionid << "\n";
                
                addr.sin_addr.S_un.S_addr=msg->details.server_ip;
                addr.sin_port=msg->details.server_port;
                Logger << "\tserver=" << addr << "\n";

                addr.sin_addr.S_un.S_addr=msg->details.client_ip;
                addr.sin_port=msg->details.client_port;
                Logger << "\tclient=" << addr << "\n";
                
                // remove from SniffMap (if exists)
                RemoveConnection(msg->details.connectionid);
                }
                break;

            case dstream_opcodes::DPACKET_DAOC_DATA:
                {
                dstream::DPACKET_DAOC_DATA* msg=reinterpret_cast<dstream::DPACKET_DAOC_DATA*>(payload);
                
                // for now, print it
                /*
                LOG_FUNC << "got DAOC DATA packet:\n";
                Logger << msg->data_size << " bytes " << (msg->protocol==0?"TCP ":"UDP ") << (msg->origin==0?"from server\n":"from client\n");
                std::ostringstream ss;
                ss << std::hex;
                for(unsigned short i=0;i<msg->data_size;++i)
                    {
                    ss << "0x";

                    std::streamsize sz=ss.width(2);
                    std::ostringstream::char_type of=ss.fill('0');
                    
                    ss << (unsigned int)msg->data[i];
                    
                    ss.width(sz);
                    ss.fill(of);
                    
                    if(isprint(msg->data[i]))
                        {
                        ss << " (" << msg->data[i] << ")\n";
                        }
                    else
                        {
                        ss << " ()\n";
                        }
                    } // end for all bytes in payload
                Logger << ss.str() << std::endl;
                */
                
                // find in the SniffMap and pass along to the appropriate connection class
                if(DAOCConnection* conn=FindConnection(msg->connectionid))
                    {
                    if(msg->origin==0)
                        {
                        // from server
                        if(msg->protocol==0)
                            {
                            // tcp
                            conn->FromTCPServer(&msg->data[0],msg->data_size,fifo);
                            }
                        else
                            {
                            // udp
                            conn->FromUDPServer(&msg->data[0],msg->data_size,fifo);
                            }
                        } // end if from server
                    else
                        {
                        // from client
                        if(msg->protocol==0)
                            {
                            // tcp
                            conn->FromTCPClient(&msg->data[0],msg->data_size,fifo);
                            }
                        else
                            {
                            // udp
                            conn->FromUDPClient(&msg->data[0],msg->data_size,fifo);
                            }
                        } // end else from client
                    } // end if connection exists
                else
                    {
                    LOG_FUNC << "Got DAOC DATA packet, but connection " << msg->connectionid << " does not exitst!\n";
                    }
                }
                break;

            case dstream_opcodes::DPACKET_SET_PACKETFILTER:
                LOG_FUNC << "got SET PACKETFILTER packet:\n";
                // how does a client get this message?!??
                break;

            case dstream_opcodes::DPACKET_AUTH_CREDENTIALS:
                LOG_FUNC << "got AUTH CREDENTIALS packet:\n";
                break;

            case dstream_opcodes::DPACKET_AUTH_RESPONSE:
                LOG_FUNC << "got AUTH RESPONSE packet:\n";
                break;
            
            case dstream_opcodes::DPACKET_SQUELCH:
                LOG_FUNC << "got PACKET SQUELCH packet:\n";
                break;

            case dstream_opcodes::DPACKET_RESUME:
                LOG_FUNC << "got PACKET RESUME packet:\n";
                break;

            default:
                // get the rest message
                LOG_FUNC << "unhandled opcode (" << unsigned int(hdr.command_id) << ")\n";
                break;
            } // end switch opcode
        } // end helo validation check
    
    // delete payload
    delete[] payload;
    
    // done
    return;
} // end ProcessInput

DAOCConnection* DStreamConnection::FindConnection(const dstream::uint32 id)const
{
    sniff_map_const_iterator it=SniffMap.find(id);
    
    DAOCConnection* conn;
    if(it!=SniffMap.end())
        {
        conn=it->second;
        }
    else
        {
        conn=0;
        }
    
    // done
    return(conn);
} // end FindConnection

DAOCConnection* DStreamConnection::AddConnection(const dstream::uint32 id)
{
    // see if already exists
    DAOCConnection* conn=FindConnection(id);
    if(!conn)
        {
        // create
        conn=new DAOCConnection;
        
        // insert it
        SniffMap.insert(sniff_map_value_type(id,conn));
        }
    
    // done
    return(conn);
} // end AddConnection

void DStreamConnection::RemoveConnection(const dstream::uint32 id)
{
    sniff_map_iterator it=SniffMap.find(id);
    
    if(it != SniffMap.end())
        {
        // get pointer
        DAOCConnection* conn=it->second;
        
        // erase it
        SniffMap.erase(it);
        
        // delete
        delete conn;
        }
    
    // done
    return;
} // end RemoveConnection

bool DStreamConnection::ValidateHelo(const dstream::DPACKET_HELO& helo)const
{
    // first, check signature
    if(helo.signature[0] != 'D' ||
       helo.signature[1] != 'S' ||
       helo.signature[2] != 'T' ||
       helo.signature[3] != 'M')
        {
        // signature check failed
        LOG_FUNC << "signature check failed\n";
        return(false);
        }
    
    // check version
    if(helo.version_no != 1)
        {
        LOG_FUNC << "version check failed: expected 1, got " << helo.version_no << "\n";
        return(false);
        }
    
    // ignore the authentication stuff
    
    // done
    return(true);
} // end ValidateHelo
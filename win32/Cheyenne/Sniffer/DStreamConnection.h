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

#pragma pack(pop,ds)

}; // end namespace dstream
class DStreamConnection : public Thread
{
public:
    typedef std::map<dstream::uint32,DAOCConnection*> sniff_map_type;
    typedef sniff_map_type::value_type sniff_map_value_type;
    typedef sniff_map_type::iterator sniff_map_iterator;
    typedef sniff_map_type::const_iterator sniff_map_const_iterator;
    typedef sniff_map_type::value_type sniff_map_value;
    typedef std::pair<sniff_map_iterator,bool> sniff_map_insert_result;

    DStreamConnection();
    virtual ~DStreamConnection();
    
    bool IsConnected(void)const{return(!(GetSocket()==INVALID_SOCKET));};
    std::string GetStatusString(void)const{return(IsConnected()?"connected":"disconnected");};
    sniff_map_type::size_type GetNumConnections(void)const{return(SniffMap.size());};
    
    bool Open(const std::string& remote_addr,const std::string& remote_port);
    // this one takes "C" arguments to make it simple :/
    // all parameters are in HOST order
    bool Open(const char* remote_addr,unsigned short remote_port);
    bool Open(SOCKET s);
    void Close(void);
    
protected:
    virtual void ProcessInput(buffer_space::Buffer& Input);

private:
    virtual DWORD Run(const bool& bContinue); // implement this and look for bContinue==false to terminate

    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        );

    bool DoOutputMaintenance(void);
    bool DoInputMaintenance(void);
    
    DAOCConnection* AddConnection(const dstream::uint32 id);
    DAOCConnection* FindConnection(const dstream::uint32 id)const;
    void RemoveConnection(const dstream::uint32 id);

    bool ValidateHelo(const dstream::DPACKET_HELO& helo)const;
    bool bHeloValidated;
    
    DECL_MEMBER_PRIVATE(SOCKET,Socket);
    DECL_MEMBER_VIEW(SOCKADDR_IN,LocalAddr);
    DECL_MEMBER_VIEW(SOCKADDR_IN,RemoteAddr);
    DECL_MEMBER_PRIVATE(buffer_space::Buffer,InputBuffer);
    DECL_MEMBER_PRIVATE(buffer_space::Buffer,OutputBuffer);

    // read variables
    OVERLAPPED ReadOverlapped;
    static const unsigned int TempBufferSize;
    unsigned char* ReadBuf; // only accessed from the DoInputMaintenance and read completion
                        // functions, but must be a non-static class member

    // the sniff-map
    sniff_map_type SniffMap;
}; // end class DStreamConnection
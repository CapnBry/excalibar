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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include "..\Sharenet\SharenetAPI.h" // for base class
#include "..\Utils\threads.h"
#include "..\Utils\tsdeque.h"
#include "..\Utils\buffer.h"
#include "..\Utils\times.h"
#include "..\Utils\cheyennemessages.h"
#include "..\Utils\CodeUtils.h"

const CheyenneTime MaxPingAge(30.0);
const CheyenneTime PingInterval(20.0);

class ShareNetClientData : public SharenetAPI
{
public:
    ShareNetClientData()
    {
        MessageOutputFifo=NULL;
        SetSocket(INVALID_SOCKET);
        ZeroMemory(&ModifyRemoteAddr(),sizeof(SOCKADDR_IN));
        ZeroMemory(&ModifyLocalAddr(),sizeof(SOCKADDR_IN));

        WSADATA wsa;
        WSAStartup(MAKEWORD(2,0),&wsa);
    
    }

    ~ShareNetClientData()
    {
        Close();
        WSACleanup();
    }

    bool IsInUse(void)const
    {
        return(GetSocket() == INVALID_SOCKET ? false:true);
    }
    
    std::string GetStatusString(void)const
    {
        if(IsInUse())
            {
            return(std::string("connected"));
            }
        else
            {
            return(std::string("disconnected"));
            }
    }

    bool QueueOutputMessage(const void* data,const unsigned short length);

    bool Open(const std::string& remote_addr,const std::string& remote_port);
    // this one takes "C" arguments to make it simple :/
    // all parameters are in HOST order
    bool Open(const char* remote_addr,unsigned short remote_port);
    bool Open(SOCKET s);

    void Close(void);
    
protected:
private:
    int GetLastInputError(void)const{return(GetInputBuffer().GetLastError());};
    int GetLastOutputError(void)const{return(GetOutputBuffer().GetLastError());};

    //unsigned int ExtractInputMessage(void* data,const unsigned int max_len);

    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        );

    bool DoOutputMaintenance(void);
    bool DoInputMaintenance(void);

    virtual DWORD Run(const bool& bContinue);
    void SavePingTime(void);
    void SendPing(void);
    
    void BuildShareMessage(const unsigned char* buf, const unsigned short len);

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
    tsfifo<CheyenneMessage*>* MessageOutputFifo;
    
    // read data
    OVERLAPPED ReadOverlapped;
    char ReadBuf[TempBufferSize]; // only accessed from the DoInputMaintenance and read completion
                        // functions, but must be a non-static class member

}; // end class ShareNetClientData

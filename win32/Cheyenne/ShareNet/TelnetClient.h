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

class TelnetClientData : public SharenetAPI
{
public:
    TelnetClientData()
    {
        MessageOutputFifo=NULL;
        SetSocket(INVALID_SOCKET);
        ZeroMemory(&ModifyRemoteAddr(),sizeof(SOCKADDR_IN));
        ZeroMemory(&ModifyLocalAddr(),sizeof(SOCKADDR_IN));

        WSADATA wsa;
        WSAStartup(MAKEWORD(2,0),&wsa);
    
    }

    ~TelnetClientData()
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

    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        );

    bool DoOutputMaintenance(void);
    bool DoInputMaintenance(void);

    virtual DWORD Run(const bool& bContinue);

    TelnetClientData(const TelnetClientData& s); // disallow
    TelnetClientData& operator=(const TelnetClientData& s); // disallow

    DECL_MEMBER(SOCKET,Socket);
    DECL_MEMBER(SOCKADDR_IN,LocalAddr);
    DECL_MEMBER(SOCKADDR_IN,RemoteAddr);
    DECL_MEMBER(buffer_space::Buffer,InputBuffer);
    DECL_MEMBER(buffer_space::Buffer,OutputBuffer);

    mutable MutexLock MyLock;
    tsfifo<std::string*>* MessageOutputFifo; // output NOT to network
    tsfifo<std::string*>* MessageInputFifo; // input NOT from network
    
    // read data
    OVERLAPPED ReadOverlapped;
    char ReadBuf[TempBufferSize]; // only accessed from the DoInputMaintenance and read completion
                        // functions, but must be a non-static class member

}; // end class TelnetClientData

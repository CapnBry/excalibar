/******************************************************************************
PCAPDStream: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the PCAPDStream Developers

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
#include "threads.h" // for base class
#include "buffer.h" // for buffer defs

struct DSERVER_CLIENT_PARAMS
{
    DSERVER_CLIENT_PARAMS(){s=INVALID_SOCKET;ZeroMemory(&addr,sizeof(addr));};
    SOCKET s;
    struct sockaddr addr;
};
class DServerClientConnection : public Thread<DSERVER_CLIENT_PARAMS>
{
public:
    DServerClientConnection();
    virtual ~DServerClientConnection();
    
    void Close(void);
    inline bool IsInUse(void)const{return(GoParam.s == INVALID_SOCKET ? false:true);};
    bool SendData(const void* data,const size_t num_bytes);

protected:
private:
    void CloseSocket(void);
    virtual DWORD Run(const bool& bContinue);
    bool DoInputMaintenance(void);
    bool DoOutputMaintenance(void);
    static VOID CALLBACK ReadCompletionRoutine
        (
        DWORD dwErrorCode,
        DWORD dwNumberOfBytesTransfered,
        OVERLAPPED* lpOverlapped
        );

    DServerClientConnection(const DServerClientConnection& s); // disallow
    DServerClientConnection& operator=(const DServerClientConnection& s); // disallow

    // this is needed for ReadFileEx
    OVERLAPPED ReadOverlapped;
    
    // output buffer
    Buffer OutputBuffer;
    
    BYTE ReadBuf[2048];
}; // end class DServerClientConnection

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
#include <winsock2.h>
#include <windows.h>

class ShareNet : public Thread
{
public:
    ShareNet();
    virtual ~ShareNet();

    void InitializeNet(const char* remote_machine, const unsigned short& remote_port);
    void ShutdownNet(void);

    enum State_t
    {
    NotInitialized,
    Initialized,
    Connecting,
    Connected,
    Disconnecting,
    Disconnected
    };


protected:
private:
    virtual DWORD Run(const bool& bContinue); 
    
    ShareNet(const ShareNet& s); // disallow
    ShareNet& operator=(const ShareNet& s); // disallow

    State_t State;
    unsigned int MyIdOffset;
    SOCKET Socket;
    SOCKADDR_IN RemoteAddr;

    MutexLock ShareMutex;
}; // end class ShareNet
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
#include <winsock2.h> // for socket defs
#include <string> // for string defs
#include "..\Utils\threads.h" // for thread defs

const unsigned int TempBufferSize(2048);

class SharenetAPI : public Thread
{
public:
    SharenetAPI(){};
    virtual ~SharenetAPI(){};
    
    virtual bool IsInUse(void)const=0;
    virtual std::string GetStatusString(void)const=0;
    virtual bool QueueOutputMessage(const void* data,const unsigned short length)=0;

    virtual bool Open(const std::string& remote_addr,const std::string& remote_port)=0;
    // this one takes "C" arguments to make it simple :/
    // all parameters are in HOST order
    virtual bool Open(const char* remote_addr,unsigned short remote_port)=0;
    virtual bool Open(SOCKET s)=0;

    virtual void Close(void)=0;

protected:
private:
    SharenetAPI(const SharenetAPI& s); // disallow
    SharenetAPI& operator=(const SharenetAPI& s); // disallow
};
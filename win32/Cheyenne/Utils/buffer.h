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

// for mutex functions
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include "..\Utils\locks.h"
#include "..\Utils\signals.h"

class AlertableWaitSingleFunctor
{
public:
    typedef DWORD return_type;
    DWORD operator()(HANDLE hEvent,unsigned int timeout_ms) const
    {
        return(WaitForSingleObjectEx(hEvent,timeout_ms,TRUE));
    }
}; // end class AlertableWaitSingleFunctor

class WaitSingleFunctor
{
public:
    typedef DWORD return_type;
    DWORD operator()(HANDLE hEvent,unsigned int timeout_ms) const
    {
        return(WaitForSingleObject(hEvent,timeout_ms));
    }
}; // end class WaitSingleFunctor

namespace buffer_space
{
// buffer size
const int buffer_size=65536; // 64k, max udp datagram on most networks
const int no_err=0;
const int overflow=1;
const int underflow=2;

class Buffer
{
public:
    Buffer();
    Buffer(const buffer_space::Buffer& s);
    virtual ~Buffer();
    
    bool Wait(unsigned int timeout_ms=1000)const
    {
        return(signal.wait(timeout_ms));
    }

    template<typename WAIT_FUNCTOR> typename WAIT_FUNCTOR::return_type Wait(WAIT_FUNCTOR func,unsigned int timeout_ms)const
    {
        return(signal.wait(func,timeout_ms));
    }

    unsigned int Size(void)const{return(size);};
    unsigned int MaxSize(void)const{return(sizeof(buf));};
    unsigned int AvailableSize(void)const{return(MaxSize()-Size());};
    bool Peek(void* data,const unsigned int length);
    bool Insert(const void* data,const unsigned int length);
    bool Extract(void* data,const unsigned int length);

    void Flush(void){AutoLock al(&l); write=0;read=0;size=0;};

    int GetLastError(void)const{return(last_error);};

    buffer_space::Buffer& operator=(const buffer_space::Buffer& s);
    
    EventSignal CopySignal(void)const{return(EventSignal(signal));};
    EventSignal* GetSignalPointer(void)const{return(&signal);};
    EventSignal& GetSignalReference(void)const{return(signal);};

    bool Lock(void){return(l.lock());};
    bool Unlock(void){return(l.unlock());};

protected:

private:
    void set(const buffer_space::Buffer& s);
    void InsertWrapped(const void* data,const unsigned int length);
    void InsertUnwrapped(const void* data,const unsigned int length);
    void ExtractWrapped(void* data,const unsigned int length);
    void ExtractUnwrapped(void* data,const unsigned int length);

    int last_error; // only valid if a function returns false
    unsigned int write; // write index
    unsigned int read; // read index
    unsigned int size; // total size stored in this buffer
    unsigned char buf[buffer_space::buffer_size]; // the buffer

    // lock
    MutexLock l;
    
    // signal
    mutable EventSignal signal;

}; // end class Buffer

} // end namespace buffer_space

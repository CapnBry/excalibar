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
#ifndef LOCKS_H
#define LOCKS_H

#pragma once
#include <windows.h>
#include <stdexcept>

class Lock
{
public:
    Lock(){};
    virtual ~Lock(){};

    virtual bool lock(unsigned int timeout_ms=1000)=0;
    virtual bool unlock(void)=0;

protected:

private:
}; // end class Lock

class MutexLock : public Lock
{
public:
    MutexLock() : 
        hMutex(CreateMutex(NULL,FALSE,NULL))
    {
    };

    MutexLock(const MutexLock& s) 
        
    {
        DuplicateHandle
            (
            GetCurrentProcess(),
            s.hMutex,GetCurrentProcess(),
            &hMutex,
            THREAD_ALL_ACCESS,
            TRUE,
            DUPLICATE_SAME_ACCESS
            );
    };

    virtual ~MutexLock(){CloseHandle(hMutex);};

    virtual bool lock(unsigned int timeout_ms=1000)
    {
        switch(WaitForSingleObject(hMutex,timeout_ms))
            {
            case WAIT_OBJECT_0:
            case WAIT_ABANDONED: // abandoned is okay, we still own the mutex
                return(true);

            default:          
                return(false);
            }
    } // end lock

    virtual bool unlock(void)
    {
        ReleaseMutex(hMutex);
        return(true);
    } // end unlock


protected:

private:
    HANDLE hMutex;
}; // end class MutexLock

class NoLock : public Lock
{
public:
    NoLock(){};
    NoLock(const NoLock& s){};
    virtual ~NoLock(){};

    virtual bool lock(unsigned int timeout_ms=1000)
    {
    return(true);
    } // end lock

    virtual bool unlock(void)
    {
        return(true);
    } // end unlock


protected:
private:
}; // end class NoLock

class CriticalSectionLock : public Lock
{
public:
    CriticalSectionLock()
    {
        InitializeCriticalSection(&cs);
    }

    CriticalSectionLock(const CriticalSectionLock& s) throw()
    {
        std::logic_error oops("no critical section copy constructor can exist");
        throw oops;
    }
    virtual ~CriticalSectionLock()
    {
        DeleteCriticalSection(&cs);
    }

    virtual bool lock(unsigned int timeout_ms=1000)
    {
        EnterCriticalSection(&cs);
        return(true);
    } // end lock

    virtual bool unlock(void)
    {
        LeaveCriticalSection(&cs);
        return(true);
    } // end unlock


protected:

private:
    CRITICAL_SECTION cs;
}; // end class CriticalSectionLock

class AutoLock
{
public:
    AutoLock(Lock* l=NULL,unsigned int timeout_ms=1000)
    {
        my_lock=l;
        my_lock->lock(timeout_ms);
    }

    AutoLock(Lock& l,unsigned int timeout_ms=1000)
    {
        my_lock=&l;
        my_lock->lock(timeout_ms);
    }

    ~AutoLock()
    {
        my_lock->unlock();
    }

protected:

private:
    Lock* my_lock;
}; // end class AutoLock

#endif //LOCKS_H

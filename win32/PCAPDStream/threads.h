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
#ifndef THREADS_H
#define THREADS_H

#pragma once

// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include <assert.h>
#include <crtdbg.h>
#include <stdexcept>
#include <iostream>

template<typename THREAD_PARAM_T>class Thread
{
public:
    Thread()
    {
        hThread=NULL;
        dwThreadID=0;
        bContinue=false;
        bRunning=false;
    };

    virtual ~Thread(){Stop();};

    bool IsRunning(void){return(bRunning);};

    bool Go(THREAD_PARAM_T p)
    {
        if(bRunning)
            {
            return(false);
            }

        // store this
        GoParam=p;

        // set flags
        bRunning=true;
        bContinue=true;

        // start thread
        hThread=CreateThread
            (
            NULL,0,
            (LPTHREAD_START_ROUTINE)ThreadFunc,
            this,
            0,
            &dwThreadID
            );

        // oops check
        if(!hThread || hThread==INVALID_HANDLE_VALUE)
            {
            hThread=NULL;

            // clear flags
            bRunning=false;
            bContinue=false;

            // done
            return(false);
            }

        // done with this
        CloseHandle(hThread);
        
        // done
        return(true);
    };
    
    void AsynchronousStop(void)
    {
        // clear flag (thread will detect this and exit)
        bContinue=false;
    }
    void Stop(void)
    {
        // clear flag (thread will detect this and exit)
        bContinue=false;

        if(bRunning)
            {
            // wait for thread to exit
            for(int i=0;i<50;++i)
                {
                // check flag
                if(!bRunning)
                    {
                    break;
                    }
        
                // sleep a bit
                Sleep(100);
                }
            }

        // if thread is still running,
        // forcibly terminate it

        if(bRunning)
            {
            bRunning=false;
            TerminateThread(hThread,-1);
            }

        // done
        return;
    };

protected:
    THREAD_PARAM_T GoParam; // for clients to use

private:

    // thread related stuff
    static DWORD WINAPI ThreadFunc(PVOID context)
    {
        Thread* pMe=static_cast<Thread*>(context);

        _ASSERTE(pMe != NULL);

        try
            {
            return(pMe->RunInternal(pMe->bContinue));
            }
        catch(std::exception& e)
            {
            // exception caught, terminate program
            std::cerr << "caught exception " << e.what() << " in thread id " << pMe->dwThreadID << std::endl;
            std::cerr << "type: " << typeid(e).name() << std::endl;
            
            // rethrow
            throw;
            }
    };

    DWORD RunInternal(const bool& bContinue)
    {
        DWORD ret=Run(bContinue);
        bRunning=false;
        return(ret);
    }

    virtual DWORD Run(const bool& bContinue)=0; // implement this and look for bContinue==false to terminate
    
    HANDLE hThread;
    DWORD dwThreadID;
    bool bContinue;
    bool bRunning; // clear this flag on exit from the Run() function

}; // end class Thread

#endif // THREADS_H
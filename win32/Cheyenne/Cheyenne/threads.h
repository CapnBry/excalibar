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

#include <windows.h>
#include <assert.h>
#include <crtdbg.h>

class Thread
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

    bool Go(void* p=NULL)
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
    void* GoParam;

private:

    // thread related stuff
    static DWORD WINAPI ThreadFunc(PVOID context)
    {
        Thread* pMe=static_cast<Thread*>(context);

        _ASSERTE(pMe != NULL);

        return(pMe->RunInternal(pMe->bContinue));
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
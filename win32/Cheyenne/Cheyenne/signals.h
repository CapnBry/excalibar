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
#ifndef SIGNALS_H
#define SIGNALS_H

#pragma once
#include <windows.h>
#include <list> // included for MultiWaitSignal

class EventSignal
{
friend class MultiWaitSignal;
public:
    EventSignal() : 
        hEvent(CreateEvent(NULL,TRUE,FALSE,NULL))
    {
    };

    EventSignal(const EventSignal& s) 
    {
        DuplicateHandle
            (
            GetCurrentProcess(),
            s.hEvent,GetCurrentProcess(),
            &hEvent,
            THREAD_ALL_ACCESS,
            TRUE,
            DUPLICATE_SAME_ACCESS
            );
    };

    virtual ~EventSignal(){CloseHandle(hEvent);};

    bool wait(unsigned int timeout_ms=1000)const
    {
        switch(WaitForSingleObject(hEvent,timeout_ms))
            {
            case WAIT_OBJECT_0:
                return(true);

            default:          
                return(false);
            }
    } // end lock

    bool signal(void)
    {
        ::SetEvent(hEvent);
        return(true);
    } // end signal

    void reset(void)
    {
        ::ResetEvent(hEvent);
        return;
    }

    bool testwait(void)const
    {
        return(wait(0));
    }


protected:

private:
    HANDLE hEvent;
}; // end class EventSignal

class PulsedEventSignal
{
friend class MultiWaitSignal;
public:
    PulsedEventSignal() : 
        hEvent(CreateEvent(NULL,TRUE,FALSE,NULL))
    {
    };

    PulsedEventSignal(const PulsedEventSignal& s) 
    {
        DuplicateHandle
            (
            GetCurrentProcess(),
            s.hEvent,GetCurrentProcess(),
            &hEvent,
            THREAD_ALL_ACCESS,
            TRUE,
            DUPLICATE_SAME_ACCESS
            );
    };

    virtual ~PulsedEventSignal(){CloseHandle(hEvent);};

    bool wait(unsigned int timeout_ms=1000)const
    {
        switch(WaitForSingleObject(hEvent,timeout_ms))
            {
            case WAIT_OBJECT_0:
                return(true);

            default:          
                return(false);
            }
    } // end lock

    bool signal(void)
    {
        ::PulseEvent(hEvent);
        return(true);
    } // end signal

    void reset(void)
    {
        ::ResetEvent(hEvent);
        return;
    }

    bool testwait(void)const
    {
        return(wait(0));
    }


protected:

private:
    HANDLE hEvent;
}; // end class PulsedEventSignal

class MultiWaitSignal
{
public:
    MultiWaitSignal(){};
    virtual ~MultiWaitSignal(){};

    bool WaitAll(unsigned int timeout=1000)
    {
        // build handle array
        HANDLE* events=new HANDLE[signal_list.size()];
        signal_list_iterator it=signal_list.begin();
        int i=0;

        while(it != signal_list.end())
            {
            events[i]=(*it)->hEvent;
            ++i;
            ++it;
            }

        switch(WaitForMultipleObjects(signal_list.size(),events,TRUE,timeout))
            {
            case WAIT_TIMEOUT:
            case WAIT_FAILED:
                delete events;
                return(false);
                break;

            default:
                delete events;
                return(true);
                break;
            }
    }

    EventSignal* WaitAny(unsigned int timeout=1000)
    {
        // build handle array
        HANDLE* events=new HANDLE[signal_list.size()];
        HANDLE the_event;
        signal_list_iterator it=signal_list.begin();
        int i=0;
        DWORD result;

        while(it != signal_list.end())
            {
            events[i]=(*it)->hEvent;
            ++i;
            ++it;
            }

        switch(result=WaitForMultipleObjects(signal_list.size(),events,FALSE,timeout))
            {
            case WAIT_TIMEOUT:
            case WAIT_FAILED:
                delete events;
                return(NULL);
                break;

            default:
                if(result >= WAIT_OBJECT_0 && result <= WAIT_OBJECT_0+signal_list.size()-1)
                    {
                    // wait satisfied by event at index result-WAIT_OBJECT_0
                    the_event=events[result-WAIT_OBJECT_0];
                    delete events;

                    }
                else
                    {
                    // wait satisfied by an abandonment at index result-WAIT_ABANDONED_0
                    the_event=events[result-WAIT_ABANDONED_0];
                    delete events;

                    }
                return(GetSignalFromList(the_event));
                break;
            }
    }

    EventSignal* GetSignalFromList(HANDLE hEvent)
    {
        signal_list_iterator it=signal_list.begin();

        while(it != signal_list.end())
            {
            if((*it)->hEvent==hEvent)
                {
                return(*it);
                }
            ++it;
            }
        
        return(NULL);
    }
    void AddSignalToList(EventSignal& s)
    {
        if(IsSignalInList(s))
            {
            }
        else
            {
            signal_list.insert(signal_list.begin(),&s);
            }
        
        return;
    }

    void EmptySignalList(void)
    {
        signal_list.erase(signal_list.begin(),signal_list.end());
        return;
    }

    bool IsSignalInList(EventSignal& s)
    {
        signal_list_iterator it=signal_list.begin();

        while(it != signal_list.end())
            {
            if(*it==&s)
                {
                return(true);
                }
            ++it;
            }
        
        return(false);
    }

protected:

private:
    MultiWaitSignal(const MultiWaitSignal& s); // no copy constructor
    MultiWaitSignal& operator=(const MultiWaitSignal& s); // no assignment operator

    typedef std::list<EventSignal*>::iterator signal_list_iterator;
    std::list<EventSignal*> signal_list;
}; // end class MultiWaitSignal

class AsyncWindowSignal
{
public:
    AsyncWindowSignal(HWND h=NULL,UINT m=0, WPARAM w=0, LPARAM l=0)
    {
        hWnd=h;
        uMsg=m;
        wParam=w;
        lParam=l;
    }

    AsyncWindowSignal(const AsyncWindowSignal& s){set(s);};
    AsyncWindowSignal& operator=(const AsyncWindowSignal& s)
    {
        if(this != &s)
            {
            set(s);
            }

        return(*this);
    }

    void signal(void)
    {
        PostMessage(hWnd,uMsg,wParam,lParam);
    }

protected:
private:
    void set(const AsyncWindowSignal& s)
    {
        hWnd=s.hWnd;
        uMsg=s.uMsg;
        wParam=s.wParam;
        lParam=s.lParam;
    }


    HWND hWnd;
    UINT uMsg;
    WPARAM wParam;
    LPARAM lParam;
}; // end class AsyncWindowSignal

#endif //SIGNALS_H
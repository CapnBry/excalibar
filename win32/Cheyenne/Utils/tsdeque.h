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
/******************************************************************************
* FILENAME:     tsdequeue.h
*
* DESCRIPTION:  This is the h file for the std::deque-derived classes
*
* FUNCTIONS:    
*
* MODULE:       
*
* LOG:          03/06/2003, initial
*******************************************************************************/

#ifndef TSDEQUEUE_H
#define TSDEQUEUE_H

#pragma once

#include <deque>
#include "..\Utils\locks.h"
#include "..\Utils\signals.h"

template<class T,class L=MutexLock,class A=std::allocator<T> > class tsdeque : public std::deque<T,A>
{
public:
    explicit tsdeque(const A& al = A()) :
        std::deque<T,A>(al)
    {
    };
    
    explicit tsdeque(size_type n, const T& v = T(), const A& al = A()) : 
        std::deque<T,A>(n,v,al)
    {
    };

    tsdeque(const tsdeque& x) : 
        std::deque<T,A>(static_cast<const std::deque<T,A>&>(x))
    {
    };
        
    tsdeque(const_iterator first, const_iterator last, const A& al = A()) :
        std::deque<T,A>(first, last,al)
    {
    };

    bool lock(unsigned int timeout_ms=1000)
    {
        return(l.lock(timeout_ms));
    }

    bool unlock(void)
    {
        return(l.unlock());
    }

protected:

private:
    L l;
}; // end class tsdeque

template<class T,class L=MutexLock,class A=std::allocator<T> > class tsfifo : public std::deque<T,A>
{
public:
    explicit tsfifo(const A& al = A()) :
        std::deque<T,A>(al)
    {
    };
    
    explicit tsfifo(size_type n, const T& v = T(), const A& al = A()) : 
        std::deque<T,A>(n,v,al)
    {
    };

    tsfifo(const tsfifo& x) : 
        std::deque<T,A>(static_cast<const std::deque<T,A>&>(x))
    {
        if(size()>0)
            {
            // if we have stuff in there, then set the signal
            s.signal();
            }
    };
        
    tsfifo(const_iterator first, const_iterator last, const A& al = A()) :
        std::deque<T,A>(first, last,al)
    {
        if(size()>0)
            {
            // if we have stuff in there, then set the signal
            s.signal();
            }
    };
    ~tsfifo()
    {
        while(begin() != end())
            {
            pop_front();
            }
    }

    T Pop(void)
    {
        lock();

        if(empty())
            {
            unlock();
            return(NULL);
            }
        
        T item=front();
        
        pop_front();

        if(empty())
            {
            s.reset();
            }

        unlock();

        return(item);
    };

    T PopWait(unsigned int timeout_ms=1000)
    {
        if(!s.wait(timeout_ms))
            {
            return(NULL);
            }

        lock();

        if(empty())
            {
            s.reset();
            unlock();
            return(NULL);
            }
        
        T item=front();
        
        pop_front();

        if(empty())
            {
            s.reset();
            }

        unlock();

        return(item);
    };

    void Push(const T& item)
    {
        lock();

        push_back(item);

        s.signal();

        unlock();
    };

protected:

    bool lock(unsigned int timeout_ms=1000)
    {
        return(l.lock(timeout_ms));
    }

    bool unlock(void)
    {
        return(l.unlock());
    }

private:
    L l;
    EventSignal s;
}; // end class tsfifo

#endif // TSDEQUEUE_H
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

#include <iostream>
#include <fstream>
#include "..\Utils\locks.h"

class CheyenneLogger
{
public:
    CheyenneLogger();
    virtual ~CheyenneLogger();
    
    bool Init(const char* const filename);

    template<class arg_t> CheyenneLogger& store(arg_t arg)
    {
        AutoLock al(mutex);

        file << arg;
            
        std::cout << arg;

        return(*this);
    }
protected:
private:
    // prevent these from ever getting called
    CheyenneLogger(const CheyenneLogger& s);
    CheyenneLogger& operator=(const CheyenneLogger& s);

    std::ofstream file;
    MutexLock mutex;
}; // end class CheyenneLogger

class NDebugCheyenneLogger
{
public:
    NDebugCheyenneLogger(){};
    virtual ~NDebugCheyenneLogger(){};

    bool Init(const char* const filename){return(true);};

    template<class arg_t> NDebugCheyenneLogger& store(arg_t arg)
    {
        // no-debug version does nothing
        return(*this);
    }

protected:
private:
    // prevent these from ever getting called
    NDebugCheyenneLogger(const NDebugCheyenneLogger& s);
    NDebugCheyenneLogger& operator=(const NDebugCheyenneLogger& s);
}; // end class NDebugCheyenneLogger

//#ifdef CHEYENNE_DEBUG
    typedef CheyenneLogger logger_t;
//#else
    //typedef NDebugCheyenneLogger logger_t;
//#endif // CHEYENNE_DEBUG

/********************************************************************
*                   CheyenneLogger operators                        *
*********************************************************************/
template<class arg_t> logger_t& operator<<(logger_t& str,arg_t arg)
{
    return(str.store(arg));
}

inline logger_t& operator<<(logger_t& str,std::ostream& (*_Pfn)(std::ostream&))
{
    return(str.store(_Pfn));
}

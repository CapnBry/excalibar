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
#ifndef LOGGER_H
#define LOGGER_H

#pragma once

#include <fstream>
#include "nids.h"
#include "locks.h"

class CheyenneLogger
{
public:
    CheyenneLogger();
    virtual ~CheyenneLogger();

    template<class arg_t> CheyenneLogger& store(const arg_t& arg)
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

    std::fstream file;
    MutexLock mutex;
}; // end class CheyenneLogger

class NDebugCheyenneLogger
{
public:
    NDebugCheyenneLogger(){};
    virtual ~NDebugCheyenneLogger(){};

    template<class arg_t> NDebugCheyenneLogger& store(const arg_t& arg)
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

#ifdef CHEYENNE_DEBUG
    typedef CheyenneLogger logger_t;
#else
    typedef NDebugCheyenneLogger logger_t;
#endif // CHEYENNE_DEBUG

#endif // LOGGER_H
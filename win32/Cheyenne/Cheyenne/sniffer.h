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
#ifndef SNIFFER_H
#define SNIFFER_H
#pragma once


#include <winsock2.h>
#include <windows.h>
#include <map>
#include "locks.h"
#include "daocconnection.h"

class DAOCConnection;

enum SOURCE_TYPE
{
    FromClient,
    FromServer,
    SourceNotFound
};

// predefine this
class Database;

class Sniffer
{
public:
    Sniffer();
    Sniffer(const Sniffer& s);
    virtual ~Sniffer();

    Sniffer& operator=(const Sniffer& s);

    bool Go(tsfifo<CheyenneMessage*>* p);
    void Stop(void);
    bool IsDAOCConnection(const struct tuple4& connection,bool bIgnorePort)const;
    void PrintConnections(void);

protected:

private:

    void NewConnection(const tuple4& key);
    void CloseConnection(const tuple4& key);
    DAOCConnection* GetConnection(const tuple4& key);
    DAOCConnection* GetUDPConnectionByClient(const tuple4& key,SOURCE_TYPE& src);

    void set(const Sniffer& s);

    static DWORD WINAPI ThreadFunc(PVOID context);
    static void tcp_callback (struct tcp_stream *a_tcp, void **this_time_not_needed,void* context);
    static void udp_callback (struct tuple4 * addr, char * buf, int len, struct ip * iph,void* context);

    void HandleTCP(struct tcp_stream *a_tcp, void **this_time_not_needed);
    void HandleUDP(struct tuple4 * addr, char * buf, int len, struct ip * iph);

    DWORD Run(void);

    HANDLE hThread;
    DWORD dwThreadID;
    bool bContinue;
    bool bRunning;

    typedef std::map<tuple4,DAOCConnection*>::iterator sniff_map_iterator;
    typedef std::map<tuple4,DAOCConnection*>::value_type sniff_map_value;
    typedef std::pair<sniff_map_iterator,bool> sniff_map_insert_result;
    std::map<tuple4,DAOCConnection*> sniff_map;
    // connection for handling UDP packets for which there is no
    // corresponding TCP connection
    DAOCConnection* DefaultUDPConnection;

    tsfifo<CheyenneMessage*>* MessageOutputFifo;
}; // end class DAOCConnection


#endif //SNIFFER_H
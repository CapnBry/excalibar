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
    static bool IsDAOCConnection(const struct tuple4& connection,bool bIgnorePort);
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

template<typename STORAGE_T> class PktLoadSniffer : public Thread
{
public:
    PktLoadSniffer() : Storage(std::string("daoc.pkt")){};
    virtual ~PktLoadSniffer(){};

    bool IsDAOCConnection(const struct tuple4& connection,bool bIgnorePort)
    {
        // use static member in Sniffer class
        return(Sniffer::IsDAOCConnection(connection,bIgnorePort));
    }
    void PrintConnections(void){};
protected:
private:
    void ProcessPacket(const typename STORAGE_T::PACKET_T& pkt)
    {
        //::Logger << "[PktLoadSniffer::ProcessPacket] Processing packet, len=" << pkt.GetLen()
                    //<< " time=" << pkt.GetTime() << "\n";

        if(pkt.GetTCP())
            {
            if(pkt.GetFromServer())
                {
                FakeConnection.FromTCPServer(pkt.GetData(),pkt.GetLen());
                } // end if TCP from server
            else
                {
                FakeConnection.FromTCPClient(pkt.GetData(),pkt.GetLen());
                } // end else TCP from client
            } // end if TCP
        else
            {
            if(pkt.GetFromServer())
                {
                FakeConnection.FromUDPServer(pkt.GetData(),pkt.GetLen());
                } // end if UDP from server
            else
                {
                FakeConnection.FromUDPClient(pkt.GetData(),pkt.GetLen());
                } // end else UDP from client
            } // end else UDP
    } // end ProcessPacket
    
    virtual DWORD Run(const bool& bContinue)
    {
        // if the file failed to load, then stop here
        if(!Storage.good())
            {
            return(-1);
            }
        
        // sleep a little bit to let the system init (this is for debugging ;)
        Sleep(10000);
        
        // recover the parameter passed to us
        tsfifo<CheyenneMessage*>* MessageOutputFifo=static_cast<tsfifo<CheyenneMessage*>*>(GoParam);
        
        // start connection
        FakeConnection.Go(MessageOutputFifo);
        
        typename STORAGE_T::PACKET_T pkt;
        
        // get first packet
        Storage >> pkt;
        // difference in time between
        // ::Clock when we started and
        // the timestamp in the first packetdouble TimeDiff; 
        const double TimeDiff = ::Clock.Current().Seconds() - pkt.GetTime();
        
        // process first packet immediately
        ProcessPacket(pkt);
        
        while(bContinue && !Storage.eof())
            {
            // get next packet
            Storage >> pkt;
            
            // see when to process it
            double SleepTime=pkt.GetTime() + TimeDiff - ::Clock.Current().Seconds();
            
            if(SleepTime > 0.010)
                {
                //::Logger << "[PktLoadSniffer::Run] Sleeping " << SleepTime << " seconds for next packet\n";
                Sleep((DWORD)(1000*SleepTime));
                }
            
            // process it
            ProcessPacket(pkt);
            } // end forever
        
        ::Logger << "[PktLoadSniffer::Run] exiting, Storage.eof()=" << Storage.eof() << "\n";
        // stop connection
        FakeConnection.Stop();
        
        return(0);
    } // end Run

    PktLoadSniffer(const PktLoadSniffer& s); // disallow
    PktLoadSniffer& operator=(const PktLoadSniffer& s); // disallow

    STORAGE_T Storage;
    DAOCConnection FakeConnection;
}; // end class PktLoadSniffer

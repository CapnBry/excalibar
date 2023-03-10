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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <assert.h>
#include <crtdbg.h>
#include <iostream>

#include "netinet\in_systm.h"
#include "netinet\ip.h"

#include "sniffer.h"
#include "..\Utils\Logger.h"
#include "..\Utils\times.h"

// these need to be defined before packetstore is included
extern logger_t Logger;
extern CheyenneClock Clock;
#include "..\..\Common\PacketStore\PacketStore.h" // include for offline packet storage

typedef DAOCConnection DAOC_CONNECTION; // use this for a regular connection
//typedef StoringDAOCConnection<PacketStore<std::ofstream> > DAOC_CONNECTION; // use this to store all packets in a file

bool operator==(const tuple4& a,const tuple4& b)
{
    return(memcmp(&a,&b,sizeof(tuple4)) == 0 ? true:false);
} // end operator==tuple4)

bool operator!=(const tuple4& a,const tuple4& b)
{
    return(memcmp(&a,&b,sizeof(tuple4)) == 0 ? false:true);
} // end operator!=tuple4)

bool operator<(const tuple4& a,const tuple4& b)
{
    const unsigned char* pa=reinterpret_cast<const unsigned char*>(&a);
    const unsigned char* pb=reinterpret_cast<const unsigned char*>(&b);

    for(int i=0;i<sizeof(tuple4);++i)
        {
        if(pa[i] < pb[i])
            {
            return(true);
            }
        
        if(pa[i] > pb[i])
            {
            return(false);
            }
        }
    
    return(false);
} // end operator<(tuple4)

bool operator<=(const tuple4& a,const tuple4& b)
{
    return(a==b ? true : a<b);
} // end operator<=(tuple4)

bool operator>(const tuple4& a,const tuple4& b)
{
    const unsigned char* pa=reinterpret_cast<const unsigned char*>(&a);
    const unsigned char* pb=reinterpret_cast<const unsigned char*>(&b);

    for(int i=0;i<sizeof(tuple4);++i)
        {
        if(pa[i] > pb[i])
            {
            return(true);
            }
        
        if(pa[i] < pb[i])
            {
            return(false);
            }
        }
    
    return(false);
} // end operator>(tuple4)

bool operator>=(const tuple4& a,const tuple4& b)
{
    return(a==b ? true : a>b);
} // end operator>=(tuple4)

/********************************************************************
*                    std::ostream operators                         *
*********************************************************************/
std::ostream& operator<< (std::ostream& str,const tuple4& a)
{
    char src[16];
    char dst[16];

    strcpy(src,inet_ntoa(*reinterpret_cast<const in_addr*>(&a.saddr)));
    strcpy(dst,inet_ntoa(*reinterpret_cast<const in_addr*>(&a.daddr)));

    str << src << 
        ":" <<
        a.source <<
        " " <<
        dst <<
        ":" <<
        a.dest;

    return(str);
} // end operator<< (tuple4)

Sniffer::Sniffer()
{
    hThread=NULL;
    dwThreadID=0;
    bContinue=false;
    bRunning=false;
    MessageOutputFifo=NULL;
    DefaultUDPConnection=new DAOCConnection;

} // end Sniffer

Sniffer::Sniffer(const Sniffer& s)
{
    set(s);
} // end Sniffer(Sniffer&)

Sniffer::~Sniffer()
{
    Stop();
    delete DefaultUDPConnection;
} // end ~Sniffer

Sniffer& Sniffer::operator=(const Sniffer& s)
{
    // check for self-assignment
    if(this != &s)
        {
        set(s);
        }

    return(*this);
} // end operator=(Sniffer&)

bool Sniffer::Go(tsfifo<CheyenneMessage*>* p)
{
    /*
    Lock rules: if we are going to use
    returned objects from the database,
    we must hold the lock until we are
    done using the returned object
    */

    if(bRunning)
        {
        Logger << "[Sniffer::Go] thread already running!" << "\n";

        return(false);
        }

    // save pointers
    MessageOutputFifo=p;

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

    Logger << "[Sniffer::Go] thread id " << unsigned int(dwThreadID) << " created\n";

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
} // end Go

void Sniffer::Stop(void)
{
    // clear flag (thread will detect this and exit)
    bContinue=false;

    if(bRunning)
        {
        // wait for thread to exit
        for(int i=0;i<10;++i)
            {
            // check flag
            if(!bRunning)
                {
                Logger << "[Sniffer::Stop] thread terminated gracefully.\n";
                break;
                }
        
            // sleep a bit
            Sleep(100);
            }

        // iterate through connections and delete them
        while(sniff_map.begin() != sniff_map.end())
            {
            // close the first connection
            CloseConnection(sniff_map.begin()->first);
            } // end for each connection still in the map
        }
    else
        {
        Logger << "[Sniffer::Stop] thread has already terminated gracefully.\n";
        }

    // if thread is still running,
    // forcibly terminate it

    if(bRunning)
        {
        Logger << "[Sniffer::Stop] thread failed to terminate, forcibly terminating it.\n";
        bRunning=false;
        TerminateThread(hThread,-1);
        }

    // done
    return;
} // end Stop

void Sniffer::set(const Sniffer& s)
{
    // assign
    hThread=s.hThread;
    dwThreadID=s.dwThreadID;
    bContinue=s.bContinue;
    bRunning=s.bRunning;
    MessageOutputFifo=s.MessageOutputFifo;

    // done
    return;
} // end set

DWORD WINAPI Sniffer::ThreadFunc(PVOID context)
{
    Sniffer* pMe=static_cast<Sniffer*>(context);

    _ASSERTE(pMe != NULL);

    try
        {
        return(pMe->Run());
        }
    catch(std::exception& e)
        {
        // exception caught, terminate program
        ::Logger << "[Sniffer::ThreadFunc] caught exception " << e.what() << "\n";

        std::cerr << "[Sniffer::ThreadFunc] caught exception " << e.what() << std::endl;
        std::cerr << "type: " << typeid(e).name() << std::endl;
        
        // rethrow
        throw;
        }
} // end ThreadFunc

void Sniffer::HandleTCP(struct tcp_stream *a_tcp, void **this_time_not_needed)
{
    switch(a_tcp->nids_state)
        {
        case NIDS_JUST_EST:
            // connection described by a_tcp is established
            
            // is this a connection we are interested in
            if(IsDAOCConnection(a_tcp->addr,false))
                {
                a_tcp->client.collect++; // we want data received by a client
                a_tcp->client.collect_urg++; // if we don't increase this value,
                                         // we won't be notified of urgent data
                                         // arrival

                a_tcp->server.collect++; // and by a server, too
                a_tcp->server.collect_urg++; // we want urgent data received by a
                                         // server

                // tell database to add new daocconnection
                NewConnection(a_tcp->addr);
                }
            else
                {
                ::Logger << "[Sniffer::HandleTCP] " << a_tcp->addr << " is not a DAoC connection\n";
                }
            break;

        case NIDS_CLOSE:
            // connection has been closed normally
            CloseConnection(a_tcp->addr);
            break;

        case NIDS_RESET:
            // connection has been closed by RST
            CloseConnection(a_tcp->addr);
            break;

        case NIDS_DATA:
            {
            // new data has arrived; gotta determine in what direction
            // get connection associated with this stream
            DAOCConnection* connection=GetConnection(a_tcp->addr);

            if(!connection)
                {
                // have data, but no connection to represent it?
                // hmmm
                Logger << "[Sniffer::HandleTCP] got data, but unable to find connection to handle it!\n";

                break;
                }

            // We don't have to check if urgent data to client has arrived,
            // because we haven't increased a_tcp->client.collect_urg variable.
            // So, we have some normal data to take care of.
            if (a_tcp->client.count_new || a_tcp->client.count_new_urg)
                {
                // new data for client
                connection->FromTCPServer((unsigned char*)a_tcp->client.data,a_tcp->client.count_new,MessageOutputFifo);
                }
            else
                {
                // new data for server
                connection->FromTCPClient((unsigned char*)a_tcp->server.data,a_tcp->server.count_new,MessageOutputFifo);
                }
            } // end if NIDS_DATA
            break;
        
        default:
            break;
        } // end switch state

    // done
    return;
} // end HandleTCP

void Sniffer::HandleUDP(struct tuple4 * addr, char * buf, int len, struct ip * iph)
{
    if(!IsDAOCConnection(*addr,true))
        {
        // not daoc, ignore
        return;
        }

    // get connection
    SOURCE_TYPE src;

    DAOCConnection* connection=GetUDPConnectionByClient(*addr,src);

    //Logger << "[Sniffer::HandleUDP] got packet on: " << *addr << "\n";

    if(src==FromClient)
        {
        // from client to server
        connection->FromUDPClient((unsigned char*)buf,len,MessageOutputFifo);
        }
    else if(src==FromServer)
        {
        // from server to client
        connection->FromUDPServer((unsigned char*)buf,len,MessageOutputFifo);
        }

    // done
    return;
} // end HandleUDP

void Sniffer::tcp_callback (struct tcp_stream *a_tcp, void **this_time_not_needed,void* context)
{
    // alias pointer -- we stored the context when the callback was registered
    Sniffer* pMe=static_cast<Sniffer*>(context);

    _ASSERTE(pMe != NULL);

    // call "real" handler
    pMe->HandleTCP(a_tcp,this_time_not_needed);

    // done
    return;
} // end tcp_callback

void Sniffer::udp_callback (struct tuple4 * addr, char * buf, int len, struct ip * iph,void* context)
{
    // alias pointer -- we stored the context when the callback was registered
    Sniffer* pMe=static_cast<Sniffer*>(context);

    _ASSERTE(pMe != NULL);

    // call "real" handler
    pMe->HandleUDP(addr,buf,len,iph);

    // done
    return;
} // end udp_callback

DWORD Sniffer::Run(void)
{
    // raise priority a little bit
    SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_ABOVE_NORMAL);

    // set flag
    bRunning=true;

    Logger << "[Sniffer::Run] beginning execution.\n";

    if (!nids_init())
        {
        Logger << "[Sniffer::Run] nids_init() failed! Exiting thread: " << &nids_errbuf[0] << "\n";
        bRunning=false;
        return(-1);
        }	

    int time = 0;
    fd_set rset;
    struct timeval tv;

    nids_register_tcp_param(tcp_callback,this);
    nids_register_udp_param(udp_callback,this);

    int fd = nids_getfd();

    while(bContinue)
        {
        tv.tv_sec = 0;
        tv.tv_usec = 500000; // 500ms
        FD_ZERO (&rset);
        FD_SET (fd, &rset);
        // add any other fd we need to take care of
        if (select (fd + 1, &rset, 0, 0, &tv))
            {
            if (FD_ISSET(fd,&rset))  // need to test it if there are other
                {
                // fd in rset
                nids_next();
                /*
                if (!nids_next ())
                    {
                    Logger << "[Sniffer::Run] nids next returned 0\n";
                    }
                */
                }
            }
        } // end forever
    
    Logger << "[Sniffer::Run] exiting thread.\n";

    // clear flag
    bRunning=false;

    return(0);
} // end Run

bool Sniffer::IsDAOCConnection(const struct tuple4& connection,bool bIgnorePort)const
{
    in_addr mask[3];
    mask[0].S_un.S_un_b.s_b1=208;
    mask[0].S_un.S_un_b.s_b2=254;
    mask[0].S_un.S_un_b.s_b3=16;
    mask[0].S_un.S_un_b.s_b4=0;

    mask[1].S_un.S_un_b.s_b1=193;
    mask[1].S_un.S_un_b.s_b2=252;
    mask[1].S_un.S_un_b.s_b3=123;
    mask[1].S_un.S_un_b.s_b4=0;

    mask[2].S_un.S_un_b.s_b1=209;
    mask[2].S_un.S_un_b.s_b2=225;
    mask[2].S_un.S_un_b.s_b3=26;
    mask[2].S_un.S_un_b.s_b4=0;

    unsigned int net[3]={0x00FFFFFF,0x00FFFFFF,0x0FFFFFFF};

    const unsigned short port=10622;

    for(int i=0;i<3;++i)
        {
        if((connection.saddr & net[i]) == mask[i].S_un.S_addr)
            {
            if(bIgnorePort)
                {
                return(true);
                }
            else if(connection.source==port)
                {
                // match
                return(true);
                }
            }

        if((connection.daddr & net[i]) == mask[i].S_un.S_addr)
            {
            if(bIgnorePort)
                {
                return(true);
                }
            else if(connection.dest==port)
                {
                // match
                return(true);
                }
            }
        } // end for each mask

    /*
    const in_addr* temp=reinterpret_cast<const in_addr*>(&connection.daddr);

    if(temp->S_un.S_un_b.s_b1==mask[0].S_un.S_un_b.s_b1 &&
       temp->S_un.S_un_b.s_b2==mask[0].S_un.S_un_b.s_b2 &&
       temp->S_un.S_un_b.s_b3==mask[0].S_un.S_un_b.s_b3)
        {
        in_addr final;
        final.S_un.S_addr = connection.daddr & net[0];
        }
    */
    
    //Logger << "[Sniffer::IsDAOCConnection] rejected connection: " << connection << "\n";

    /*
    208.254.16.55 == Galahad
    */

    // done
    return(false);
} // end IsDAOCConnection

void Sniffer::PrintConnections(void)const
{
    Logger << "[Sniffer::PrintConnections] active connections:\n";

    // iterate through connections and delete them
    for(sniff_map_const_iterator it=sniff_map.begin(); it != sniff_map.end();++it)
        {
        // print the first connection
        Logger << it->first << "\n";
        } // end for each connection still in the map

    Logger << "[Sniffer::PrintConnections] end connections\n";

    // done
    return;
} // end PrintConnections()

void Sniffer::PrintConnections(std::ostream& str)const
{
    // iterate through connections and delete them
    for(sniff_map_const_iterator it=sniff_map.begin(); it != sniff_map.end();++it)
        {
        // print the first connection
        str << it->first << "\n";
        } // end for each connection still in the map
} // end PrintConnections(ostream)

void Sniffer::NewConnection(const tuple4& key)
{
    // create a new value
    DAOCConnection* conn=new DAOC_CONNECTION;
    sniff_map_value value(key,conn);
    // add it to map
    sniff_map_insert_result result=sniff_map.insert(value);
    
    // we had better have added a new value!
    _ASSERTE(result.second == true);

    Logger << "[Sniffer::NewConnection] added connection -> " << key << "\n";

    // done
    return;
} // end NewConnection

void Sniffer::CloseConnection(const tuple4& key)
{
    Logger << "[Sniffer::CloseConnection] removing connection -> " << key << "\n";

    // find connection associated with this key
    DAOCConnection* item=GetConnection(key);

    // this had better exist!
    _ASSERTE(item!=NULL);

    // erase from the map
    sniff_map.erase(key);

    // free memory we allocated in NewConnection
    delete item;

    // done
    return;
} // end CloseConnection

DAOCConnection* Sniffer::GetConnection(const tuple4& key)
{
    // find a connection matching key
    sniff_map_iterator it=sniff_map.find(key);

    if(it == sniff_map.end())
        {
        // connection does not exist
        return(NULL);
        }
    else
        {
        // connection exists, return it
        return(it->second);
        }
} // end GetConnection(key)

DAOCConnection* Sniffer::GetUDPConnectionByClient(const tuple4& key,SOURCE_TYPE& src)
{
    // here we are taking advantage of the fact
    // that connections added via NewConnection()
    // are initiated by the client -- so the 
    // source address in the
    // pair<tuple4,DAOCConnection*> is always
    // the client

    // find key in the map
    // and see who it came from
    sniff_map_iterator it=sniff_map.begin();
    while(it != sniff_map.end())
        {
        if(key.saddr==it->first.saddr)
            {
            // its from the client
            src=FromClient;
            return(it->second);
            }
       
        if(key.saddr==it->first.daddr)
            {
            // its from the server
            src=FromServer;
            return(it->second);
            }

        if(key.daddr==it->first.saddr)
            {
            // its from the client
            src=FromServer;
            return(it->second);
            }
       
        if(key.daddr==it->first.daddr)
            {
            // its from the server
            src=FromClient;
            return(it->second);
            }

        // go to next item
        ++it;
        } // end while

    // not found, return default
    // this is done so that UDP packets that
    // have no corresponding TCP connection
    // can be processed
    src=SourceNotFound;
    return(DefaultUDPConnection);
} // end GetConnection(key,src,dst)

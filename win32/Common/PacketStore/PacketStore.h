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

#include <string> // for string definitions

template<typename T> class PacketStore;
class Packet
{
friend class PacketStore; // friend, horrible syntax though
public:
    Packet(bool tcp=false,bool from_server=false,double time=0,const char* data=NULL,unsigned int len=0)
    {
        TCP=tcp;
        FromServer=from_server;
        Time=time;
        Len=len;
        
        Data=new char[Len];
        memcpy(Data,data,Len);
    }
    Packet(const Packet& s)
    {
        set(s);
    }
    ~Packet()
    {
        delete[] Data;
    }
    
    Packet& operator=(const Packet& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }

    bool GetTCP(void)const{return(TCP);};
    bool GetFromServer(void)const{return(FromServer);};
    double GetTime(void)const{return(Time);};
    unsigned int GetLen(void)const{return(Len);};
    const char* GetData(void)const{return(Data);};

protected:
private:
    void set(const Packet& s)
    {
        TCP=s.TCP;
        FromServer=s.FromServer;
        Time=s.Time;
        Len=s.Len;
        
        delete[] Data;
        Data=new char[Len];
        memcpy(Data,s.Data,Len);
    }

    bool TCP;
    bool FromServer;
    double Time;
    unsigned int Len;
    char* Data; // we allocate this
};

template <typename STORAGE_T> class PacketStore
{
public:
    typedef Packet PACKET_T;
    explicit PacketStore
        (
        const std::string& s
        ) : FileName(s)
    {
        File.open(s.c_str(),std::ios::binary);
    }
    ~PacketStore()
    {
    }

    std::string GetFileName(void)const{return(FileName);};
    
    PacketStore<STORAGE_T>& Store(const typename PacketStore<STORAGE_T>::PACKET_T& s)
    {
        File << s.Time << "\n"
             << s.TCP << "\n"
             << s.FromServer << "\n"
             << s.Len << "\n";
        File.write(s.Data,s.Len);
        return(*this);
    }
    PacketStore<STORAGE_T>& LoadNext(typename PacketStore<STORAGE_T>::PACKET_T& s)
    {
        bool tcp=false;
        bool from_server=false;
        double time=0;
        char* data=NULL;
        unsigned int len=0;
        
        File >> time
             >> tcp
             >> from_server
             >> len;
        File.ignore(1); // ignore the linefeed (carriage return has already been read)
        data=new char[len];
        
        /*
        for(unsigned int i=0;i<len;++i)
            {
            File >> data[i];
            }
        */
        
        File.read(data,len);
        
        s=PACKET_T(tcp,from_server,time,data,len);
        delete[]data;
        return(*this);
    }

    bool good(void)const{return(File.good());};
    bool eof(void)const{return(File.eof());};
    
protected:
private:
    PacketStore(); // disallow
    PacketStore<STORAGE_T>& operator=(const PacketStore<STORAGE_T>& s); // disallow
    PacketStore(const PacketStore& s); // disallow

    const std::string FileName;
    STORAGE_T File;
}; // end class PacketStore

template<typename STORAGE_T>
     inline PacketStore<STORAGE_T>& operator<<(PacketStore<STORAGE_T>& storage,const typename PacketStore<STORAGE_T>::PACKET_T& data)
{
    return(storage.Store(data));
} // end operator<<(storage)

template<typename STORAGE_T>
     inline PacketStore<STORAGE_T>& operator>>(PacketStore<STORAGE_T>& storage,typename PacketStore<STORAGE_T>::PACKET_T& data)
{
    return(storage.LoadNext(data));
} // end operator>>(storage)

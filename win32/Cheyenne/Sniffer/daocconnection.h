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

#include <list>
#include <string>
#include "..\Utils\cheyennemessages.h"
#include "..\Utils\tsdeque.h"

class Database;
class EventSignal;
class CheyenneMessage;

namespace buffer_space
{
class Buffer;

void GetData(unsigned char& res,int& start,const unsigned char* buf);
void GetData(char& res,int& start,const unsigned char* buf);

void GetData(unsigned short& res,int& start,const unsigned char* buf);
void GetData(short& res,int& start,const unsigned char* buf);

void GetData(unsigned int& res,int& start,const unsigned char* buf);
void GetData(int& res,int& start,const unsigned char* buf);

void SkipData(int& start,int bytes);

void GetData(void* variable,int len,int& start,const unsigned char* buf);

void GetPascalString(char** unallocated_string,int& start,const unsigned char* buf);
void GetZeroString(char** unallocated_string,int& start,const unsigned char* buf,const unsigned int& minlen);
} // end pre-define for buffer_space namespace

//const connection_buffer_size=0xFFFF; // 64k

class DAOCConnection
{
public:
    DAOCConnection();
    virtual ~DAOCConnection();


	// sniffing api
    virtual void FromTCPServer(const unsigned char* bytes,const unsigned int& length,tsfifo<CheyenneMessage*>* fifo);
    virtual void FromTCPClient(const unsigned char* bytes,const unsigned int& length,tsfifo<CheyenneMessage*>* fifo);
    virtual void FromUDPServer(const unsigned char* bytes,const unsigned int& length,tsfifo<CheyenneMessage*>* fifo);
    virtual void FromUDPClient(const unsigned char* bytes,const unsigned int& length,tsfifo<CheyenneMessage*>* fifo);

    // crypto
    void Decrypt(unsigned char* data,int data_size);
    static void DAOCConnection::exCrypt_c(unsigned char *data, int data_size, const char *key, int key_size);

    // opcode reference
    const char* GetOpcodeName(opcodes::c_opcode_t opcode)const;

protected:

private:
    // disallow these
    DAOCConnection(const DAOCConnection& s);
    DAOCConnection& operator=(const DAOCConnection& s);
    
    // message parsing
    daocmessages::crypt_and_version* ParseCryptAndVersion(int& ndx,const unsigned char* buffer)const;
    daocmessages::self_id_position* ParseSelfIDPosition(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_pos_update* ParsePlayerPosUpdate(int& ndx,const unsigned char* buffer)const;
    daocmessages::self_health_update* ParseSelfHealthUpdate(int& ndx,const unsigned char* buffer)const;
    daocmessages::mob_pos_update* ParseMobPosUpdate(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_head_update* ParsePlayerHeadUpdate(int& ndx,const unsigned char* buffer)const;
    daocmessages::delete_object* ParseDeleteObject(int& ndx,const unsigned char* buffer)const;
    daocmessages::object_identity* ParseObjectIdentity(int& ndx,const unsigned char* buffer)const;
    daocmessages::vehicle_identity* ParseVehicleIdentity(int& ndx,const unsigned char* buffer)const;
    daocmessages::mob_identity* ParseMobIdentity(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_identity* ParsePlayerIdentity(int& ndx,const unsigned char* buffer)const;
    daocmessages::set_hp* ParseSetHP(int& ndx,const unsigned char* buffer)const;
    daocmessages::self_zone_change* ParseSelfZoneChange(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_level_name* ParsePlayerLevelName(int& ndx,const unsigned char* buffer)const;
    daocmessages::stealth* ParseStealth(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_target* ParsePlayerTarget(int& ndx,const unsigned char* buffer)const;
    daocmessages::player_ground_target* ParsePlayerGroundTarget(int& ndx,const unsigned char* buffer)const;
    daocmessages::object_equipment* ParseObjectEquipment(int& ndx,const unsigned char* buffer)const;
    void ParseNameRealmZone(int& ndx,const unsigned char* buffer);

    // debug
    void PrintPacket
        (
        const bool bTCP,
        const bool bFromServer,
        const unsigned char* buffer,
        const unsigned short len
        )const;

    // encryption and version info
    unsigned char serverprotocol;
    unsigned short self_id;
    unsigned short self_region;
    char crypt_key[12];
    bool bCryptSet;

    // player character info
    std::list<std::pair<std::string,unsigned char> > PlayerCharacterList;
    typedef std::list<std::pair<std::string,unsigned char> >::iterator PlayerCharacterListIterator;
    unsigned char player_realm;

    // opcode/name cross reference
    static char* OpcodeReference[256];

    // message counters (indexed by opcode)
    unsigned int OpcodeCount[256];

}; // end class DAOCConnection

template<typename STORAGE_T> class StoringDAOCConnection : public DAOCConnection
{
public:
    StoringDAOCConnection() : Storage(std::string("daoc.pkt")){};
    StoringDAOCConnection(const StoringDAOCConnection& s) : DAOCConnection(s),Storage(std::string("daoc.pkt")){};
    virtual ~StoringDAOCConnection(){};

    StoringDAOCConnection& operator=(const StoringDAOCConnection& s)
    {
        DAOCConnection::operator =(s);
        return(*this);
    }

    // sniffing api
    virtual void FromTCPServer(const char* bytes,const unsigned int& length)
    {
        Storage << typename STORAGE_T::PACKET_T(true,true,::Clock.Current().Seconds(),bytes,length);
        DAOCConnection::FromTCPServer(bytes,length);
    }
    virtual void FromTCPClient(const char* bytes,const unsigned int& length)
    {
        Storage << typename STORAGE_T::PACKET_T(true,false,::Clock.Current().Seconds(),bytes,length);
        DAOCConnection::FromTCPClient(bytes,length);
    }
    virtual void FromUDPServer(const char* bytes,const unsigned int& length)
    {
        Storage << typename STORAGE_T::PACKET_T(false,true,::Clock.Current().Seconds(),bytes,length);
        DAOCConnection::FromUDPServer(bytes,length);
    }
    virtual void FromUDPClient(const char* bytes,const unsigned int& length)
    {
        Storage << typename STORAGE_T::PACKET_T(false,false,::Clock.Current().Seconds(),bytes,length);
        DAOCConnection::FromUDPClient(bytes,length);
    }
protected:
private:
    STORAGE_T Storage;
}; // end StoringDAOCConnection

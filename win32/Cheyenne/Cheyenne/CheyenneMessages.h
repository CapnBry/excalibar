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
#ifndef CHEYENNE_MESSAGES_H
#define CHEYENNE_MESSAGES_H

#pragma once

class CheyenneMessage
{
public:
    CheyenneMessage(){};
    virtual ~CheyenneMessage(){};

    bool IsSniffed(void)const{return(bSniffed);};
protected:
    bool bSniffed;
private:
}; // end class CheyenneMessage

union word_builder
{
    public:
    word_builder(){dword=0;};
    ~word_builder(){};

    unsigned int dword;
    unsigned short word[2];
    unsigned char byte[4];
    float real;
};

namespace opcodes
{
    typedef const unsigned char c_opcode_t;
    typedef unsigned char opcode_t;

    c_opcode_t player_pos_update=0x01;      // 1
    c_opcode_t self_health_update=0x05;     // 5
    c_opcode_t system_message=0x07;         // 7
    c_opcode_t mob_pos_update=0x09;         // 9
    c_opcode_t delete_object=0x0A;          // 10
    c_opcode_t player_head_update=0x12;     // 18
    c_opcode_t set_hp=0x14;                 // 20
    c_opcode_t player_target=0x18;          // 24
    c_opcode_t self_zone_change=0x1F;       // 31
    c_opcode_t xp=0x39;                     // 57
    c_opcode_t ground_target=0x44;          // 68
    c_opcode_t begin_crafting=0x45;         // 69
    c_opcode_t stealth=0x49;                // 73
    c_opcode_t name_realm_zone=0x55;        // 85
    c_opcode_t craft_timer=0x5B;            // 91
    c_opcode_t unknown_purpose=0x6C;        // 108
    c_opcode_t object_id=0x71;              // 113
    c_opcode_t mob_id=0x72;                 // 114
    c_opcode_t player_id=0x7C;              // 124
    c_opcode_t selfid_pos=0x88;             // 136
    c_opcode_t crypt_and_version=0x8A;      // 138
    c_opcode_t inventory_change=0xAA;       // 170
    c_opcode_t object_equipment=0xBD;       // 189
    c_opcode_t player_level_name=0xBE;      // 190

}; // end namespace opcodes

namespace daocmessages
{
class SniffedMessage : public CheyenneMessage
{
public:
    SniffedMessage(){bSniffed=true;};
    virtual ~SniffedMessage(){};
    opcodes::opcode_t GetOpcode(void)const{return(opcode);};

protected:
    opcodes::opcode_t opcode;
private:
}; // end SniffedMessage

struct player_pos_update : public daocmessages::SniffedMessage
    {
    player_pos_update() {opcode=opcodes::player_pos_update;};
    ~player_pos_update(){};

    unsigned short player_id;
    unsigned short speed;
    unsigned short z;
    unsigned int x;
    unsigned int y;
    unsigned short heading;
    }; // end player_pos_update

struct self_health_update : public daocmessages::SniffedMessage
    {
    self_health_update(){opcode=opcodes::self_health_update;};
    ~self_health_update(){};

    unsigned char health;
    unsigned char mana;
    unsigned char endurance;
    unsigned short player_id;
    }; // end self_help_update

struct system_message : public daocmessages::SniffedMessage
    {
    system_message() : string(NULL){opcode=opcodes::system_message;};
    ~system_message(){if(string){delete string;}};

    unsigned char subcode;
    unsigned char typecode;

    char* string;
    }; // end system_message

struct mob_pos_update : public daocmessages::SniffedMessage
    {
    mob_pos_update(){opcode=opcodes::mob_pos_update;};
    ~mob_pos_update(){};

    unsigned short speed;
    unsigned short heading;
    unsigned int x;
    unsigned int y;
    unsigned short z;
    unsigned short mob_id;
    unsigned char health;
    }; // end mob_pos_update

struct player_head_update : public daocmessages::SniffedMessage
    {
    player_head_update(){opcode=opcodes::player_head_update;};
    ~player_head_update(){};

    unsigned short player_id;
    unsigned short heading;
    unsigned char health; // depending on the source of the message, this field may
                          // no be used. in this case, it should be set to -1 (255)
                          // to indicate that it is not valid
    }; // end player_head_update

struct equipment_item
    {
    bool valid;
    unsigned char obj_list;
    unsigned char obj_index;
    unsigned char obj_color;
    };

enum equipment_slots
    {
    right_hand=0,
    left_hand=1,
    two_hand=2,
    ranged=3,
    helm=4,
    gloves=5,
    boots=6,
    chest=7,
    cloak=8,
    leggings=9,
    sleeves=10,
    unknown=11
    };

struct object_equipment : public daocmessages::SniffedMessage
    {
    object_equipment(){opcode=opcodes::object_equipment;};
    ~object_equipment(){};

    unsigned short info_id;

    equipment_item items[12];
    }; // end object_equipment

struct player_target : public daocmessages::SniffedMessage
    {
    player_target(){opcode=opcodes::player_target;};
    ~player_target(){};

    unsigned short player_id;
    unsigned short target_id;
    }; // end player_target

struct player_ground_target : public daocmessages::SniffedMessage
    {
    player_ground_target(){opcode=opcodes::ground_target;};
    ~player_ground_target(){};

    unsigned short player_id;
    unsigned int x;
    unsigned int y;
    unsigned int z; // strange that this is 4 bytes, normally z seems to be 2 bytes...
    }; // end player_ground_target

struct crypt_and_version : public daocmessages::SniffedMessage
    {
    crypt_and_version(){opcode=opcodes::crypt_and_version;};
    ~crypt_and_version(){};

    unsigned char serverprotocol;
    unsigned char crypt_key[12]; // 12 byte key
    }; // end crypt_version

struct name_realm_zone : public daocmessages::SniffedMessage
    {
    name_realm_zone() : name(NULL){opcode=opcodes::name_realm_zone;};
    ~name_realm_zone(){if(name){delete name;}};

    unsigned char realm;
    unsigned char region;
    char* name;
    }; // end name_realm_zone

struct crafting_timer : public daocmessages::SniffedMessage
    {
    crafting_timer() : product(NULL){opcode=opcodes::craft_timer;};
    ~crafting_timer(){if(product){delete product;}};

    unsigned short timecount;
    char* product;
    }; // end crafting_timer

struct self_id_position : public daocmessages::SniffedMessage
    {
    self_id_position(){opcode=opcodes::selfid_pos;};
    ~self_id_position(){};

    unsigned short self_id;
    unsigned int x;
    unsigned int y;
    unsigned char detected_region;
    unsigned char realm;
    // for some reason, there is no z in this message
    }; // end self_id_position

struct delete_object : public daocmessages::SniffedMessage
    {
    delete_object(){opcode=opcodes::delete_object;};
    ~delete_object(){};

    unsigned short object_id;
    }; // end delete_object

struct object_identity : public daocmessages::SniffedMessage
    {
    object_identity() : name(NULL){opcode=opcodes::object_id;};
    ~object_identity(){if(name){delete name;}};

    unsigned short object_id;
    unsigned short heading;
    unsigned short z;
    unsigned int x;
    unsigned int y;
    char* name;
    unsigned char detected_region;
    }; // end object identity

struct mob_identity : public daocmessages::SniffedMessage
    {
    mob_identity() : name(NULL),guild(NULL){opcode=opcodes::mob_id;};
    ~mob_identity(){if(name){delete name;}if(guild){delete guild;}};

    unsigned short mob_id;
    unsigned short heading;
    unsigned short z;
    unsigned int x;
    unsigned int y;
    unsigned char level;
    char* name;
    char* guild;
    unsigned char detected_region;
    }; // end mob_identity

struct player_identity : public daocmessages::SniffedMessage
    {
    player_identity() : name(NULL),guild(NULL),surname(NULL){opcode=opcodes::player_id;};
    ~player_identity(){if(name){delete name;}if(guild){delete guild;}if(surname){delete surname;}};

    unsigned short player_id;
    unsigned short info_id; // ??
    unsigned int x;
    unsigned int y;
    unsigned short z;
    unsigned short heading;
    unsigned char realm;
    unsigned char level;
    char* name;
    char* guild;
    char* surname;
    unsigned char detected_region;
    }; // end player identity

struct set_hp : public daocmessages::SniffedMessage
    {
    set_hp(){opcode=opcodes::set_hp;};
    ~set_hp(){};

    unsigned short id;
    unsigned char hp;
    }; // end set hp

struct self_zone_change : public daocmessages::SniffedMessage
    {
    self_zone_change(){opcode=opcodes::self_zone_change;};
    ~self_zone_change(){};

    unsigned short region;
    unsigned short id;
    }; // end self zone change

struct inventory_change : public daocmessages::SniffedMessage
    {
    inventory_change() : name(NULL){opcode=opcodes::inventory_change;};
    ~inventory_change(){if(name){delete name;}};

    unsigned char slot;
    unsigned char condition;
    unsigned char durability;
    unsigned char quality;
    unsigned char bonus;
    char* name;
    }; // end inventory change

struct player_level_name : public daocmessages::SniffedMessage
    {
    player_level_name() : name(NULL){opcode=opcodes::player_level_name;};
    ~player_level_name(){if(name){delete name;}};

    unsigned char what;
    unsigned char tp;
    unsigned char level;
    char* name;
    unsigned short player_id;
    unsigned char region;
    }; // end player_level_name

struct stealth : public daocmessages::SniffedMessage
    {
    stealth(){opcode=opcodes::stealth;};
    ~stealth(){};

    unsigned short id;
    }; // end stealth

}; // end namespace daocmessages

#endif // CHEYENNE_MESSAGES_H
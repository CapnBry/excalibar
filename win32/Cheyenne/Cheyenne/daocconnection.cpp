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
// get rid of the stupid
// "identifier truncated" warnings
#pragma warning(disable : 4786)

#include <assert.h>
#include <crtdbg.h>
#include "global.h"
#include "daocconnection.h"
#include "buffer.h"
#include "signals.h"
#include <iostream>

#include "cheyennemessages.h"

char* DAOCConnection::OpcodeReference[256];

using namespace opcodes;

void GetData(unsigned char& res,int& start,const unsigned char* buf)
{
    res=buf[start];

    start+=sizeof(unsigned char);

    // done
    return;
}
void GetData(char& res,int& start,const unsigned char* buf)
{
    res=buf[start];

    start+=sizeof(char);

    // done
    return;
}

void GetData(unsigned short& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[1];
    rb.byte[1]=pwb->byte[0];

    res=rb.word[0];

    start+=sizeof(unsigned short);

    // done
    return;
}

void GetData(short& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[1];
    rb.byte[1]=pwb->byte[0];

    res=rb.word[0];

    start+=sizeof(short);

    // done
    return;
}

void GetData(unsigned int& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[3];
    rb.byte[1]=pwb->byte[2];
    rb.byte[2]=pwb->byte[1];
    rb.byte[3]=pwb->byte[0];

    res=rb.dword;

    start+=sizeof(unsigned int);

    // done
    return;
}

void GetData(int& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[3];
    rb.byte[1]=pwb->byte[2];
    rb.byte[2]=pwb->byte[1];
    rb.byte[3]=pwb->byte[0];

    res=rb.dword;

    start+=sizeof(int);

    // done
    return;
}

void GetData(void* variable,int len,int& start,const unsigned char* buf)
{
    // copy
    memcpy(variable,&buf[start],len);

    // increment
    start+=len;
    
    // done
    return;
} // end GetData variable

void GetPascalString(char** unallocated_string,int& start,const unsigned char* buf)
{
    char len;

    // get the length of the string
    GetData(len,start,buf);

    // not doing length validation here is really bad form, however,
    // we know that the messages are all very small
    // and the buffer they are in is very large (buffer_space::buffer_size, 64k)
    // and that the max a char can hold is 255. So there should not be any 
    // danger of overrunning the buffer and attempting to read memory
    // that we shouldn't. The real reason is that I don't feel like passing
    // the message length all the way down to this function just to prevent
    // a problem that should never happen anyway ;)

    char* sz=new char[len+1]; // +1 for null terminator

    // get characters
    GetData(sz,len,start,buf);

    // add null terminator
    sz[len]='\0';

    // put new string into unallocated_string, making it allocated now :P
    *unallocated_string=sz;

    // done
    return;
} // end GetPascalString

void GetZeroString(char** unallocated_string,int& start,const unsigned char* buf,const unsigned int& minlen)
{
    int current=start;

    // find length
    while(buf[current] != '\0' && current < 65535)
        {
        ++current;
        }

    int len=current-start;

    // allocate string
    char* sz=new char[len+1];

    // get characters
    memcpy(sz,&buf[start],len);
    //GetData(sz,len,start,buf);

    // add null terminator
    sz[len]='\0';

    // if len < minlen then adjust
    if(len<int(minlen))
        {
        start += minlen;
        }
    else
        {
        start += len;
        }

    // put new string into unallocated_string, making it allocated now :P
    *unallocated_string=sz;

    // done
    return;
} // end GetZeroString

void SkipData(int& start,int bytes)
{
    start += bytes;

    // done
    return;
} // end SkipData

DAOCConnection::DAOCConnection()
{
    FromTCPServerBuf=new buffer_space::Buffer;
    FromTCPClientBuf=new buffer_space::Buffer;
    FromUDPServerBuf=new buffer_space::Buffer;
    FromUDPClientBuf=new buffer_space::Buffer;

    bCryptSet=false;
    self_id=0;
    self_region=0;
    player_realm=0;
    fifo=NULL;
    serverprotocol=1; // 1.62 protocol, previous protocols are 0x31

    hThread=NULL;
    dwThreadID=0;
    bContinue=false;
    bRunning=false;

    // make the opcode cross reference table
    for(int i=0;i<256;++i)
        {
        OpcodeReference[i]="unknown";
        }

    OpcodeReference[opcodes::player_pos_update]="player_pos_update";
    OpcodeReference[opcodes::self_health_update]="self_health_update";
    OpcodeReference[opcodes::system_message]="system_message";
    OpcodeReference[opcodes::mob_pos_update]="mob_pos_update";
    OpcodeReference[opcodes::delete_object]="delete_object";
    OpcodeReference[opcodes::player_head_update]="player_head_update";
    OpcodeReference[opcodes::set_hp]="set_hp";
    OpcodeReference[opcodes::player_target]="player_target";
    OpcodeReference[opcodes::self_zone_change]="self_zone_change";
    OpcodeReference[opcodes::xp]="xp";
    OpcodeReference[opcodes::ground_target]="ground_target";
    OpcodeReference[opcodes::begin_crafting]="begin_crafting";
    OpcodeReference[opcodes::stealth]="stealth";
    OpcodeReference[opcodes::name_realm_zone]="name_realm_zone";
    OpcodeReference[opcodes::craft_timer]="craft_timer";
    OpcodeReference[opcodes::unknown_purpose]="unknown_purpose";
    OpcodeReference[opcodes::object_id]="object_id";
    OpcodeReference[opcodes::mob_id]="mob_id";
    OpcodeReference[opcodes::player_id]="player_id";
    OpcodeReference[opcodes::selfid_pos]="selfid_pos";
    OpcodeReference[opcodes::crypt_and_version]="crypt_and_version";
    OpcodeReference[opcodes::inventory_change]="inventory_change";
    OpcodeReference[opcodes::object_equipment]="object_equipment";
    OpcodeReference[opcodes::player_level_name]="player_level_name";

    ZeroMemory(&OpcodeCount[0],sizeof(OpcodeCount));

    // done
    return;
} // end DAOCConnection()

DAOCConnection::DAOCConnection(const DAOCConnection& s)
{
    // need to allocate these here too
    FromTCPServerBuf=new buffer_space::Buffer;
    FromTCPClientBuf=new buffer_space::Buffer;
    FromUDPServerBuf=new buffer_space::Buffer;
    FromUDPClientBuf=new buffer_space::Buffer;

    hThread=NULL;
    dwThreadID=0;
    bContinue=false;
    bRunning=false;

    // set
    set(s);

    // done
    return;
} // end DAOCConnection(DAOCConnection&)

DAOCConnection::~DAOCConnection()
{
    // stop thread if running
    Stop();

    // log opcode counts
    Logger << "[DAOCConnection::~DAOCConnection] used-opcode count:\n";

    for(int i=0;i<256;++i)
        {
        if(OpcodeCount[i] != 0)
            {
            Logger << OpcodeReference[i] << " (" << i << ")=" << OpcodeCount[i] << "\n";
            }
        }

    // free these up
    delete FromTCPServerBuf;
    delete FromTCPClientBuf;
    delete FromUDPServerBuf;
    delete FromUDPClientBuf;

    // done
    return;
} // end ~DAOCConnection

const char* DAOCConnection::GetOpcodeName(opcodes::c_opcode_t opcode)
{
    return(DAOCConnection::OpcodeReference[opcode]);
} // end GetOpcodeName

DAOCConnection& DAOCConnection::operator=(const DAOCConnection& s)
{
    // check for self-assignment
    if(this != &s)
        {
        set(s);
        }
    
    // done
    return(*this);
} // end operator=(DAOCConnection)

void DAOCConnection::set(const DAOCConnection& s)
{
    *FromTCPServerBuf=*s.FromTCPServerBuf;
    *FromTCPClientBuf=*s.FromTCPClientBuf;
    *FromUDPServerBuf=*s.FromUDPServerBuf;
    *FromUDPClientBuf=*s.FromUDPClientBuf;

    bCryptSet=s.bCryptSet;
    serverprotocol=s.serverprotocol;
    memcpy(&crypt_key[0],&s.crypt_key[0],sizeof(crypt_key));
    self_id=s.self_id;
    fifo=s.fifo;

    // done
    return;
} // end set(DAOCConnection)

void DAOCConnection::PrintPacket(bool bTCP,bool bFromServer,const unsigned char* buffer)
{
    // expect buffer to point to the first byte
    // that contains the size of the message

    // get size
    unsigned short size=GetSizeFromBuf(bTCP,bFromServer,buffer);

    Logger << "[DAOCConnection::PrintPacket] packet:\n"
           << (bTCP?"TCP":"UDP")
           << (bFromServer?" From Server":" From Client")
           << " size="
           << size
           << "\n";


    // we will print each line as 8 bits then as a char

    for(int i=0;i<size;i+=4)
        {
        // alias a pointer to the buffer
        word_builder* p=(word_builder*)&buffer[i];

        Logger << (unsigned int)p->byte[0] << " " 
               << (unsigned int)p->byte[1] << " " 
               << (unsigned int)p->byte[2] << " " 
               << (unsigned int)p->byte[3] << " "
               << (isprint(p->byte[0]) ? p->byte[0] : '.')
               << (isprint(p->byte[1]) ? p->byte[1] : '.')
               << (isprint(p->byte[2]) ? p->byte[2] : '.')
               << (isprint(p->byte[3]) ? p->byte[3] : '.') << "\n";

        }

    // done
    return;
} // end PrintPacket

void DAOCConnection::FromTCPServer(const char* bytes,const unsigned int& length)
{
    //Logger << "[DAOCConnection::FromTCPServer] len=" << length << "\n";

    // add to buffer
    if(FromTCPServerBuf->Insert(bytes,length))
        {
        // more data added, signal
        data_signal.signal();
        }

    // done
    return;
} // end FromTCPServer

void DAOCConnection::FromTCPClient(const char* bytes,const unsigned int& length)
{
    //Logger << "[DAOCConnection::FromTCPClient] len=" << length << "\n";

    // add to buffer
    if(FromTCPClientBuf->Insert(bytes,length))
        {
        // more data added, signal
        data_signal.signal();
        }

    // done
    return;
} // end FromTCPClient

void DAOCConnection::FromUDPServer(const char* bytes,const unsigned int& length)
{
    //Logger << "[DAOCConnection::FromUDPServer] len=" << length << "\n";

    // add to buffer
    if(FromUDPServerBuf->Insert(bytes,length))
        {
        // more data added, signal
        data_signal.signal();
        }

    // done
    return;
} // end FromUDPServer

void DAOCConnection::FromUDPClient(const char* bytes,const unsigned int& length)
{
    //Logger << "[DAOCConnection::FromUDPClient] len=" << length << "\n";

    // add to buffer
    if(FromUDPClientBuf->Insert(bytes,length))
        {
        // more data added, signal
        data_signal.signal();
        }

    // done
    return;
} // end FromUDPClient

void DAOCConnection::FromUDPUnknown(const char* bytes,const unsigned int& length)
{
    //Logger << "[DAOCConnection::FromUDPUnknown] len=" << length << "\n";

    // done
    return;
} // end FromUDPUnknown

void DAOCConnection::BuildMessages(void)
{
    unsigned char buffer[buffer_space::buffer_size]; // this should be at least the size of largest message
    unsigned short size; // bytes
    bool bBuiltMessage=false;

    /*
    #ifdef CHEYENNE_DEBUG
    
    if(FromTCPServerBuf->Size() || FromTCPClientBuf->Size() || FromUDPServerBuf->Size() || FromUDPClientBuf->Size())
        {
        Logger << "[DAOCConnection::BuildMessageList]"
               << "\n\ttcp server: " << FromTCPServerBuf->Size()
               << "\n\ttcp client: " << FromTCPClientBuf->Size()
               << "\n\tudp server: " << FromUDPServerBuf->Size()
               << "\n\tudp client: " << FromUDPClientBuf->Size()
               << "\n";
        }
    #endif
    */

    //FromTCPServerBuf->Flush();
    //FromTCPClientBuf->Flush();
    //FromUDPServerBuf->Flush();
    //FromUDPClientBuf->Flush();
    //return;

    // grab size from the buffers
    // check to make sure we have 
    // add the data, then make
    // messages based on the extracted
    // data

    // use Peek() to get the size so that
    // if we do not have all the data, we
    // maintain the size in the buffer
    
    // the offsets (+2) are to adjust for the unsigned short
    // size we just Peek()ed at

    if(FromTCPServerBuf->Peek(buffer,2))
        {
        size=GetSizeFromBuf(true,true,buffer);
        if(FromTCPServerBuf->Extract(buffer,size+2))
            {
            // flag
            bBuiltMessage=true;

            // decrypt
            Decrypt(&buffer[2],size);

            // process
            BuildMessagesFromTCPServer(buffer);
            }
        }

    if(FromTCPClientBuf->Peek(buffer,2))
        {
        size=GetSizeFromBuf(true,false,buffer);
        if(FromTCPClientBuf->Extract(buffer,size+2))
            {
            // flag
            bBuiltMessage=true;

            // decrypt
            Decrypt(&buffer[2],size);

            // process
            BuildMessagesFromTCPClient(buffer);
            }
        }

    if(FromUDPServerBuf->Peek(buffer,2))
        {
        size=GetSizeFromBuf(false,true,buffer);
        if(FromUDPServerBuf->Extract(buffer,size+2))
            {
            // flag
            bBuiltMessage=true;

            // decrypt
            Decrypt(&buffer[2],size);

            // process
            BuildMessagesFromUDPServer(buffer);
            }
        }

    if(FromUDPClientBuf->Peek(buffer,2))
        {
        // flush, we dont really know what to 
        // do with these messages
        FromUDPClientBuf->Flush();

        /*
        size=GetSizeFromBuf(false,false,buffer);
        if(FromUDPClientBuf->Extract(buffer,size+2))
            {
            // flag
            bBuiltMessage=true;

            // no decrypt
            BuildMessagesFromUDPClient(buffer);

            }
        */
        }

    if(bBuiltMessage)
        {
        // since we build a message, check again for more messages immediately
        // this allows us to process messages in groups -- we actually tend to
        // receive them that way anyway

        data_signal.signal();
        }

    // done
    return;
} // end BuildMessageList

unsigned short DAOCConnection::GetSizeFromBuf
    (
    bool bTCP,
    bool bFromServer,
    const unsigned char* buf
    ) const
{
    unsigned short size;
    int ndx=0;

    GetData(size,ndx,buf);

    if(bTCP)
        {
        if(bFromServer)
            {
            // tcp from server
            size+=1;
            }
        else
            {
            // tcp from client
            size+=10;
            }
        }
    else
        {
        // from server or from client,
        // the size computation is the same
        size+=3;
        }

    //Logger << "[DAOCConnection::GetSizeFromBuf] size = " << size << "\n";
    // done
    return(size);
} // end GetSizeFromBuf

void DAOCConnection::BuildMessagesFromTCPServer
    (
    unsigned char* buffer
    )
{
    int ndx=2; // start at offset 2 so we dont look at the size field
               // want to maintain the size in the buffer though so 
               // we can PrintPacket() if we want
    opcode_t opcode;

    // get the opcode
    GetData(opcode,ndx,buffer);
    
    // increment count
    ++OpcodeCount[opcode];

    // the main reason the contents of the case statements are
    // enclosed in curly braces is so that messages can be
    // printed, logged, or whatever as soon as they are
    // extracted from the interface and decrypted

    switch(opcode)
        {
        case player_pos_update:
            {
            daocmessages::player_pos_update* msg=ParsePlayerPosUpdate(ndx,buffer);

            /*
            PrintPacket(true,true,buffer);
            Logger << "player id " << msg->player_id << "\n"
                   << "<" << msg->x << "," << msg->y << "," << msg->z 
                   << "> speed=" << msg->speed 
                   << " heading=" << ((msg->heading&0x0FFF) * 360.0f/4096.0f) << "\n";
            */

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case self_health_update:
            {
            daocmessages::self_health_update* msg=ParseSelfHealthUpdate(ndx,buffer);

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case system_message:
            break;

        case mob_pos_update:
            {
            daocmessages::mob_pos_update* msg=ParseMobPosUpdate(ndx,buffer);

            /*
            PrintPacket(true,true,buffer);
            Logger << "mob id " << msg->mob_id << "\n"
                   << "<" << msg->x << "," << msg->y << "," << msg->z 
                   << "> speed=" << msg->speed 
                   << " heading=" << msg->heading 
                   << " health=" << (unsigned int)msg->health << "\n";
            */

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case player_head_update:
            {
            daocmessages::player_head_update* msg=ParsePlayerHeadUpdate(ndx,buffer);

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case object_equipment:
            {
            daocmessages::object_equipment* msg=ParseObjectEquipment(ndx,buffer);

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case crypt_and_version:
            {
            daocmessages::crypt_and_version* msg=ParseCryptAndVersion(ndx,buffer);

            // set members appropriately
            serverprotocol=msg->serverprotocol;

            memcpy(&crypt_key[0],&msg->crypt_key[0],sizeof(crypt_key));
            bCryptSet=true;

            Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got crypto and version: "
                   << (unsigned int)serverprotocol << "\n";

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case name_realm_zone:
            {
            ParseNameRealmZone(ndx,buffer);

            Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got opcode "
                   << (unsigned short)opcode 
                   << " (" << GetOpcodeName(opcode) << ")\n";

            //PrintPacket(true,true,buffer);

            // this one is not sent to the database
            }
            break;

        case craft_timer:
            break;

        case selfid_pos:
            {
            daocmessages::self_id_position* msg=ParseSelfIDPosition(ndx,buffer);

            // save my ID
            self_id=msg->self_id;

            // save my region
            msg->detected_region=self_region;

            // save my realm
            msg->realm=player_realm;

            
            
            Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got opcode "
                   << (unsigned short)opcode 
                   << " (" << GetOpcodeName(opcode) << ")\n";
            PrintPacket(true,true,buffer);
            Logger << "realm set to " << (unsigned int)msg->realm << "\n";
            

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case delete_object:
            {
            daocmessages::delete_object* msg=ParseDeleteObject(ndx,buffer);

            // object id is an infoid
            // if this is our "self_id" then ignore it --
            // we can't delete ourselves!

            // 90% sure I have seen Mythic send a this message for
            // the "self" that is logged in. The DAoC client ignores it
            // but (until now) the Cheyenne database processed it because there
            // is no concept of "self" in the database. This is why the code to filter
            // this message out is here: the daocconnection determines the "self-centric view", 
            // and reports to the "world-centric" database.

            // I am also fairly sure that this is a method used by Mythic's staff to 
            // determine who is running a sniffer (this sniffer in particular) because you 
            // will definately change your standard behavior when you "lose yourself" on the map.

            // This may be the second strike against you:
            // 1. You did something to attract the attention of a CSR who began the 
            //    monitoring process against you (unless they do this stuff randomly, trolling as it were)
            // 2. You changed your behaviour because of CSR activity that, if you were not running
            //    a sniffer, you should not know about.
            // 3. ?? 

            // There are many valid and inobtrusive tactics that they can use to catch you; 
            // if you like your account un-banned but still insist on running a sniffer,
            // then paranoia is your best friend.

            if(msg->object_id == self_id)
                {
                // log it 
                Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got delete_object that matches self id (" << self_id << ")! Ignoring it...\n";

                // delete here since we are not passing it along
                delete msg;
                }
            else
                {

                // push it onto the fifo for the database
                fifo->Push(msg);
                }
            }
            break;

        case object_id:
            {
            daocmessages::object_identity* msg=ParseObjectIdentity(ndx,buffer);

            // save my region
            msg->detected_region=self_region;

            // push it onto fifo for the database
            fifo->Push(msg);
            }
            break;

        case mob_id:
            {
            daocmessages::mob_identity* msg=ParseMobIdentity(ndx,buffer);

            // save my region
            msg->detected_region=self_region;

            // push it onto fifo for the database
            fifo->Push(msg);
            }
            break;

        case player_id:
            {
            daocmessages::player_identity* msg=ParsePlayerIdentity(ndx,buffer);

            // save my region
            msg->detected_region=self_region;

            // push it onto fifo for the database
            fifo->Push(msg);
            }
            break;

        case set_hp:
            {
            daocmessages::set_hp* msg=ParseSetHP(ndx,buffer);

            // push it onto fifo for database
            fifo->Push(msg);
            }
            break;

        case self_zone_change:
            {
            daocmessages::self_zone_change* msg=ParseSelfZoneChange(ndx,buffer);

            // save id for server
            msg->id=self_id;

            // save my region
            self_region=msg->region;

            // push it onto fifo for database
            fifo->Push(msg);
            }
            break;

        case inventory_change:
            break;

        case unknown_purpose:
            break;

        case player_level_name:
            {
            daocmessages::player_level_name* msg=ParsePlayerLevelName(ndx,buffer);

            if(msg->what == 3 && msg->tp == 0)
                {
                // save id for server
                msg->player_id=self_id;

                // see about finding myself in the list

                for(PlayerCharacterListIterator it=PlayerCharacterList.begin();it!=PlayerCharacterList.end();++it)
                    {
                    if(msg->name != NULL)
                        {
                        if((*it).first == msg->name && self_region==0)
                            {
                            // save initial region
                            // this is overwritten by the zone change message
                            self_region=(*it).second;
                            }
                        }
                    }
                }
            
            // save region
            msg->region=self_region;

            Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got opcode "
                   << (unsigned short)opcode 
                   << " (" << GetOpcodeName(opcode) << ")\n";
            //PrintPacket(true,true,buffer);
            

            // push it on fifo for database
            fifo->Push(msg);
            }
            break;

        case stealth:
            {
            daocmessages::stealth* msg=ParseStealth(ndx,buffer);

            // push it on fifo for database
            fifo->Push(msg);
            }
            break;

        case xp:
            break;

        default:
            if(Config.GetLogUnknownPackets())
                {
                Logger << "unknown opcode " << unsigned int(opcode) << ":\n";
                PrintPacket(true,true,buffer);
                }
            break;
        } // end switch opcode

    
    //Logger << "[DAOCConnection::BuildMessagesFromTCPServer] got opcode "
           //<< (unsigned short)opcode 
           //<< " (" << GetOpcodeName(opcode) << ")\n";

    /*
    PrintPacket(true,true,buffer);
    */
    // done
    return;
} // end BuildMessagesFromTCPServer

void DAOCConnection::BuildMessagesFromTCPClient
    (
    unsigned char* buffer
    )
{
    int ndx=2; // start at offset 2 so we dont look at the size field
               // want to maintain the size in the buffer though so 
               // we can PrintPacket() if we want

    unsigned short seq;
    unsigned short srcid;
    opcode_t opcode;
    unsigned short command;

    // get sequence
    GetData(seq,ndx,buffer);

    // get src id
    GetData(srcid,ndx,buffer);

    // skip unknown
    SkipData(ndx,2);

    // get command
    GetData(command,ndx,buffer);
    
    // turn it into an opcode
    opcode=(opcode_t)command;
    
    // increment count
    ++OpcodeCount[opcode];

    // the main reason the contents of the case statements are
    // enclosed in curly braces is so that messages can be
    // printed, logged, or whatever as soon as they are
    // extracted from the interface and decrypted

    switch(opcode)
        {
        case player_pos_update:
            {
            daocmessages::player_pos_update* msg=ParsePlayerPosUpdate(ndx,buffer);

            // from the tcp client, this messages is a little
            // different. The id is apparently not present, so
            // we will overwrite what the parse has retrieved
            msg->player_id=self_id;

            /*
            PrintPacket(true,false,buffer);
            Logger << "player id " << msg->player_id << "\n"
                   << "<" << msg->x << "," << msg->y << "," << msg->z 
                   << "> speed=" << msg->speed 
                   << " heading=" << ((msg->heading&0x0FFF) * 360.0f/4096.0f) << "\n";
            */

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case player_head_update:
            {
            daocmessages::player_head_update* msg=ParsePlayerHeadUpdate(ndx,buffer);

            // from the tcp client, this messages is a little
            // different. The id is apparently not present, so
            // we will overwrite what the parse has retrieved.
            // also, the health is not present, so
            // we'll overwrite that with an invalid health so the
            // database knows not to use it

            msg->player_id=self_id;
            msg->health=255;

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case player_target:
            {
            daocmessages::player_target* msg=ParsePlayerTarget(ndx,buffer);

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case ground_target:
            {
            daocmessages::player_ground_target* msg=ParsePlayerGroundTarget(ndx,buffer);

            // set player id
            msg->player_id=self_id;

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case begin_crafting:
            // ?? 
            break;

        default:
            if(Config.GetLogUnknownPackets())
                {
                Logger << "unknown opcode " << unsigned int(opcode) << ":\n";
                PrintPacket(true,false,buffer);
                }
            break;
        } // end switch opcode

    
    //Logger << "[DAOCConnection::BuildMessagesFromTCPClient] got opcode "
           //<< (unsigned short)opcode 
           //<< " (" << GetOpcodeName(opcode) << ")\n";
    
    //PrintPacket(true,false,buffer);

    // done
    return;
} // end BuildMessagesFromTCPClient

void DAOCConnection::BuildMessagesFromUDPServer
    (
    unsigned char* buffer
    )
{
    int ndx=2; // start at offset 2 so we dont look at the size field
               // want to maintain the size in the buffer though so 
               // we can PrintPacket() if we want
    unsigned short seq;
    opcode_t opcode;

    // get sequence
    GetData(seq,ndx,buffer);

    // get opcode
    GetData(opcode,ndx,buffer);

    // increment count
    ++OpcodeCount[opcode];

    // the main reason the contents of the case statements are
    // enclosed in curly braces is so that messages can be
    // printed, logged, or whatever as soon as they are
    // extracted from the interface and decrypted

    switch(opcode)
        {
        case player_pos_update:
            {
            daocmessages::player_pos_update* msg=ParsePlayerPosUpdate(ndx,buffer);

            /*
            PrintPacket(false,true,buffer);
            Logger << "player id " << msg->player_id << "\n"
                   << "<" << msg->x << "," << msg->y << "," << msg->z 
                   << "> speed=" << msg->speed 
                   << " heading=" << ((msg->heading&0x0FFF) * 360.0f/4096.0f) << "\n";
            */

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case self_health_update:
            {
            daocmessages::self_health_update* msg=ParseSelfHealthUpdate(ndx,buffer);

            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case system_message:
            break;

        case mob_pos_update:
            {
            daocmessages::mob_pos_update* msg=ParseMobPosUpdate(ndx,buffer);
            
            /*
            PrintPacket(false,true,buffer);
            Logger << "mob id " << msg->mob_id << "\n"
                   << "<" << msg->x << "," << msg->y << "," << msg->z 
                   << "> speed=" << msg->speed 
                   << " heading=" << msg->heading 
                   << " health=" << (unsigned int)msg->health << "\n";

            */
            // put on fifo for the database
            fifo->Push(msg);
            }
            break;

        case player_head_update:
            {
            daocmessages::player_head_update* msg=ParsePlayerHeadUpdate(ndx,buffer);

            // put on fifo for server
            fifo->Push(msg);
            }
            break;

        case object_equipment:
            break;

        default:
            if(Config.GetLogUnknownPackets())
                {
                Logger << "unknown opcode " << unsigned int(opcode) << ":\n";
                PrintPacket(false,true,buffer);
                }
            break;
        } // end switch opcode

    //Logger << "[DAOCConnection::BuildMessagesFromUDPServer] got opcode "
           //<< (unsigned short)opcode 
           //<< " (" << GetOpcodeName(opcode) << ")\n";

    /*
    PrintPacket(false,true,buffer);
    */

    // done
    return;
} // end BuildMessagesFromUDPServer

void DAOCConnection::BuildMessagesFromUDPClient
    (
    unsigned char* buffer
    )
{
    // no known messages

    PrintPacket(false,false,buffer);
    // done
    return;
} // end BuildMessagesFromUDPClient

void DAOCConnection::Decrypt(unsigned char* data,int data_size)
{
    // only do if the key is set
    if(bCryptSet)
        {
        exCrypt_c(data,data_size,&crypt_key[0],sizeof(crypt_key));
        }
    else
        {
        Logger << "[DAOCConnection::Decrypt] crypto not set yet\n";
        }

    // done
    return;
} // end Decrypt

void DAOCConnection::exCrypt_c(unsigned char *data, int data_size, const char *key, int key_size)
{
    // this is snorked straight from the excalibur code...

    int data_pos;
    int key_pos;
    int status_vect;
    int seed_1;
    int seed_2;

    int work_val;

    if (!data)
        return;

    if (!data_size)
        return;

    if (!key)
        return;

    data_pos = 0;
    key_pos = 0;
    status_vect = 0;
    seed_1 = 1;  // esi
    seed_2 = 2;  // edi

    do {
        if (key_pos == key_size)
            key_pos = 0;

        work_val = key[key_pos];
        work_val = work_val + data_pos;
        work_val = work_val + key_pos;
        seed_2 = seed_2 + work_val;
        work_val = work_val * seed_1;
        seed_1 = work_val + 1;
        work_val = seed_1;
        work_val = work_val * seed_2;

        status_vect = status_vect + work_val;
        data[data_pos] = data[data_pos] ^ status_vect;

        data_pos++;
        key_pos++;
    } while (data_pos < data_size);
}

DWORD DAOCConnection::Run(void)
{
    Logger << "[DAOCConnection::Run] beginning execution.\n";

    while(bContinue)
        {
        if(data_signal.wait(500))
            {
            // reset signal
            data_signal.reset();

            // have data, now process it
            BuildMessages();
            }
        } // end forever

    Logger << "[DAOCConnection::Run] exiting thread.\n";

    // clear flag
    bRunning=false;

    // done
    return(0);
} // end Run

bool DAOCConnection::Go(tsfifo<CheyenneMessage*>* p)
{
    if(bRunning)
        {
        Logger << "[DAOCConnection::Go] thread already running!" << "\n";

        return(false);
        }

    // save pointers
    fifo=p;

    // oops check
    _ASSERTE(fifo != NULL);

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

    Logger << "[DAOCConnection::Go] thread id " << unsigned int(dwThreadID) << " created\n";

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

void DAOCConnection::Stop(void)
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
                Logger << "[DAOCConnection::Stop] thread terminated gracefully.\n";
                break;
                }
        
            // sleep a bit
            Sleep(100);
            }
        }
    else
        {
        Logger << "[DAOCConnection::Stop] thread has already terminated gracefully.\n";
        }

    // if thread is still running,
    // forcibly terminate it

    if(bRunning)
        {
        Logger << "[DAOCConnection::Stop] thread failed to terminate, forcibly terminating it.\n";
        bRunning=false;
        TerminateThread(hThread,-1);
        }

    // done
    return;
} // end Stop

DWORD WINAPI DAOCConnection::ThreadFunc(PVOID context)
{
    DAOCConnection* pMe=static_cast<DAOCConnection*>(context);

    _ASSERTE(pMe != NULL);

    return(pMe->Run());
} // end ThreadFunc

daocmessages::crypt_and_version* DAOCConnection::ParseCryptAndVersion
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::crypt_and_version* msg=new daocmessages::crypt_and_version;

    // get unused stuff out of the way
    SkipData(ndx,1);

    // server protocol
    GetData(msg->serverprotocol,ndx,buffer);

    // more unused stuff
    SkipData(ndx,3);

    // crypto key
    GetData(&msg->crypt_key[0],12,ndx,buffer);

    // done
    return(msg);
} // end ParseCryptAndVersion

daocmessages::self_id_position* DAOCConnection::ParseSelfIDPosition
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::self_id_position* msg=new daocmessages::self_id_position;

    // get my id
    GetData(msg->self_id,ndx,buffer);

    // skip unused
    SkipData(ndx,2);

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // done
    return(msg);
} // end ParseSelfIDPosition

daocmessages::player_pos_update* DAOCConnection::ParsePlayerPosUpdate
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_pos_update* msg=new daocmessages::player_pos_update;
    
    // get ID
    GetData(msg->player_id,ndx,buffer);

    // get speed
    GetData(msg->speed,ndx,buffer);

    // get z
    GetData(msg->z,ndx,buffer);

    // skip unused
    SkipData(ndx,2);

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &= 0xFFF;

    // done
    return(msg);
} // end ParsePlayerPosUpdate

daocmessages::self_health_update* DAOCConnection::ParseSelfHealthUpdate
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::self_health_update* msg=new daocmessages::self_health_update;

    // get health
    GetData(msg->health,ndx,buffer);

    // get mana
    GetData(msg->mana,ndx,buffer);

    // skip unused
    SkipData(ndx,3);

    // get endurance
    GetData(msg->endurance,ndx,buffer);

    // put my ID in there for 
    // the server
    msg->player_id=self_id;

    // done
    return(msg);
} // end ParseSelfHealthUpdate

daocmessages::mob_pos_update* DAOCConnection::ParseMobPosUpdate
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::mob_pos_update* msg=new daocmessages::mob_pos_update;

    // get speed
    GetData(msg->speed,ndx,buffer);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &=0xFFF;

    // pre-1.62
    if(serverprotocol != 0x31)
        {
        // get x
        GetData(msg->x,ndx,buffer);

        // get y
        GetData(msg->y,ndx,buffer);

        // skip unused
        SkipData(ndx,8);

        // get z
        GetData(msg->z,ndx,buffer);

        // get ID
        GetData(msg->mob_id,ndx,buffer);

        // get hitpoints
        GetData(msg->health,ndx,buffer);
        } // end if serverprotocol != 0x31
    else // 0x01, 1.62 version
        {
        unsigned short local_x;
        unsigned short local_y;
        unsigned short local_z;
        unsigned char zone;

        // get x
        GetData(local_x,ndx,buffer);

        // skip
        SkipData(ndx,2);

        // get y
        GetData(local_y,ndx,buffer);

        // skip
        SkipData(ndx,2);

        // get z
        GetData(local_z,ndx,buffer);

        // skip
        SkipData(ndx,2);

        // get id
        GetData(msg->mob_id,ndx,buffer);

        // skip
        SkipData(ndx,2);

        // get health
        GetData(msg->health,ndx,buffer);

        // skip
        SkipData(ndx,1);

        // get zone
        GetData(zone,ndx,buffer);

        // convert to global coordinates from zone-relative
        ::Zones.GetGlobalFromZone(zone,local_x,local_y,local_z,msg->x,msg->y,msg->z);
        } // end else serverprotocol == 0x31
    
    // done
    return(msg);
} // end ParseMobPosUpdate

daocmessages::player_head_update* DAOCConnection::ParsePlayerHeadUpdate
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_head_update* msg=new daocmessages::player_head_update;

    // get player id
    GetData(msg->player_id,ndx,buffer);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &=0xFFF;

    // skip unused
    SkipData(ndx,4);

    // get HP (this may not be used -- if >100, its invalid)
    GetData(msg->health,ndx,buffer);

    // done
    return(msg);
} // end ParsePlayerHeadUpdate

daocmessages::delete_object* DAOCConnection::ParseDeleteObject
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::delete_object* msg=new daocmessages::delete_object;

    // get ID
    GetData(msg->object_id,ndx,buffer);

    // done
    return(msg);
} // end ParseDeleteObject

daocmessages::object_identity* DAOCConnection::ParseObjectIdentity
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::object_identity* msg=new daocmessages::object_identity;

    // get object id
    GetData(msg->object_id,ndx,buffer);

    // skip unused 
    SkipData(ndx,2);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &=0xFFF;

    // get z
    GetData(msg->z,ndx,buffer);

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // skip unused
    SkipData(ndx,4);

    // get name (pascal string)
    GetPascalString(&msg->name,ndx,buffer);

    // done
    return(msg);
} // end ParseObjectIdentity

daocmessages::mob_identity* DAOCConnection::ParseMobIdentity
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::mob_identity* msg=new daocmessages::mob_identity;
    
    // get id
    GetData(msg->mob_id,ndx,buffer);

    // skip unused
    SkipData(ndx,2);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &= 0xFFF;

    // get z
    GetData(msg->z,ndx,buffer);

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // skip unused
    SkipData(ndx,5);

    // get level
    GetData(msg->level,ndx,buffer);

    // skip unused
    SkipData(ndx,2);

    // get name
    GetPascalString(&msg->name,ndx,buffer);

    // get guild (mobs have guilds?)
    GetPascalString(&msg->guild,ndx,buffer);
    
    // done
    return(msg);
} // end ParseMobIdentity

daocmessages::player_identity* DAOCConnection::ParsePlayerIdentity
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_identity* msg=new daocmessages::player_identity;

    // get id
    GetData(msg->player_id,ndx,buffer);

    // get info id (how is this different from id?)
    GetData(msg->info_id,ndx,buffer);

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // get z
    GetData(msg->z,ndx,buffer);

    // get heading
    GetData(msg->heading,ndx,buffer);
    msg->heading &=0xFFF;

    // skip unused
    SkipData(ndx,4);

    // get realm
    GetData(msg->realm,ndx,buffer);

    // get level
    GetData(msg->level,ndx,buffer);

    // skip unused
    SkipData(ndx,2);

    // get name
    GetPascalString(&msg->name,ndx,buffer);

    // get guild
    GetPascalString(&msg->guild,ndx,buffer);

    // get surname
    GetPascalString(&msg->surname,ndx,buffer);

    // done
    return(msg);
} // end ParsePlayerIdentity

daocmessages::set_hp* DAOCConnection::ParseSetHP
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::set_hp* msg=new daocmessages::set_hp;

    // skip unused
    SkipData(ndx,2);

    // get id
    GetData(msg->id,ndx,buffer);

    // skip unused
    SkipData(ndx,7);

    // get hp
    GetData(msg->hp,ndx,buffer);

    // done
    return(msg);
} // end ParseSetHP

daocmessages::self_zone_change* DAOCConnection::ParseSelfZoneChange
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::self_zone_change* msg=new daocmessages::self_zone_change;

    // get zone
    GetData(msg->region,ndx,buffer);

    // done
    return(msg);
} // end ParseSelfZoneChange

daocmessages::player_level_name* DAOCConnection::ParsePlayerLevelName
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_level_name* msg=new daocmessages::player_level_name;

    // get what
    GetData(msg->what,ndx,buffer);

    // skip unused
    SkipData(ndx,1);

    // get tp
    GetData(msg->tp,ndx,buffer);

    // see if we are done (this is strange, I know)
    if(msg->what == 3 && msg->tp == 0)
        {
        // skip unused
        SkipData(ndx,1);

        // get level
        GetData(msg->level,ndx,buffer);

        // get player name
        GetPascalString(&msg->name,ndx,buffer);
        }

    // done
    return(msg);
} // end ParsePlayerLevelName

daocmessages::stealth* DAOCConnection::ParseStealth
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::stealth* msg=new daocmessages::stealth;

    // get id
    GetData(msg->id,ndx,buffer);

    // done
    return(msg);
} // end ParseStealth

daocmessages::player_target* DAOCConnection::ParsePlayerTarget
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_target* msg=new daocmessages::player_target;

    // get target id
    GetData(msg->target_id,ndx,buffer);

    // set self id
    msg->player_id=self_id;

    // done
    return(msg);
} // end ParsePlayerTarget

daocmessages::player_ground_target* DAOCConnection::ParsePlayerGroundTarget
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::player_ground_target* msg=new daocmessages::player_ground_target;

    // get x
    GetData(msg->x,ndx,buffer);

    // get y
    GetData(msg->y,ndx,buffer);

    // get z
    GetData(msg->z,ndx,buffer);

    // done
    return(msg);
} // end ParsePlayerGroundTarget

daocmessages::object_equipment* DAOCConnection::ParseObjectEquipment
    (
    int& ndx,
    const unsigned char* buffer
    )const
{
    daocmessages::object_equipment* msg=new daocmessages::object_equipment;

    // this one we need to zero out
    msg->info_id=0;
    for(unsigned char cnt=0;cnt<12;++cnt)
        {
        msg->items[cnt].valid=false;
        }

    // get infoid
    GetData(msg->info_id,ndx,buffer);

    // skip unused
    SkipData(ndx,2); // FF FF for mobs?

    unsigned char objcount;
    unsigned char slot;
    unsigned char finalcount=0;
    daocmessages::equipment_slots equipment_slot=daocmessages::unknown;

    // get object count
    GetData(objcount,ndx,buffer);

    while(objcount)
        {
        // get slot
        GetData(slot,ndx,buffer);

        switch(slot)
            {
            case 0x0a:  // right hand
                equipment_slot=daocmessages::right_hand;
                break;

            case 0x0b:  // left hand
                equipment_slot=daocmessages::left_hand;
                break;

            case 0x0c:  // 2h slot
                equipment_slot=daocmessages::two_hand;
                break;

            case 0x0d:  // ranged
                equipment_slot=daocmessages::ranged;
                break;

            case 0x15:  // helm
                equipment_slot=daocmessages::helm;
                break;

            case 0x16:  // gloves
                equipment_slot=daocmessages::gloves;
                break;

            case 0x17:  // boots
                equipment_slot=daocmessages::boots;
                break;

            case 0x19:  // chest
                equipment_slot=daocmessages::chest;
                break;

            case 0x1a:  // cloak
                equipment_slot=daocmessages::cloak;
                break;

            case 0x1b:  // leggings
                equipment_slot=daocmessages::leggings;
                break;

            case 0x1c:  // sleeves
                equipment_slot=daocmessages::sleeves;
                break;

            default:
                equipment_slot=daocmessages::unknown;
                break;
            } // end switch slot

        // get equipment data
        GetData(msg->items[equipment_slot].obj_list,ndx,buffer);
        GetData(msg->items[equipment_slot].obj_index,ndx,buffer);
        msg->items[equipment_slot].valid=true;
        
        if(msg->items[equipment_slot].obj_list & 0x40)
            {
            // get a color
            GetData(msg->items[equipment_slot].obj_color,ndx,buffer);
            }
        else
            {
            msg->items[equipment_slot].obj_color=0;
            }


        if(msg->items[equipment_slot].obj_list & 0x20)
            {
            // skip data (particle effect?)
            SkipData(ndx,2);
            }

        if(msg->items[equipment_slot].obj_list & 0x80)
            {
            // skip data (guild emblem?)
            SkipData(ndx,2);
            }

        /* it appears if the high bit is set, that this is actually
           a particle effect on the preceeding item? */
        if (!(slot & 0x80))
            {
            objcount--;
            }
        } // end while objcount

    // done
    return(msg);
} // end ParseObjectEquipment

void DAOCConnection::ParseNameRealmZone
    (
    int& ndx,
    const unsigned char* buffer
    )
{
    char* name=NULL;
    unsigned char character_realm;
    unsigned char zone;
    int characters_left;

    /*
     xxx Mythic said "No account shall ever have more than 4 characters xxx
     xxx because it's too hard to change".  So they make our lives easy xxx
     Mythic lied, now there are 8 slots
     */
    characters_left = 8;

    SkipData(ndx,24);

    while(characters_left)
        {
        // get name
        GetZeroString(&name,ndx,buffer,48);

        // skip unused
        SkipData(ndx,74);

        // get realm
        GetData(character_realm,ndx,buffer);
        
        // skip unused
        SkipData(ndx,3);

        // get zone
        GetData(zone,ndx,buffer);
        
        // skip unused
        SkipData(ndx,57);

        // add to list
        if(name)
            {
            if(strlen(name) > 0 && zone != 0)
                {
                Logger << "[DAOCConnection::ParseNameRealmZone] name=" << name << " region=" << (unsigned int)zone
                       << " Realm=" << (unsigned int)character_realm << "\n";
                PlayerCharacterList.insert(PlayerCharacterList.end(),std::make_pair(std::string(name),zone));

                // save this only of there's a character in this slot
                Logger << "[DAOCConnection::ParseNameRealmZone] setting player_realm=" << (unsigned int)player_realm << "\n";
                player_realm=character_realm;
                }
            }

        // done with this
        if(name)
            {
            delete name;
            name=NULL;
            }
        characters_left--;
        } // while chars left
    
    // done
    return;
} // end ParseNameRealmZone


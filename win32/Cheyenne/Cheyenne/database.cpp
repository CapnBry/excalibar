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

#include "global.h"
#include <assert.h>
#include <crtdbg.h>
#include <iostream>
#include <algorithm>
#include <sstream>
#include "nids.h"
#include "database.h"
#include "daocconnection.h"
#include "buffer.h"

Database::Database() : 
    //SpeedCorrection(0.259162303664921465968586387433f), // this is garnered from experience :(
    //SpeedCorrection(0.021f), // this is garnered from experience :(
    SpeedCorrection(1.0f),
    ActorEvents(DatabaseEvents::_LastEvent)
{

    // done
    return;
} // end Database

Database::~Database()
{
    // stop first for cleanup
    Stop();

    // done
    return;
} // end ~Database

DWORD Database::Run(const bool& bContinue)
{
    Logger << "[Database::Run] beginning execution in thread " << unsigned int(GetCurrentThreadId()) << "\n";

    // recover the go param to get the input message fifo
    MessageInputFifo=static_cast<tsfifo<CheyenneMessage*>*>(GoParam);

    // save time of last maintenance
    CheyenneTime LastMaintenanceTime=::Clock.Current();
    const CheyenneTime MaintenanceInterval(0.250f);
    unsigned int WaitAmount;

    /* fake character for display testing
    {
    daocmessages::player_identity* msg=new daocmessages::player_identity;

    msg->player_id=1;

    msg->info_id=2;

    // get x
    msg->x=737280;
    //msg->x=500;

    // get y
    msg->y=606208;
    //msg->y=500;

    // get z
    msg->z=0;

    // get heading
    //msg->heading=1024; // 90�
    msg->heading=0; // 0�

    // get realm
    msg->realm=2;

    // get level
    msg->level=20;

    // get name
    msg->name=new char[20];
    strcpy(msg->name,"NONAME");

    // get guild
    msg->guild=new char[20];
    strcpy(msg->guild,"NOGUILD");

    msg->surname=new char[20];
    strcpy(msg->surname,"NOSURNAME");

    msg->detected_region=100;

    MessageInputFifo->Push(msg);
    }
    {
    daocmessages::player_pos_update* msg=new daocmessages::player_pos_update;

    msg->player_id=1;
    msg->speed=100;
    msg->heading=0;
    msg->x=737280;
    msg->y=606208;
    msg->z=0;

    MessageInputFifo->Push(msg);
    }
    */
        
    while(bContinue)
        {
        // see if its time to do maintenance
        if(::Clock.Current() >= LastMaintenanceTime + MaintenanceInterval)
            {
            // save time
            LastMaintenanceTime=::Clock.Current();

            // do maintenance
            DoMaintenance();
            } // end if time to do maintenance

        // set wait amount to the amount of time before the next maintenance cycle
        WaitAmount=static_cast<unsigned int>(((LastMaintenanceTime + MaintenanceInterval - ::Clock.Current()).Seconds() * 1000.0));

        // when data arrives, process it
        WaitForData(WaitAmount);

        } // end forever

    Logger << "[Database::Run] terminating execution\n";

    // done
    return(0);
} // end Run

void Database::WaitForData(unsigned int timeout)
{
    // wait on the fifo

    if(CheyenneMessage* msg=MessageInputFifo->PopWait(timeout))
        {
        if(msg->IsSniffed())
            {
            // handle sniffed message
            HandleSniffedMessage(static_cast<const daocmessages::SniffedMessage*>(msg));
            }
        else
            {
            // handle non-sniffed message
            }

        // done with this
        delete msg;
        } // end if messages are on fifo
    
    // done
    return;
} // end WaitForData

void Database::UpdateActorByAge(Actor& ThisActor,const CheyenneTime& CurrentAge)
{
    // get <x,y>
    float x=float(ThisActor.GetMotion().GetXPos());
    float y=float(ThisActor.GetMotion().GetYPos());

    float xvel=(ThisActor.GetMotion().GetSpeed() * sin(ThisActor.GetMotion().GetHeading()));
    float yvel=(ThisActor.GetMotion().GetSpeed() * -cos(ThisActor.GetMotion().GetHeading()));

    // update with velocity & heading & delta time
    x = x + (CurrentAge.Seconds()*xvel);
    y = y + (CurrentAge.Seconds()*yvel);

    // store back
    ThisActor.ModifyMotion().SetXPos(x);
    ThisActor.ModifyMotion().SetYPos(y);

    // set time
    ThisActor.ModifyMotion().ModifyValidTime() += CurrentAge;

    // fire event
    ActorEvents[DatabaseEvents::MaintenanceUpdate](ThisActor);

    // done
    return;
} // end UpdateActorByAge

void Database::DoMaintenance(void)
{
    std::list<unsigned int> IdToDelete;
    CheyenneTime MaxAge;
    CheyenneTime CurrentAge;

    // lock the database during the update
    AutoLock al(DBMutex);

    for(actor_iterator it=Actors.begin();it != Actors.end();++it)
        {
        // update 'em
        Actor& ThisActor=(*it).second;

        // max age depends on 
        // what type it is
        switch(ThisActor.GetActorType())
            {
            case Actor::Player:
                // 60 seconds
                MaxAge=CheyenneTime(60.0);
                break;

            case Actor::Object:
                // 300 seconds
                MaxAge=CheyenneTime(300.0);
                break;

            case Actor::Mob:
            default:
                // 60 seconds
                MaxAge=CheyenneTime(60.0);
                break;
            } // end switch actor type

        // get time since last update for this actor
        CurrentAge=::Clock.Current();
        CurrentAge -= ThisActor.GetLastUpdateTime();

        // first see if we need to delete it
        if(CurrentAge > MaxAge)
            {
            // this one needs to be deleted, 
            // add to the delete list
            IdToDelete.insert(IdToDelete.end(),ThisActor.GetInfoId());
            }
        else
            {
            // recalc age so it is delta between current time and actor valid time
            CurrentAge=::Clock.Current();
            CurrentAge-=ThisActor.GetMotion().GetValidTime();

            // update the actor to current time
            UpdateActorByAge(ThisActor,CurrentAge);
            }

        } // end for all actors

    // erase from the list
    while(IdToDelete.begin() != IdToDelete.end())
        {
        //Logger << "[Database::DoMaintenance] deleting id " << *IdToDelete.begin() << "\n";
        DeleteActor(*IdToDelete.begin());
        IdToDelete.erase(IdToDelete.begin());
        }

    // fire event -- we did the maintenance
    ActorEvents[DatabaseEvents::MaintenanceIntervalDone]();
    
    // done
    return;
} // end DoMaintenance

void Database::IntegrateActorToCurrentTime(const CheyenneTime& CurrentTime,Actor& ThisActor)
{
    // calc age so it is delta between current time and actor valid time
    CheyenneTime CurrentAge=(CurrentTime-ThisActor.GetMotion().GetValidTime());

    // only update if there is something to do
    if(CurrentAge.Seconds() > 0.0)
        {
        // update the actor to current time
        UpdateActorByAge(ThisActor,CurrentAge);
        }

    // done
    return;
} // end IntegrateActorToCurrentTime

Actor* Database::GetActorById(const unsigned int& id)
{
    // find it
    actor_iterator it=Actors.find(id);

    // return NULL if not found

    if(it == Actors.end())
        {
        return(NULL);
        }
    else
        {
        return(&(*it).second);
        }
} // end GetActorById

void Database::GetDatabaseStatistics(DatabaseStatistics& stats)const
{
    // lock the database
    AutoLock al(DBMutex);

    // gather some metrics

    stats.SetNumAlbs(0);
    stats.SetNumHibs(0);
    stats.SetNumMids(0);
    stats.SetNumMobs(0);
    stats.SetInfoIdSize(0);

    const_actor_iterator it;

    for(it=Actors.begin();it!=Actors.end();++it)
        {
        switch(it->second.GetRealm())
            {
            case Actor::Albion:
                stats.SetNumAlbs(stats.GetNumAlbs()+1);
                break;

            case Actor::Hibernia:
                stats.SetNumHibs(stats.GetNumHibs()+1);
                break;

            case Actor::Midgard:
                stats.SetNumMids(stats.GetNumMids()+1);
                break;

            default:
                if(it->second.GetActorType() == Actor::Mob)
                    {
                    stats.SetNumMobs(stats.GetNumMobs()+1);
                    }
                break;
            }
        }

    stats.SetInfoIdSize(InfoIdMap.size());

    // done
    return;
} // end GetDatabaseStatistics

unsigned int Database::GetActorInfoIdFromId(const unsigned int& id)
{
    // find it
    infoid_iterator it=InfoIdMap.find(id);

    // if not found, return the supplied id
    if(it==InfoIdMap.end())
        {
        return(id);
        }
    else
        {
        return(it->second);
        }
} // end GetActorInfoIdFromId

Database::actor_iterator Database::GetActorIteratorById(const unsigned int& id)
{
    actor_iterator it=Actors.find(id);

    return(it);
} // end GetActorIteratorById

Actor Database::CopyActorById(const unsigned int& id)const
{
    // make sure we are locked: this is a PUBLIC function
    AutoLock al(DBMutex);

    // find it
    const_actor_iterator it=Actors.find(id);

    // return NULL if not found

    if(it == Actors.end())
        {
        // return an empty actor
        return(Actor());
        }
    else
        {
        return(Actor((*it).second));
        }

} // end CopyActorById

Actor& Database::InsertActorById(const unsigned int& id,bool& bInserted)
{
    actor_map_insert_result result=Actors.insert(actor_map_value(id,Actor()));
    
    bInserted=result.second;

    return((*result.first).second);
} // end InsertActorById

void Database::DeleteActor(const unsigned int& info_id)
{
    // get actor
    actor_iterator it=GetActorIteratorById(info_id);

    if(it == Actors.end())
        {
        // we're done, its not in there
        //Logger << "[Database::DeleteActor] can not delete object (" << info_id << "): its already deleted!\n";
        return;
        }

    Actor& ThisActor=(*it).second;

    // fire event
    ActorEvents[DatabaseEvents::ActorDeleted](ThisActor);

    // see if its a player
    if(ThisActor.IsType(Actor::Player))
        {
        // need to remove infoid too
        infoid_iterator it2=InfoIdMap.find(ThisActor.GetId());

        // make sure it exists
        if(it2 != InfoIdMap.end())
            {
            InfoIdMap.erase(it2);
            }
        }

    // erase it
    Actors.erase(it);

    // done
    return;
} // end DeleteActor

void Database::ResetDatabase(void)
{
    // delete all actors
    while(Actors.begin() != Actors.end())
        {
        if(Actors.begin()->second.IsType(Actor::Player))
            {
            // delete player by its infoid
            DeleteActor(Actors.begin()->second.GetInfoId());
            }
        else
            {
            // delete mobs and objects by their id
            DeleteActor(Actors.begin()->second.GetId());
            }
        }

    // done
    return;
} // end ResetDatabase

void Database::HandleSniffedMessage(const daocmessages::SniffedMessage* msg)
{
    static std::ostringstream os;
    os.seekp(0);

    // lock database
    AutoLock al(DBMutex);

    // handle the message
    switch(msg->GetOpcode())
        {
        case opcodes::player_pos_update:
            {
            const daocmessages::player_pos_update* p=static_cast<const daocmessages::player_pos_update*>(msg);

            // get actor
            Actor* pa=GetActorById(GetActorInfoIdFromId(p->player_id));

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (player_pos_update) unable to find player id " << p->player_id << "\n";

                break;
                }

            Actor& ThisActor=*pa;

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));
            ThisActor.ModifyMotion().SetSpeed(float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff)));

            ThisActor.ModifyMotion().SetSpeed(SpeedCorrection * ThisActor.GetMotion().GetSpeed());
            
            ThisActor.SetLastUpdateTime(::Clock.Current());

            /*
            if(ThisActor.GetId() == ThisActor.GetInfoId())
                {
                // this is probably a locally generated char
                ThisActor.Print(os);
                Logger << "player_pos_update: " << os.str().c_str() << "\n";
                os.str("");
            
                Logger << "[Database::HandleSniffedMessage] player pos update (" << p->player_id << "):\n"
                       << "<" << p->x << "," << p->y << "," << p->z << "> speed="
                       << float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff))
                       << " heading=" << p->heading*360.0f/4096.0f << "\n";
                }
            */
            
            }
            break;

        case opcodes::mob_pos_update:
            {
            const daocmessages::mob_pos_update* p=static_cast<const daocmessages::mob_pos_update*>(msg);

            Actor* pa=GetActorById(p->mob_id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (mob_pos_update) unable to find mob id " << p->mob_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));
            ThisActor.ModifyMotion().SetSpeed(float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff)));
            
            ThisActor.ModifyMotion().SetSpeed(SpeedCorrection * ThisActor.GetMotion().GetSpeed());

            // save other actor info
            ThisActor.SetHealth(p->health);

            ThisActor.SetLastUpdateTime(::Clock.Current());

            //ThisActor.Print(os);
            //os << '\0'; // put null terminator in its place
            //Logger << "mob_pos_update: " << os.str().c_str() << "\n";
            /*
            Logger << "[Database::HandleSniffedMessage] mob pos update (" << p->mob_id << "):\n"
                   << "<" << p->x << "," << p->y << "," << p->z << "> speed="
                   << p->speed << " heading=" << p->heading*360.0f/4096.0f 
                   << " health=" << (int)p->health << "\n";
            */

            }
            break;

        case opcodes::player_head_update:
            {
            const daocmessages::player_head_update* p=static_cast<const daocmessages::player_head_update*>(msg);

            const unsigned int info_id=GetActorInfoIdFromId(p->player_id);
            Actor* pa=GetActorById(info_id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (player_head_update) unable to find player id " << p->player_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save heading
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));

            ThisActor.SetLastUpdateTime(::Clock.Current());

            //ThisActor.Print(os);
            //os << '\0'; // put null terminator in its place
            //Logger << "player_head_update: " << os.str().c_str() << "\n";
            /*
            Logger << "[Database::HandleSniffedMessage] player head update (" << p->player_id << "):\n"
                   << "heading=" << p->heading*360.0f/4096.0f << "\n";
            */

            }
            break;

        case opcodes::object_equipment:
            {
            const daocmessages::object_equipment* p=static_cast<const daocmessages::object_equipment*>(msg);

            // for now: just print it
            // Jonathan?? :P

            // look up by infoid
                        
            Actor* pa=GetActorById(p->info_id);

            if(!pa)
                {
                //Logger << "[Database::HandleSniffedMessage] unable to find player id for object_equipment " << id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            /*
            Logger << "[Database::HandleSniffedMessage] got object equipment for infoid=" << id
                   << " using id=" << id <<  ":\n"
                   << "Actor: " << ThisActor.GetName().c_str() << "\n";
                   
            for(unsigned char i=0;i<sizeof(p->items)/sizeof(daocmessages::equipment_item);++i)
                {
                if(p->items[i].valid)
                    {
                    // print the item
                    switch(i)
                        {
                        case daocmessages::right_hand:
                            Logger << "right_hand ";
                            break;

                        case daocmessages::left_hand:
                            Logger << "left_hand ";
                            break;

                        case daocmessages::two_hand:
                            Logger << "two_hand ";
                            break;

                        case daocmessages::ranged:
                            Logger << "ranged ";
                            break;

                        case daocmessages::helm:
                            Logger << "helm ";
                            break;

                        case daocmessages::gloves:
                            Logger << "gloves ";
                            break;

                        case daocmessages::boots:
                            Logger << "boots ";
                            break;

                        case daocmessages::chest:
                            Logger << "chest ";
                            break;

                        case daocmessages::cloak:
                            Logger << "cloak ";
                            break;

                        case daocmessages::leggings:
                            Logger << "leggings ";
                            break;

                        case daocmessages::sleeves:
                            Logger << "sleeves ";
                            break;

                        default:
                            Logger << "wtf? ";
                            break;
                        }
                    
                    Logger << unsigned int(p->items[i].obj_list) << " "
                           << unsigned int(p->items[i].obj_index) << " "
                           << unsigned int(p->items[i].obj_color) << "\n";
                    } // end if valid
                }
            */
            }
            break;

        case opcodes::self_health_update:
            {
            const daocmessages::self_health_update* p=static_cast<const daocmessages::self_health_update*>(msg);

            // get actor
            Actor* pa=GetActorById(p->player_id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (self_health_update) unable to find self player id " << p->player_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save health
            if(p->health <= 100)
                {
                ThisActor.SetHealth(p->health);
                }
            
            ThisActor.SetLastUpdateTime(::Clock.Current());

            //ThisActor.Print(os);
            //os << '\0'; // put null terminator in its place
            //Logger << "self_health_update: " << os.str().c_str() << "\n";
            }
            break;

        case opcodes::system_message:
            {
            }
            break;

        case opcodes::crypt_and_version:
            {
            // don't care about this one
            }
            break;

        case opcodes::name_realm_zone:
            {
            const daocmessages::name_realm_zone* p=static_cast<const daocmessages::name_realm_zone*>(msg);
            }
            break;

        case opcodes::craft_timer:
            {
            }
            break;

        case opcodes::delete_object:
            {
            const daocmessages::delete_object* p=static_cast<const daocmessages::delete_object*>(msg);

            // delete the actor
            DeleteActor(p->object_id);

            //Logger << "[Database::HandleSniffedMessage] delete object(" << p->object_id << ")\n";

            }
            break;

        case opcodes::selfid_pos:
            {
            const daocmessages::self_id_position* p=static_cast<const daocmessages::self_id_position*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(p->self_id);

            if(bye)
                {
                Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                       << " because it's id conflicts (self_id)\n";
                DeleteActor(bye->GetId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(p->self_id,bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));

            // mark as player
            ThisActor.SetActorType(Actor::Player);

            // save id
            ThisActor.SetId(p->self_id);

            // for self, id=infoid
            ThisActor.SetInfoId(p->self_id);

            // save region
            ThisActor.SetRegion(p->detected_region);

            // save realm
            ThisActor.SetRealm(p->realm);

            ThisActor.SetLastUpdateTime(::Clock.Current());

            if(bInserted)
                {
                // fire event
                ActorEvents[DatabaseEvents::ActorCreated](ThisActor);
                }
            else
                {
                // already existed! fire event
                ActorEvents[DatabaseEvents::ActorReassigned](ThisActor);
                }

            ThisActor.Print(os);
            Logger << "selfid_pos: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            /*
            Logger << "[Database::HandleSniffedMessage] self id position (" << p->self_id << "):\n"
                   << "<" << p->x << "," << p->y <<">\n";
            */

            }
            break;

        case opcodes::object_id:
            {
            const daocmessages::object_identity* p=static_cast<const daocmessages::object_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(p->object_id);

            if(bye)
                {
                Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                       << " because it's id conflicts with " << p->name << "\n";
                DeleteActor(bye->GetId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(p->object_id,bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));

            // save other actor info
            ThisActor.SetName(std::string(p->name));

            // mark as object
            ThisActor.SetActorType(Actor::Object);

            // save id
            ThisActor.SetId(p->object_id);

            // for objects, id=info_id
            ThisActor.SetInfoId(p->object_id);

            // save region
            ThisActor.SetRegion(p->detected_region);

            ThisActor.SetLastUpdateTime(::Clock.Current());

            if(bye==NULL)
                {
                // fire event
                ActorEvents[DatabaseEvents::ActorCreated](ThisActor);
                }
            else
                {
                // already existed! fire event
                ActorEvents[DatabaseEvents::ActorReassigned](ThisActor);
                }
            /*
            ThisActor.Print(os);
            Logger << "object_id: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            */
            /*
            Logger << "[Database::HandleSniffedMessage] object identity (" << p->object_id << "):\n"
                   << "<" << p->x << "," << p->y << "," << p->z << ">"
                   << " heading=" << p->heading*360.0f/4096.0f 
                   << " name=" << p->name << "\n";
            */
            }
            break;

        case opcodes::mob_id:
            {
            const daocmessages::mob_identity* p=static_cast<const daocmessages::mob_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(p->mob_id);

            if(bye)
                {
                Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                       << " because it's id conflicts with " << p->name << "\n";
                DeleteActor(bye->GetInfoId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(p->mob_id,bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));

            // save other actor info
            ThisActor.SetName(std::string(p->name));
            ThisActor.SetLevel(p->level);
            ThisActor.SetGuild(std::string(p->guild));

            // mark as mob
            ThisActor.SetActorType(Actor::Mob);

            // save id
            ThisActor.SetId(p->mob_id);

            // for mobs, id=info_id
            ThisActor.SetInfoId(p->mob_id);

            // save region
            ThisActor.SetRegion(p->detected_region);

            ThisActor.SetLastUpdateTime(::Clock.Current());

            if(bye!=NULL)
                {
                // fire event
                ActorEvents[DatabaseEvents::ActorCreated](ThisActor);
                }
            else
                {
                // already existed! fire event
                ActorEvents[DatabaseEvents::ActorReassigned](ThisActor);
                }

            /*
            ThisActor.Print(os);
            Logger << "mob_id: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            */
            
            /*
            Logger << "[Database::HandleSniffedMessage] mob identity (" << p->mob_id << "):\n"
                   << "<" << p->x << "," << p->y << "," << p->z << ">"
                   << " heading=" << p->heading*360.0f/4096.0f
                   << " level=" << (int)p->level 
                   << " name=" << p->name
                   << " guild=" << p->guild << "\n";
            */
            }
            break;

        case opcodes::player_id:
            {
            const daocmessages::player_identity* p=static_cast<const daocmessages::player_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(p->info_id);

            if(bye)
                {
                Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                       << " because it's id conflicts with " << p->name << "\n";
                DeleteActor(bye->GetInfoId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(p->info_id,bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));

            // save other actor info
            ThisActor.SetRealm(p->realm);
            ThisActor.SetLevel(p->level);
            ThisActor.SetName(std::string(p->name));
            ThisActor.SetSurname(std::string(p->surname));
            ThisActor.SetGuild(std::string(p->guild));

            // mark as player
            ThisActor.SetActorType(Actor::Player);

            // save id
            ThisActor.SetId(p->player_id);
            ThisActor.SetInfoId(p->info_id);

            // save region
            ThisActor.SetRegion(p->detected_region);

            // insert into id -> info_id map
            /*
            infoid_iterator it=InfoIdMap.find(ThisActor.GetId());

            if(it != InfoIdMap.end())
                {
                // it is already in there! remove it!
                bye=GetActorById(it->second);

                if(bye)
                    {
                    Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                           << " because it's infoid conflicts with " << ThisActor.GetName().c_str() << "\n";
                    DeleteActor(bye->GetInfoId());
                    }
                }
            */

            InfoIdMap.insert(infoid_map_value(ThisActor.GetId(),ThisActor.GetInfoId()));

            ThisActor.SetLastUpdateTime(::Clock.Current());

            if(bInserted)
                {
                // fire event
                ActorEvents[DatabaseEvents::ActorCreated](ThisActor);
                }
            else
                {
                // already existed! fire event
                ActorEvents[DatabaseEvents::ActorReassigned](ThisActor);
                }

            /*
            ThisActor.Print(os);
            Logger << "player_id: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            
            Logger << "[Database::HandleSniffedMessage] player identity (" << p->player_id << "):\n"
                   << "<" << p->x << "," << p->y << "," << p->z << ">"
                   << " infoid=" << p->info_id
                   << " heading=" << p->heading*360.0f/4096.0f
                   << " realm=" << (int)p->realm
                   << " level=" << (int)p->level 
                   << " name=" << p->name
                   << " surname=" << p->surname
                   << " guild=" << p->guild << "\n";
            */
            }
            break;

        case opcodes::set_hp:
            {
            const daocmessages::set_hp* p=static_cast<const daocmessages::set_hp*>(msg);

            Actor* pa;

            // get actor by id
            pa=GetActorById(p->id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (set_hp) unable to find id " << p->id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // set hp
            if(p->hp <= 100)
                {
                ThisActor.SetHealth(p->hp);
                }

            ThisActor.SetLastUpdateTime(::Clock.Current());

            //ThisActor.Print(os);
            //os << '\0'; // put null terminator in its place
            //Logger << "set_hp: " << os.str().c_str() << "\n";
            }
            break;

        case opcodes::self_zone_change:
            {
            const daocmessages::self_zone_change* p=static_cast<const daocmessages::self_zone_change*>(msg);

            // get actor
            Actor* pa=GetActorById(p->id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] self_zone_change: unable to find id " << p->id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save region
            ThisActor.SetRegion(p->region);

            Logger << "[Database::HandleSniffedMessage] got self_zone_change to region " << unsigned int(p->region) << "\n";

            // copy actor
            //Actor OriginalActor=ThisActor;

            // Reset database on a zone change. What a pita.
            ResetDatabase();

            /*
            // add original back in
            bool bInserted;
            Actor& CopyActor=InsertActorById(OriginalActor.GetId(),bInserted);

            CopyActor=OriginalActor;
            */
            }
            break;

        case opcodes::inventory_change:
            {
            }
            break;

        case opcodes::unknown_purpose:
            {
            }
            break;

        case opcodes::player_level_name:
            {
            const daocmessages::player_level_name* p=static_cast<const daocmessages::player_level_name*>(msg);

            // get actor
            Actor* pa=GetActorById(p->player_id);

            if(!pa)
                {
                break;
                }

            Actor& ThisActor=*pa;

            // set actor info
            
            if(p->level <= 100)
                {
                ThisActor.SetLevel(p->level);
                }
            
            if(p->name != NULL)
                {
                ThisActor.SetName(std::string(p->name));
                }

            // make sure its marked as a player
            ThisActor.SetActorType(Actor::Player);

            // save region
            ThisActor.SetRegion(p->region);

            ThisActor.SetLastUpdateTime(::Clock.Current());

            // fire event -- we may be renaming a toon here
            ActorEvents[DatabaseEvents::ActorReassigned](ThisActor);

            ThisActor.Print(os);
            Logger << "player_level_name: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            }
            break;

        case opcodes::stealth:
            {
            }
            break;

        case opcodes::xp:
            {
            }
            break;

        case opcodes::player_target:
            {
            const daocmessages::player_target* p=static_cast<const daocmessages::player_target*>(msg);

            // get actor
            Actor* pa=GetActorById(p->player_id);

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] unable to find id " << p->player_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // make sure its a player

            if(!ThisActor.IsType(Actor::Player))
                {
                Logger << "[Database::HandleSniffedMessage] got target for non-player " << ThisActor.GetId() << "\n";
                break;
                }

            // set actor's target (this is done by INFOID!!)

            ThisActor.SetTargetId(p->target_id);

            //ThisActor.Print(os);
            //os << '\0'; // put null terminator in its place
            //Logger << "set_hp: " << os.str().c_str() << "\n";
            /*
            Logger << "[Database::HandleSniffedMessage] player target (" << p->player_id << "):\n"
                   << "target=" << p->target_id << "\n";
            */
            }
            break;

        case opcodes::ground_target:
            {
            }
            break;

        case opcodes::begin_crafting:
            {
            }
            break;

        default:
            // unhandled
            break;
        }

    // done
    return;
} // end HandleSniffedMessage


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
#include <sstream>
#include "..\Utils\buffer.h"
#include "..\Utils\Logger.h"
#include "..\Utils\Times.h"
#include "..\Utils\CodeUtils.h"

// reference a clock & logger (this must be instantiated in 
// a translation unit somewhere)
extern CheyenneClock Clock;
extern logger_t Logger;

#include "stealthmask.h"
#include "database.h"

UncorrelatedStealthInfo::UncorrelatedStealthInfo()
{
    ModifyMask().MakeSphere();
    SetAverageX(0);
    SetAverageY(0);
    SetAverageZ(0);
} // end UncorrelatedStealthInfo()

UncorrelatedStealthInfo::UncorrelatedStealthInfo(const UncorrelatedStealthInfo& s)
{
    set(s);
} // end UncorrelatedStealthInfo(const UncorrelatedStealthInfo&)
UncorrelatedStealthInfo::~UncorrelatedStealthInfo()
{
} // end ~UncorrelatedStealthInfo()

void UncorrelatedStealthInfo::Merge(void)
{
    const centers_size_type center_size=GetCenters().size();
    
    if(center_size==0)
        {
        // done
        return;
        }
        
    // first: merge the positions into an average
    
    // no weighting is given to new vs. old data
    // since it is presumed that all are relatively
    // "fresh" data
    float AccumX=0;
    float AccumY=0;
    float AccumZ=0;
    
    const_centers_iterator it;
    for(it=GetCenters().begin();it!=GetCenters().end();++it)
        {
        AccumX+=it->second.GetXPos();
        AccumY+=it->second.GetYPos();
        AccumZ+=it->second.GetZPos();
        } // end for all centers
    
    // average them out and store
    const float NumAccum=(const float)center_size;
    SetAverageX(AccumX/NumAccum);
    SetAverageY(AccumY/NumAccum);
    SetAverageZ(AccumZ/NumAccum);
    
    // next: merge the stealth masks as offsets from the average
    
    // this must be done as a second for loop since we don't know the 
    // average until it is completely computed
    
    centers_size_type Accumulator[16][16];
    ZeroMemory(&Accumulator[0][0],sizeof(Accumulator));
    
    for(it=GetCenters().begin();it!=GetCenters().end();++it)
        {
        // since each bit in the mask is a 200x200 area
        // we do a bit of math here to get offsets in 
        // mask bit positions instead of world units
        
        const int x_offset=(const int)((it->second.GetXPos() - GetAverageX())/StealthMask::SpanX);
        const int y_offset=(const int)((it->second.GetYPos() - GetAverageY())/StealthMask::SpanY);
        
        for(int y=0;y<16;++y)
            {
            for(int x=0;x<16;++x)
                {
                Accumulator[x][y]+=it->first.Get(x+x_offset,y+y_offset)?1:0;
                } // end for x
            } // end for y
        
        } // end for all centers
    
    for(int y=0;y<16;++y)
        {
        for(int x=0;x<16;++x)
            {
            if(Accumulator[x][y]==center_size)
                {
                ModifyMask().Set(x,y,true);
                }
            else
                {
                ModifyMask().Set(x,y,false);
                }
            } // end for x
        } // end for y
} // end Merge

UncorrelatedStealthInfo& UncorrelatedStealthInfo::operator=(const UncorrelatedStealthInfo& s)
{
    if(this!=&s)
        {
        set(s);
        }
    return(*this);
} // end operator=(UncorrelatedStealthInfo&)
void UncorrelatedStealthInfo::set(const UncorrelatedStealthInfo& s)
{
    MEMBER_ASSIGN(Centers);
    MEMBER_ASSIGN(Mask);
    MEMBER_ASSIGN(AverageX);
    MEMBER_ASSIGN(AverageY);
    MEMBER_ASSIGN(AverageZ);
} // end set(const UncorrelatedStealthInfo&)

Database::Database() : 
    ActorEvents(DatabaseEvents::_LastEvent),
    OldActorThreshold(15.0),
    bFullUpdateRequest(false),
    DeadReconingThreshold(500.0f),
    MinNetworkTime(2.0f),
    NetworkHeartbeat(10.0f),
    UncorrelatedStealthTime(-15.0),
    bSaveDAoCMessages(false),
    MessageSaveStream(0)
{

    // done
    return;
} // end Database

Database::~Database()
{
    // stop first for cleanup
    Stop();

    // delete this
    delete MessageSaveStream;
    
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
    msg->x=737280;
    msg->y=606208;
    msg->z=0;
    //msg->heading=1024; // 90°
    msg->heading=0; // 0°
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

    msg=new daocmessages::player_identity;
    msg->player_id=2;
    msg->info_id=3;
    msg->x=737280;
    msg->y=607208;
    msg->z=0;
    //msg->heading=1024; // 90°
    msg->heading=0; // 0°
    // get realm
    msg->realm=1;
    // get level
    msg->level=20;
    // get name
    msg->name=new char[20];
    strcpy(msg->name,"ENNUS");
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
    //msg->heading=1024*2; // 180°
    msg->heading=0;
    msg->x=737280;
    msg->y=606208;
    msg->z=0;
    msg->visibility = 0x02; // stealth testing

    MessageInputFifo->Push(msg);
    }
    {
    daocmessages::player_target* msg=new daocmessages::player_target;
    
    msg->player_id=2;
    msg->target_id=3;
    MessageInputFifo->Push(msg);
    }
    */
        
    while(bContinue)
        {
        CheyenneTime CurrentTime=::Clock.Current();
        
        // see if its time to do maintenance
        if(CurrentTime >= LastMaintenanceTime + MaintenanceInterval)
            {
            // save time
            LastMaintenanceTime=CurrentTime;

            // do maintenance
            DoMaintenance();
            } // end if time to do maintenance

        // set wait amount to the amount of time before the next maintenance cycle
        WaitAmount=static_cast<unsigned int>(((LastMaintenanceTime + MaintenanceInterval - CurrentTime).Seconds() * 1000.0));

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
            // handle sharenet message
            HandleShareMessage(static_cast<const sharemessages::ShareMessage*>(msg));
            }

        // done with this
        delete msg;
        
        // loop (without waiting) until there are 
        // no more messages to be processed
        while(msg=MessageInputFifo->Pop())
            {
            if(msg->IsSniffed())
                {
                // handle sniffed message
                HandleSniffedMessage(static_cast<const daocmessages::SniffedMessage*>(msg));
                }
            else
                {
                // handle sharenet message
                HandleShareMessage(static_cast<const sharemessages::ShareMessage*>(msg));
                }

            // done with this
            delete msg;
            } // end while there are still messages on the fifo
        } // end if messages are on fifo
    
    // done
    return;
} // end WaitForData

void Database::UpdateActorByAge(Actor& ThisActor,const CheyenneTime& CurrentAge)
{
        // update motion to current time (this also sets ThisActor::Motion::ValidTime)
    ThisActor.ModifyMotion().IntegrateMotion(CurrentAge);

    // update stealth cycle if actor is stealthed
    // IMHO, this looks really cool on-screen :)
    if(ThisActor.GetStealth())
        {
        // map 2 to 2PI (6.283185307179586476925286766559 radians)
        double Radians=6.283185307179586476925286766559 * (0.5 * fmod(ThisActor.GetMotion().GetValidTime().Seconds(),2.0));
        ThisActor.SetStealthCycleA(float(0.5+(0.5*cos(Radians))));
        ThisActor.SetStealthCycleB(float(0.5+(0.5*cos(Radians-1.0471975511965977461542144610932)))); // offset -60°
        ThisActor.SetStealthCycleC(float(0.5+(0.5*cos(Radians-2.0943951023931954923084289221863)))); // offset -120°
        } // end if stealthed

    // fire event
    ActorEvents[DatabaseEvents::MaintenanceUpdate](ThisActor);

    // done
    return;
} // end UpdateActorByAge

void Database::MaintainUncorrelatedStealth(void)
{
    const CheyenneTime MaxAge(15.0); // 15 seconds
    CheyenneTime CurrentAge;
    bool MergeRequired; // flag for when merging is required
    
    // use while loops because we need to 
    // control the iteration when we erase items
    stealth_iterator usi=UncorrelatedStealthers.begin();
    while(usi!=UncorrelatedStealthers.end())
        {
        // claer flag
        MergeRequired=false;
        
        // loop over each center
        UncorrelatedStealthInfo::centers_iterator centers=usi->second.ModifyCenters().begin();
        while(centers!=usi->second.ModifyCenters().end())
            {
            CurrentAge=::Clock.Current();
            CurrentAge-=centers->second.GetValidTime();
            if(CurrentAge > MaxAge)
                {
                // erase this one, its too old
                centers=usi->second.ModifyCenters().erase(centers);
                
                // we modified the centers, merging is required
                MergeRequired=true;
                }
            else
                {
                // increment to next one
                ++centers;
                }
            } // end for all centers on the current uncorrelated stealther
        
        if(usi->second.GetCenters().size() == 0)
            {
            // no more centers (they were all too old), erase this one
            usi=UncorrelatedStealthers.erase(usi);
            }
        else
            {
            if(MergeRequired)
                {
                // merge the uncorrelated info, this
                // makes UncorrelatedStealthInfo::Mask a valid mask
                usi->second.Merge();
                }
            // increment to next one
            ++usi;
            }
        } // end for all uncorrelated stealthers
    
    // done
    return;
} // end MaintainUncorrelatedStealth

void Database::DoMaintenance(void)
{
    std::list<Database::id_type> IdToDelete;
    const CheyenneTime MaxAge(600); // 600 seconds (10 minutes)
    CheyenneTime CurrentAge;

    // lock the database during the update
    AutoLock al(DBMutex);

    for(actor_iterator it=Actors.begin();it != Actors.end();++it)
        {
        // update 'em
        Actor& ThisActor=(*it).second;

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
            // if current age is greater than the threshold,
            // mark the actor as "old" and set speed to 0
            if(CurrentAge > OldActorThreshold)
                {
                ThisActor.SetOld(true);
                ThisActor.ModifyMotion().SetSpeed(0);
                }

            // recalc age so it is delta between current time and actor valid time
            CurrentAge=::Clock.Current();
            CurrentAge-=ThisActor.GetMotion().GetValidTime();

            // update the actor to current time
            UpdateActorByAge(ThisActor,CurrentAge);
            
            // recalc age so it is delta between current time and network valid time
            CurrentAge=::Clock.Current();
            CurrentAge-=ThisActor.GetNet().GetValidTime();
            
            // temporarily store network data
            Motion Net(ThisActor.GetNet());
            
            // update to now
            Net.IntegrateMotion(CurrentAge);
            
            // get distance error between where the network sees
            // this actor and where we see this actor
            const float DRError=Net.RangeTo(ThisActor.GetMotion());

            // update network: we need to determine whether or not to send this 
            // actor to the network here
            if(DRError > DeadReconingThreshold && CurrentAge > MinNetworkTime)
                {
                //Logger << "[Database::DoMaintanance] threshold update:\n";
                /*
                std::ostringstream oss;
                ThisActor.Print(oss);
                Net.Print(oss);
                Logger << oss.str().c_str() << "\n";
                */
                // store current pos back to actor's net
                ThisActor.SetNet(ThisActor.GetMotion());
                
                // send threshold update to network
                SendNetworkUpdate(ThisActor,share_opcodes::threshold_update);
                } // end if DR threshold is violated
            else if(bFullUpdateRequest)
                {
                // store current pos back to actor's net
                ThisActor.SetNet(ThisActor.GetMotion());

                // send full update to network
                SendNetworkUpdate(ThisActor,share_opcodes::full_update);
                } // end else full update requested
            else if(CurrentAge > NetworkHeartbeat && ::Clock.Current() - ThisActor.GetLastLocalTime() < NetworkHeartbeat)
                {
                // store temp network back to actor
                ThisActor.SetNet(Net);

                // send heartbeat update to network
                SendNetworkUpdate(ThisActor,share_opcodes::heartbeat_update);
            	}
            } // end else CurrentAge <= MaxAge

        } // end for all actors

    // erase from the list
    while(IdToDelete.begin() != IdToDelete.end())
        {
        //Logger << "[Database::DoMaintenance] deleting id " << *IdToDelete.begin() << "\n";
        DeleteActor(*IdToDelete.begin());
        IdToDelete.erase(IdToDelete.begin());
        }

    // maintain the uncorrelated stealth database
    MaintainUncorrelatedStealth();
    
    // fire event -- we did the maintenance
    ActorEvents[DatabaseEvents::MaintenanceIntervalDone]();
    
    // clear the full update request flag: full update
    // requests only last for 1 maintenance interval
    bFullUpdateRequest=false;
    
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

Actor* Database::GetActorById(const Database::id_type& info_id)
{
    // find it (this works by INFO ID)
    actor_iterator it=Actors.find(info_id);

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

UncorrelatedStealthInfo* Database::GetUncorrelatedStealthById(const Database::id_type& info_id)
{
    // find it (this works by INFO ID)
    stealth_iterator it=UncorrelatedStealthers.find(info_id);

    // return NULL if not found
    if(it == UncorrelatedStealthers.end())
        {
        return(NULL);
        }
    else
        {
        return(&(*it).second);
        }
} // end GetUncorrelatedStealthById

bool Database::IsUncorrelatedStealth(void)const
{
    return((::Clock.Current() - UncorrelatedStealthTime).Seconds() <= 15.0f);
}
Actor Database::GetUncorrelatedStealthCenter(void)const
{
    return(Actor(UncorrelatedStealthCenter));
}

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
                
                if(it->second.GetHealth() > 0)
                    {
                    stats.SetLiveAlbs(stats.GetLiveAlbs()+1);
                    }
                break;

            case Actor::Hibernia:
                stats.SetNumHibs(stats.GetNumHibs()+1);

                if(it->second.GetHealth() > 0)
                    {
                    stats.SetLiveHibs(stats.GetLiveHibs()+1);
                    }
                break;

            case Actor::Midgard:
                stats.SetNumMids(stats.GetNumMids()+1);
                
                if(it->second.GetHealth() > 0)
                    {
                    stats.SetLiveMids(stats.GetLiveMids()+1);
                    }
                break;

            default:
                if(it->second.GetActorType() == Actor::Mob)
                    {
                    stats.SetNumMobs(stats.GetNumMobs()+1);

                    if(it->second.GetHealth() > 0)
                        {
                        stats.SetLiveMobs(stats.GetLiveMobs()+1);
                        }
                    }

                break;
            }
        }

    stats.SetInfoIdSize(unsigned int(InfoIdMap.size()));

    // done
    return;
} // end GetDatabaseStatistics

Database::id_type Database::GetActorInfoIdFromId(const Database::id_type& id)
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

Database::actor_iterator Database::GetActorIteratorById(const Database::id_type& id)
{
    actor_iterator it=Actors.find(id);

    return(it);
} // end GetActorIteratorById

Actor Database::CopyActorById(const Database::id_type& info_id)const
{
    // make sure we are locked: this is a PUBLIC function
    AutoLock al(DBMutex);

    // find it
    const_actor_iterator it=Actors.find(info_id);

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

bool Database::CopyActorByName(const std::string name,Actor& Result)const
{
    // make sure we are locked: this is a PUBLIC function
    AutoLock al(DBMutex);
    
    // find it
    const_actor_iterator it;
    for(it=Actors.begin();it!=Actors.end();++it)
        {
        if(it->second.GetName() == name)
            {
            // found it, copy
            Result=it->second;
            // done
            return(true);
            }
        } // end for all actors


    // done, did not find it
    return(false);
} // end CopyActorByName

Actor& Database::InsertActorById(const Database::id_type& id,bool& bInserted)
{
    actor_map_insert_result result=Actors.insert(actor_map_value(id,Actor()));
    
    bInserted=result.second;

    return((*result.first).second);
} // end InsertActorById

void Database::DeleteActor(const Database::id_type& info_id)
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

    // DEBUG CODE
    
    if(ThisActor.IsType(Actor::Player))
        {
        if(ThisActor.GetId() == ThisActor.GetInfoId())
            {
            std::ostringstream oss;
            ThisActor.Print(oss);

            // this, most likely, is a LOCAL PLAYER AND IS BEING
            // DELETED. LOG IT
            Logger << "[Database::DeleteActor] deleting a local player:\n"
                   << oss.str() << "\n";
            }
        }

    // END DEBUG CODE

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
    
    // clear uncorrelated stealthers
    UncorrelatedStealthers.clear();
    
    // call functor
    ActorEvents[DatabaseEvents::DatabaseReset]();
    
    // done
    return;
} // end ResetDatabase

Database::id_type Database::GetUniqueId
    (
    const unsigned short region,
    const Database::id_type id_or_infoid
    )
{
    // smush region and id_or_infoid together to make an id that 
    // will be unique across all regions on the current server
    // this is necessary for ShareNet when players participating
    // in the net are in different regions
    word_builder wb;
    
    wb.word[0]=unsigned short(id_or_infoid);
    wb.word[1]=region;
    
    return(Database::id_type(wb.dword));
} // end GetUniqueId

void Database::CrackUniqueId
    (
    const Database::id_type& id,
    unsigned short& id_region,
    Database::id_type& original_id_or_infoid
    )
{
    // un-smush id_region and original_id_or_infoid from the id that 
    // that GetUniqueId(0 makes
    word_builder wb;
    wb.dword=id;
    original_id_or_infoid=wb.word[0];
    id_region=wb.word[1];

    // done
    return;
} // end CrackUniqueId

void Database::SendNetworkUpdate
    (
    const Actor& ThisActor,
    share_opcodes::c_opcode_t opcode
    )
{
    // build and send appropriate message
    switch(opcode)
        {
        case share_opcodes::full_update:
            {
            //::Logger << "[Database::SendNetworkUpdate] full_update on " << ThisActor.GetName().c_str() << "\n";
            sharemessages::full_update msg;
            msg.data.id=ThisActor.GetId();
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.x=ThisActor.GetMotion().GetXPos();
            msg.data.y=ThisActor.GetMotion().GetYPos();
            msg.data.z=ThisActor.GetMotion().GetZPos();
            msg.data.heading=ThisActor.GetMotion().GetHeading();
            msg.data.speed=ThisActor.GetMotion().GetSpeed();
            msg.data.health=ThisActor.GetHealth();
            msg.data.level=ThisActor.GetLevel();
            msg.data.realm=ThisActor.GetRealm();
            msg.data.type=ThisActor.GetActorType();
            msg.data.region=ThisActor.GetRegion();
            msg.data.stealth=ThisActor.GetStealth() ? 1:0;
            memcpy
                (
                msg.data.name,
                ThisActor.GetName().c_str(),
                min<std::string::size_type>(32,ThisActor.GetName().size())
                );
            msg.data.name[min<std::string::size_type>(32,ThisActor.GetName().size())]=0;
            memcpy
                (
                msg.data.surname,
                ThisActor.GetSurname().c_str(),
                min<std::string::size_type>(32,ThisActor.GetSurname().size())
                );
            msg.data.surname[min<std::string::size_type>(32,ThisActor.GetSurname().size())]=0;
            memcpy
                (
                msg.data.guild,
                ThisActor.GetGuild().c_str(),
                min<std::string::size_type>(32,ThisActor.GetGuild().size())
                );
            msg.data.guild[min<std::string::size_type>(32,ThisActor.GetGuild().size())]=0;
            TransmitMessage(msg);
            }
            break;
            
        case share_opcodes::threshold_update:
            {
            //::Logger << "[Database::SendNetworkUpdate] threshold_update on " << ThisActor.GetName().c_str() << "\n";
            sharemessages::threshold_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.heading=ThisActor.GetMotion().GetHeading();
            msg.data.speed=ThisActor.GetMotion().GetSpeed();
            msg.data.x=ThisActor.GetMotion().GetXPos();
            msg.data.y=ThisActor.GetMotion().GetYPos();
            msg.data.z=ThisActor.GetMotion().GetZPos();
            msg.data.health=ThisActor.GetHealth();
            msg.data.level=ThisActor.GetLevel();
            TransmitMessage(msg);
            }
            break;
        
        case share_opcodes::visibility_update:
            {
            //::Logger << "[Database::SendNetworkUpdate] visibility_update on " << ThisActor.GetName().c_str() << "\n";
            sharemessages::visibility_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.visibility=0;//init to 0
            
            if(ThisActor.GetStealth())
                {
                msg.data.visibility |= sharemessages::visibility_update::impl_t::stealth;
                }
            TransmitMessage(msg);
            }
            break;
        
        case share_opcodes::heartbeat_update:
            {
            //::Logger << "[Database::SendNetworkUpdate] heartbeat_update on " << ThisActor.GetName().c_str() << "\n";
            sharemessages::heartbeat_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.health=ThisActor.GetHealth();
            msg.data.level=ThisActor.GetLevel();
            TransmitMessage(msg);
            }
            break;
            
        case share_opcodes::request_full_update:
            ::Logger << "[Database::SendNetworkUpdate] request_full_update\n";
            // request full update from the network
            RequestFullUpdate();
            break;
        
        case share_opcodes::hard_delete:
            {
            sharemessages::hard_delete msg;
            msg.data.infoid=ThisActor.GetInfoId();
            TransmitMessage(msg);
            }
            break;
            
        case share_opcodes::heading_update:
            {
            sharemessages::heading_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.heading=ThisActor.GetMotion().GetHeading();
            TransmitMessage(msg);
            }
            break;
        
        case share_opcodes::speed_update:
            {
            sharemessages::speed_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.speed=ThisActor.GetMotion().GetSpeed();
            TransmitMessage(msg);
            }
            break;
        
        case share_opcodes::target_update:
            {
            sharemessages::target_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.infoid_target=ThisActor.GetTargetId();
            TransmitMessage(msg);
            }
            break;
        
        case share_opcodes::ground_target_update:
            {
            sharemessages::ground_target_update msg;
            msg.data.infoid=ThisActor.GetInfoId();
            msg.data.x=ThisActor.GetGroundTargetX();
            msg.data.y=ThisActor.GetGroundTargetY();
            msg.data.z=ThisActor.GetGroundTargetZ();
            TransmitMessage(msg);
            }
            break;

        default:
            // hmm...
            ::Logger << "[Database::SendNetworkUpdate] unknown opcode: "
                     << unsigned int(opcode) << "\n";
            break;
        } // end switch opcode
    // done
    return;
} // end SendNetworkUpdate

void Database::RequestFullUpdate(void)
{
    // send full update request
    TransmitMessage(sharemessages::request_full_update());

    // set flag: we will do a full update as well
    {
    // lock database: this makes sure we are not in the maintainance
    // functions when the flag is set (maintenance clears the flag)
    AutoLock al(DBMutex);
    bFullUpdateRequest=true;
    }
    
    // done
    return;
} // end RequestFullUpdate

void Database::SaveMessage(const daocmessages::SniffedMessage& msg)
{
    // see if we are supposed to save
    if(bSaveDAoCMessages)
        {
        // make sure exists
        if(MessageSaveStream==0)
            {
            MessageSaveStream=new std::ofstream("daoc_messages.log");
            }

        // save it
        msg.Print(*MessageSaveStream);
        } // end if we are supposed to save
} // end SaveMessage

void Database::HandleShareMessage(const sharemessages::ShareMessage* msg)
{
    // lock database
    AutoLock al(DBMutex);
    
    // storage for the actor pointer
    Actor* pa=NULL; // init to NULL in case we don't find it

    // handle the message
    switch(msg->GetOpcode())
        {
        case share_opcodes::request_full_update:
            {
            const sharemessages::request_full_update* p=static_cast<const sharemessages::request_full_update*>(msg);
            
            // no data in this message, but we set the full update flag
            // as a result
            bFullUpdateRequest=true;
            }
            break;
            
        case share_opcodes::full_update:
            {
            const sharemessages::full_update* p=static_cast<const sharemessages::full_update*>(msg);
            
            // first, see if we have this actor. The infoid
            // here has already been made unique so we don't have to do that
            pa=GetActorById(p->data.infoid);
            
            if(!pa)
                {
                // this is a new actor
                bool bInserted; // don't need to check this
                Actor& ThisActor=InsertActorById(p->data.infoid,bInserted);

                // save motion info
                ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
                ThisActor.ModifyMotion().SetXPos(p->data.x);
                ThisActor.ModifyMotion().SetYPos(p->data.y);
                ThisActor.ModifyMotion().SetZPos(p->data.z);
                ThisActor.ModifyMotion().SetHeading(p->data.heading);
                ThisActor.ModifyMotion().SetSpeed(p->data.speed);

                // save other actor info
                ThisActor.SetRealm(p->data.realm);
                ThisActor.SetLevel(p->data.level);
                ThisActor.SetName(std::string(p->data.name));
                ThisActor.SetSurname(std::string(p->data.surname));
                ThisActor.SetGuild(std::string(p->data.guild));

                // mark as type
                ThisActor.SetActorType(Actor::ActorTypes(p->data.type));

                // save id
                ThisActor.SetId(p->data.id);
                ThisActor.SetInfoId(p->data.infoid);

                // save region
                ThisActor.SetRegion(p->data.region);

                // if player, save id->infoid mapping
                if(ThisActor.IsType(Actor::Player))
                    {
                    InfoIdMap.insert(infoid_map_value(ThisActor.GetId(),ThisActor.GetInfoId()));
                    }

                // save update time
                ThisActor.SetLastUpdateTime(::Clock.Current());

                // clear old flag
                ThisActor.SetOld(false);
                
                // save net data to be = to the motion data
                // we just got
                ThisActor.SetNet(ThisActor.GetMotion());

                // fire event
                ActorEvents[DatabaseEvents::ActorCreated](ThisActor);
                } // end if new actor
            else
                {
                // update an existing actor
                
                // don't bother if our data is pretty recent
                if((::Clock.Current() - pa->GetLastUpdateTime()).Seconds() > 2.0f)
                    {
                    // our last update time is 2s old. Use new data.

                    // save motion info
                    pa->ModifyMotion().SetValidTime(::Clock.Current());
                    pa->ModifyMotion().SetXPos(p->data.x);
                    pa->ModifyMotion().SetYPos(p->data.y);
                    pa->ModifyMotion().SetZPos(p->data.z);
                    pa->ModifyMotion().SetHeading(p->data.heading);
                    pa->ModifyMotion().SetSpeed(p->data.speed);

                    // save other actor info
                    pa->SetLevel(p->data.level);
                    pa->SetName(std::string(p->data.name));
                    pa->SetSurname(std::string(p->data.surname));
                    pa->SetGuild(std::string(p->data.guild));
                    
                    // save update time
                    pa->SetLastUpdateTime(::Clock.Current());

                    // clear old flag
                    pa->SetOld(false);

                    // save net data to be = to the motion data
                    // we just got
                    pa->SetNet(pa->GetMotion());
                    } // end if existing actor is old enough to warrant updating
                } // end else existing actor
            }
            break;
            
        case share_opcodes::heartbeat_update:
            {
            //::Logger << "[share_opcodes::heartbeat_update]\n";
            const sharemessages::heartbeat_update* p=static_cast<const sharemessages::heartbeat_update*>(msg);
            
            // find the actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // if we do not have recent data, then store the info
                if(::Clock.Current() - pa->GetLastUpdateTime() > MinNetworkTime)
                    {
                    // save update time, somebody out there still sees this actor
                    // if the dead reckoning threshold has been violated, we would
                    // have gotten threshold_update instead to update the position.
                    pa->SetLastUpdateTime(::Clock.Current());
                    
                    // clear old flag
                    pa->SetOld(false);

                    // save other data from message
                    pa->SetLevel(p->data.level);
                    pa->SetHealth(p->data.health);
                    
                    // set net=maintenance
                    pa->SetNet(pa->GetMotion());
                    } // end if we don't have any other recent data
                } // end if actor exists
            else
                {
                // got heartbeat on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got heartbeat on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::threshold_update:
            {
            const sharemessages::threshold_update* p=static_cast<const sharemessages::threshold_update*>(msg);
            
            // we get this message when the sender determines that 
            // the actor has violated the dead reckoning threshold
            // and needs to be updated to the network
            // find the actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // save motion info
                pa->ModifyMotion().SetValidTime(::Clock.Current());
                pa->ModifyMotion().SetXPos(p->data.x);
                pa->ModifyMotion().SetYPos(p->data.y);
                pa->ModifyMotion().SetZPos(p->data.z);
                pa->ModifyMotion().SetHeading(p->data.heading);
                pa->ModifyMotion().SetSpeed(p->data.speed);

                // save update time
                pa->SetLastUpdateTime(::Clock.Current());
                
                // clear old flag
                pa->SetOld(false);

                // set net to be = to the motion stuff we just got
                pa->SetNet(pa->GetMotion());
                }
            else
                {
                // got threshold on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got threshold on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::visibility_update:
            {
            const sharemessages::visibility_update* p=static_cast<const sharemessages::visibility_update*>(msg);
            
            // we get this message when someone needs to update the visibility on this actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // save visiblity info
                if(p->data.visibility & sharemessages::visibility_update::impl_t::stealth)
                    {
                    pa->SetStealth(true);
                    }

                // save update time
                pa->SetLastUpdateTime(::Clock.Current());

                // clear old flag
                pa->SetOld(false);
                }
            else
                {
                // got visiblity on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got visiblity on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::hard_delete:
            {
            //::Logger << "[share_opcodes::hard_delete]\n";
            const sharemessages::hard_delete* p=static_cast<const sharemessages::hard_delete*>(msg);
            
            // we get this message when someone needs to update the visibility on this actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // delete it
                DeleteActor(pa->GetInfoId());
                }
            else
                {
                // got visiblity on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got hard delete on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::heading_update:
            {
            //::Logger << "[share_opcodes::heading_update]\n";
            const sharemessages::heading_update* p=static_cast<const sharemessages::heading_update*>(msg);
            
            // we get this message when someone needs to update the heading on this actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                if(::Clock.Current() - pa->GetLastUpdateTime() > MinNetworkTime)
                    {
                    // update heading
                    pa->ModifyMotion().SetHeading(p->data.heading);

                    // save update time
                    pa->SetLastUpdateTime(::Clock.Current());

                    // clear old flag
                    pa->SetOld(false);

                    // set net to be = to the motion stuff we just got
                    pa->SetNet(pa->GetMotion());
                    }
                }
            else
                {
                // got heading_update on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got heading_update on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::speed_update:
            {
            //::Logger << "[share_opcodes::speed_update]\n";
            const sharemessages::speed_update* p=static_cast<const sharemessages::speed_update*>(msg);
            
            // we get this message when someone needs to update the speed on this actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                if(::Clock.Current() - pa->GetLastUpdateTime() > MinNetworkTime)
                    {
                    // update speed
                    pa->ModifyMotion().SetSpeed(p->data.speed);

                    // save update time
                    pa->SetLastUpdateTime(::Clock.Current());

                    // clear old flag
                    pa->SetOld(false);

                    // set net to be = to the motion stuff we just got
                    pa->SetNet(pa->GetMotion());
                    }
                }
            else
                {
                // got speed_update on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got speed_update on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::target_update:
            {
            //::Logger << "[share_opcodes::target_update]\n";
            const sharemessages::target_update* p=static_cast<const sharemessages::target_update*>(msg);
            
            // we get this message when someone needs to update the target for an actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // update target
                pa->SetTargetId(p->data.infoid_target);

                // save update time
                pa->SetLastUpdateTime(::Clock.Current());

                // clear old flag
                pa->SetOld(false);
                }
            else
                {
                // got target_update on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got target_update on an actor we don't have!\n";
                }
            }
            break;
            
        case share_opcodes::ground_target_update:
            {
            //::Logger << "[share_opcodes::ground_target_update]\n";
            const sharemessages::ground_target_update* p=static_cast<const sharemessages::ground_target_update*>(msg);
            
            // we get this message when someone needs to update the ground target for an actor
            pa=GetActorById(p->data.infoid);
            
            if(pa)
                {
                // update ground target
                pa->SetGroundTargetX(float(p->data.x));
                pa->SetGroundTargetY(float(p->data.y));
                pa->SetGroundTargetZ(float(p->data.z));
                

                // save update time
                pa->SetLastUpdateTime(::Clock.Current());

                // clear old flag
                pa->SetOld(false);
                }
            else
                {
                // got ground_target_update on an actor we don't hold! Hmmm.
                Logger << "[Database::HandleShareMessage] got ground_target_update on an actor we don't have!\n";
                }
            }
            break;
            
        default:
            Logger << "[Database::HandleShareMessage] unknown opcode: "
                   << unsigned int(msg->GetOpcode()) << "\n";
            break;
        } // end switch opcode
    
    // see if we ever found an actor
    // if so, then set it's network time
    // to current
    if(pa)
        {
        pa->SetNetTime(::Clock.Current());
        } // end if pa
        
    // done
    return;
} // end HandleShareMessage

void Database::HandleSniffedMessage(const daocmessages::SniffedMessage* msg)
{
    // lock database
    AutoLock al(DBMutex);

    // handle the message
    switch(msg->GetOpcode())
        {
        case opcodes::player_pos_update:
            {
            const daocmessages::player_pos_update* p=static_cast<const daocmessages::player_pos_update*>(msg);

            // get actor
            Actor* pa=GetActorById(GetActorInfoIdFromId(GetUniqueId(p->detected_region,p->player_id)));

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
            
            float speed=float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff));
            
            if(ThisActor.GetMotion().GetSpeed() != speed)
                {
                ThisActor.ModifyMotion().SetSpeed(speed);
                
                // send network update
                SendNetworkUpdate(ThisActor,share_opcodes::speed_update);
                }

            // save stealth
            bool stealth=p->visibility & 0x02 ? true:false;
            if(ThisActor.GetStealth() != stealth)
                {
                ThisActor.SetStealth(stealth);
                
                // send network update
                SendNetworkUpdate(ThisActor,share_opcodes::visibility_update);
                }
            
            // set hp
            if(p->hp <= 100)
                {
                ThisActor.SetHealth(p->hp);
                }
            
            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());
            }
            break;

        case opcodes::mob_pos_update:
            {
            const daocmessages::mob_pos_update* p=static_cast<const daocmessages::mob_pos_update*>(msg);

            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->mob_id));

            if(!pa)
                {
                //Logger << "[Database::HandleSniffedMessage] (mob_pos_update) unable to find mob id " << p->mob_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            
            float heading=Actor::DAOCHeadingToRadians(p->heading);
            
            if(ThisActor.GetMotion().GetHeading() != heading)
                {
                ThisActor.ModifyMotion().SetHeading(heading);
                
                // send network update
                SendNetworkUpdate(ThisActor,share_opcodes::heading_update);
                }

            float speed=float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff));
            
            if(ThisActor.GetMotion().GetSpeed() != speed)
                {
                ThisActor.ModifyMotion().SetSpeed(speed);
                
                // send network update
                SendNetworkUpdate(ThisActor,share_opcodes::speed_update);
                }

            // save other actor info
            ThisActor.SetHealth(p->health);

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            const Database::id_type info_id=GetActorInfoIdFromId(GetUniqueId(p->detected_region,p->player_id));
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,info_id));

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (player_head_update) unable to find player id " << p->player_id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save heading
            float heading=Actor::DAOCHeadingToRadians(p->heading);
            if(ThisActor.GetMotion().GetHeading() != heading)
                {
                ThisActor.ModifyMotion().SetHeading(heading);
                
                // send update
                SendNetworkUpdate(ThisActor,share_opcodes::heading_update);
                }
            
            // save stealth
            bool stealth=p->visibility & 0x02 ? true:false;
            if(ThisActor.GetStealth() != stealth)
                {
                ThisActor.SetStealth(stealth);
                
                // send network update
                SendNetworkUpdate(ThisActor,share_opcodes::visibility_update);
                }

            // set hp
            if(p->hp <= 100)
                {
                ThisActor.SetHealth(p->hp);
                }

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            // save update time
            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());
            }
            break;

        case opcodes::object_equipment:
            {
            const daocmessages::object_equipment* p=static_cast<const daocmessages::object_equipment*>(msg);

            // for now: just print it
            // Jonathan?? :P

            // look up by infoid
                        
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->info_id));

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
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->player_id));

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
            
            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            id_type id=GetUniqueId(p->detected_region,p->object_id);

            // get actor
            Actor* pa=GetActorById(id);
            
            if(pa)
                {
                // send hard_delete to network
                SendNetworkUpdate(*pa,share_opcodes::hard_delete);
                
                // delete the actor
                DeleteActor(id);
                }
            //Logger << "[Database::HandleSniffedMessage] delete object(" << p->object_id << ")\n";
            }
            break;

        case opcodes::selfid_pos:
            {
            const daocmessages::self_id_position* p=static_cast<const daocmessages::self_id_position*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(GetUniqueId(p->detected_region,p->self_id));

            if(bye)
                {
                Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                       << " because it's id conflicts (self_id)\n";
                DeleteActor(bye->GetId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(GetUniqueId(p->detected_region,p->self_id),bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));

            // mark as player
            ThisActor.SetActorType(Actor::Player);

            // save id
            ThisActor.SetId(GetUniqueId(p->detected_region,p->self_id));

            // for self, id=infoid
            ThisActor.SetInfoId(GetUniqueId(p->detected_region,p->self_id));

            // save region
            ThisActor.SetRegion(p->detected_region);

            // save realm
            ThisActor.SetRealm(p->realm);

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            std::ostringstream os;
            ThisActor.Print(os);
            Logger << "selfid_pos: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            
            // send full update to the network
            //SendNetworkUpdate(ThisActor,share_opcodes::full_update);
            /*
            Logger << "[Database::HandleSniffedMessage] self id position (" << p->self_id << "):\n"
                   << "<" << p->x << "," << p->y <<">\n";
            */

            }
            break;

        case opcodes::new_vehicle:
            {
            const daocmessages::vehicle_identity* p=static_cast<const daocmessages::vehicle_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(GetUniqueId(p->detected_region,p->object_id));

            if(bye)
                {
                if(bye->GetName() != p->name)
                    {
                    Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                           << " because it's id conflicts with " << p->name << "\n";
                    }
                DeleteActor(bye->GetId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(GetUniqueId(p->detected_region,p->object_id),bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));
            
            float speed=float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff));
            ThisActor.ModifyMotion().SetSpeed(speed);

            // save other actor info
            ThisActor.SetName(std::string(p->name));

            // mark as object
            ThisActor.SetActorType(Actor::Object);

            // save id
            ThisActor.SetId(GetUniqueId(p->detected_region,p->object_id));

            // for objects, id=info_id
            ThisActor.SetInfoId(GetUniqueId(p->detected_region,p->object_id));

            // save region
            ThisActor.SetRegion(p->detected_region);

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            // send full update to the network
            SendNetworkUpdate(ThisActor,share_opcodes::full_update);
            }
            break;
        case opcodes::object_id:
            {
            const daocmessages::object_identity* p=static_cast<const daocmessages::object_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(GetUniqueId(p->detected_region,p->object_id));

            if(bye)
                {
                if(bye->GetName() != p->name)
                    {
                    Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                           << " because it's id conflicts with " << p->name << "\n";
                    }
                DeleteActor(bye->GetId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(GetUniqueId(p->detected_region,p->object_id),bInserted);

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
            ThisActor.SetId(GetUniqueId(p->detected_region,p->object_id));

            // for objects, id=info_id
            ThisActor.SetInfoId(GetUniqueId(p->detected_region,p->object_id));

            // save region
            ThisActor.SetRegion(p->detected_region);

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            // send full update to the network
            SendNetworkUpdate(ThisActor,share_opcodes::full_update);
            }
            break;

        case opcodes::mob_id:
            {
            const daocmessages::mob_identity* p=static_cast<const daocmessages::mob_identity*>(msg);

            // check for duplication of ID
            Actor* bye=GetActorById(GetUniqueId(p->detected_region,p->mob_id));

            if(bye)
                {
                if(bye->GetName() != p->name)
                    {
                    Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                        << " because it's id conflicts with " << p->name << "\n";
                    }
                DeleteActor(bye->GetInfoId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(GetUniqueId(p->detected_region,p->mob_id),bInserted);

            // save motion info
            ThisActor.ModifyMotion().SetValidTime(::Clock.Current());
            ThisActor.ModifyMotion().SetXPos(float(p->x));
            ThisActor.ModifyMotion().SetYPos(float(p->y));
            ThisActor.ModifyMotion().SetZPos(float(p->z));
            ThisActor.ModifyMotion().SetHeading(Actor::DAOCHeadingToRadians(p->heading));
            ThisActor.ModifyMotion().SetSpeed(float((p->speed&0x0200 ? -((p->speed & 0x3ff) & 0x1ff) : p->speed & 0x3ff)));

            // save other actor info
            ThisActor.SetName(std::string(p->name));
            ThisActor.SetLevel(p->level);
            ThisActor.SetGuild(std::string(p->guild));

            // mark as mob
            ThisActor.SetActorType(Actor::Mob);

            // save id
            ThisActor.SetId(GetUniqueId(p->detected_region,p->mob_id));

            // for mobs, id=info_id
            ThisActor.SetInfoId(GetUniqueId(p->detected_region,p->mob_id));

            // save region
            ThisActor.SetRegion(p->detected_region);

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            // send full update to the network
            SendNetworkUpdate(ThisActor,share_opcodes::full_update);

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
            Actor* bye=GetActorById(GetUniqueId(p->detected_region,p->info_id));

            if(bye)
                {
                if(bye->GetName() != p->name)
                    {
                    Logger << "[Database::HandleSniffedMessage] removing " << bye->GetName().c_str()
                        << " because it's id conflicts with " << p->name << "\n";
                    }  
                DeleteActor(bye->GetInfoId());
                }

            // get actor
            bool bInserted;
            Actor& ThisActor=InsertActorById(GetUniqueId(p->detected_region,p->info_id),bInserted);

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
            ThisActor.SetId(GetUniqueId(p->detected_region,p->player_id));
            ThisActor.SetInfoId(GetUniqueId(p->detected_region,p->info_id));

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

            // make sure the flag is cleared
            ThisActor.SetOld(false);

            ThisActor.SetLastUpdateTime(::Clock.Current());
            ThisActor.SetLastLocalTime(ThisActor.GetLastUpdateTime());

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

            // send full update to network
            SendNetworkUpdate(ThisActor,share_opcodes::full_update);

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
            pa=GetActorById(GetUniqueId(p->detected_region,p->id));

            if(!pa)
                {
                //Logger << "[Database::HandleSniffedMessage] (set_hp) unable to find id " << p->id << "\n";
                break;
                }

            // set hp
            if(p->hp <= 100)
                {
                pa->SetHealth(p->hp);
                }

            // make sure the flag is cleared
            pa->SetOld(false);

            pa->SetLastUpdateTime(::Clock.Current());
            pa->SetLastLocalTime(pa->GetLastUpdateTime());
            }
            break;

        case opcodes::self_zone_change:
            {
            const daocmessages::self_zone_change* p=static_cast<const daocmessages::self_zone_change*>(msg);

            // get actor
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->id));

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] self_zone_change: unable to find id " << p->id << "\n";
                break;
                }

            Actor& ThisActor=*pa;

            // save region
            ThisActor.SetRegion(unsigned char(p->region));

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
            // temporary actor storage
            Actor SelfActor;

            // get actor
            Actor* pa=GetActorById(GetUniqueId(p->region,p->player_id));

            if(!pa)
                {
                // see if this is a reassignment: see the player_level_name
                // case in daocconnection
                
                pa=GetActorById(GetUniqueId(p->original_self_region,p->player_id));
                
                if(!pa)
                    {
                    // oh boy, now what's happened??
                    Logger << "[Database::HandleSniffedMessage] (player_level_name) can't find local actor even on second lookup!\n";
                    break;
                    } // end second if !pa
                else
                    {
                    // we found it!
                    Logger << "[Database::HandleSniffedMessage] (player_level_name) found local actor on second try!\n";
                    
                    // copy and delete
                    SelfActor=*pa;
                    DeleteActor(pa->GetInfoId());
                    
                    // now reinsert
                    bool bInserted;
                    Actor& ThisActor=InsertActorById(GetUniqueId(p->region,p->player_id),bInserted);
                    
                    // save IDs
                    SelfActor.SetId(GetUniqueId(p->region,p->player_id));
                    SelfActor.SetInfoId(GetUniqueId(p->region,p->player_id));
                    
                    // restore everything else
                    ThisActor=SelfActor;
                    
                    // reassign pointer
                    pa=&ThisActor;
                    } // end else second pa
                } // end first if !pa

            // set actor info
            if(p->level <= 100)
                {
                pa->SetLevel(p->level);
                }
            
            if(p->name != NULL)
                {
                pa->SetName(std::string(p->name));
                }

            // make sure its marked as a player
            pa->SetActorType(Actor::Player);

            // save region
            pa->SetRegion(p->region);

            // make sure the flag is cleared
            pa->SetOld(false);

            pa->SetLastUpdateTime(::Clock.Current());
            pa->SetLastLocalTime(pa->GetLastUpdateTime());

            // fire event -- we may be renaming a toon here
            ActorEvents[DatabaseEvents::ActorCreated](*pa);

            // send full update to the network
            SendNetworkUpdate(*pa,share_opcodes::full_update);

            std::ostringstream os;
            pa->Print(os);
            Logger << "player_level_name: " << os.str().c_str() << "\n";
            os.str(""); // put null terminator in its place
            }
            break;

        case opcodes::stealth:
            {
            const daocmessages::stealth* p=static_cast<const daocmessages::stealth*>(msg);
            const id_type id=GetUniqueId(p->detected_region,p->info_id);
            
            // new mask, init to a sphere. Use static const since
            // it is slightly expensive to keep constructing this
            // as a sphere all the time
            static const StealthMask new_mask(true);
            
            // get actor
            Actor* pa=GetActorById(id);
            
            if(!pa)
                {
                // save position and time
                Motion new_position(CopyActorById(GetUniqueId(p->detected_region,p->detector_id)).GetMotion());
                new_position.SetValidTime(::Clock.Current());

                // see if we can find it in the uncorrelated stealth database
                if(UncorrelatedStealthInfo* usi=GetUncorrelatedStealthById(id))
                    {
                    // add new_position to the existing list, maintenance takes
                    // care of the 15 second limit
                    usi->ModifyCenters().insert(usi->ModifyCenters().begin(),std::make_pair(new_mask,new_position));
                    // we changed the centers: merge is required
                    usi->Merge();
                    } // end if already have uncorrelated stealth on this guy
                else
                    {
                    // make a new one
                    UncorrelatedStealthInfo new_usi;
                    // add to centers list
                    new_usi.ModifyCenters().insert(new_usi.ModifyCenters().begin(),std::make_pair(new_mask,new_position));
                    // we changed the centers (ok, so we created them): merge is required
                    new_usi.Merge();
                    // put in database
                    UncorrelatedStealthers.insert(stealth_map_value(id,new_usi));
                    } // end else new uncorrelated stealth
                //Logger << "[Database::HandleSniffedMessage] (stealth) unable to find id " << p->info_id << "\n";
                // save uncorrelated stealth time
                UncorrelatedStealthTime=::Clock.Current();
                // save center point for uncorrelated stealth
                UncorrelatedStealthCenter=CopyActorById(GetUniqueId(p->detected_region,p->detector_id));
                } // end if uncorrelated stealth
            else
                {
                // set stealth flag
                pa->SetStealth(true);
                
                // save time
                pa->SetLastLocalTime(::Clock.Current());

                // if this is an old actor, we want to keep it "alive" since we are
                // still seeing packets for it
                if(pa->GetOld())
                    {
                    // save update time, but do not clear old flag. This
                    // will cause the PPI to show it as old, but the 
                    // database will never delete it -- unless we stop receiving
                    // packets for it.
                    pa->SetLastUpdateTime(::Clock.Current());
                    pa->SetLastLocalTime(pa->GetLastUpdateTime());
                    }

                // send visibility update to the network
                SendNetworkUpdate(*pa,share_opcodes::visibility_update);
                } // end else found stealther
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
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->player_id));

            if(!pa)
                {
                Logger << "[Database::HandleSniffedMessage] (player_target) unable to find id " << p->player_id << "\n";
                break;
                }

            // make sure its a player
            if(!pa->IsType(Actor::Player))
                {
                Logger << "[Database::HandleSniffedMessage] got target for non-player " << pa->GetId() << "\n";
                break;
                }

            // set actor's target (this is done by INFOID!!)
            Database::id_type target_id=GetUniqueId(p->detected_region,p->target_id);
            
            if(pa->GetTargetId() != target_id)
                {
                // save target
                pa->SetTargetId(target_id);
                
                // send network update
                SendNetworkUpdate(*pa,share_opcodes::target_update);
                }

            //LOG_FUNC << "player target (" << p->player_id << "):\n"
                   //<< "target=" << p->target_id << "\n";
            
            }
            break;

        case opcodes::ground_target:
            {
            const daocmessages::player_ground_target* p=static_cast<const daocmessages::player_ground_target*>(msg);
            
            // send network update
            // get actor
            Actor* pa=GetActorById(GetUniqueId(p->detected_region,p->player_id));
            
            if(pa)
                {
                pa->SetGroundTargetX(float(p->x));
                pa->SetGroundTargetY(float(p->y));
                pa->SetGroundTargetZ(float(p->z));
                
                SendNetworkUpdate(*pa,share_opcodes::ground_target_update);
                }
            else
                {
                ::Logger << "[Database::HandleSniffedMessage] got ground target, but can't find local actor!\n";
                }
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


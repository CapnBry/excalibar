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

#include <map>
#include <vector>
#include "..\Utils\CodeUtils.h"
#include "..\Utils\Times.h"
#include "..\Utils\locks.h"
#include "..\Utils\signals.h"
#include "..\Utils\tsdeque.h"
#include "..\Utils\threads.h"
#include "..\Utils\CheyenneMessages.h"
#include ".\actor.h"

class DatabaseFunctor
{
public:
    DatabaseFunctor(){};
    DatabaseFunctor(const DatabaseFunctor& s){};
    virtual ~DatabaseFunctor(){};

    void operator()(const Actor& a){DoIt(a);};
    void operator()(const void* p,const unsigned int len){DoIt(p,len);};
    void operator()(void){DoIt();};

protected:
    virtual void DoIt(const Actor& a){};
    virtual void DoIt(void){};
    virtual void DoIt(const void* p,const unsigned int len){};

private:
    DatabaseFunctor& operator=(const DatabaseFunctor& s);
};

class DatabaseFunctorWrapper
{
public:
    DatabaseFunctorWrapper(){};
    DatabaseFunctorWrapper(const DatabaseFunctorWrapper& s){wrapped=s.wrapped;};
    virtual ~DatabaseFunctorWrapper(){};

    void operator()(const Actor& a)
    {
        if(wrapped.get() != NULL)
            {
            (*wrapped)(a);
            }
    };

    void operator()(void)
    {
        if(wrapped.get() != NULL)
            {
            (*wrapped)();
            }
    };

    void operator()(const void* p,const unsigned short l)
    {
        if(wrapped.get() != NULL)
            {
            (*wrapped)(p,l);
            }
    };

    DatabaseFunctorWrapper& operator=(const DatabaseFunctorWrapper& s){wrapped=s.wrapped;return(*this);};
    DatabaseFunctorWrapper& operator=(std::auto_ptr<DatabaseFunctor>& s){wrapped.operator=(s);return(*this);};

protected:
private:
    DatabaseFunctorWrapper(std::auto_ptr<DatabaseFunctor>& s);

    mutable std::auto_ptr<DatabaseFunctor> wrapped; // conceptually const ;)
};

class DatabaseStatistics
{
public:
    DatabaseStatistics()
    {
        SetNumAlbs(0);
        SetNumHibs(0);
        SetNumMids(0);
        SetNumMobs(0);
        SetInfoIdSize(0);
        SetLiveAlbs(0);
        SetLiveHibs(0);
        SetLiveMids(0);
        SetLiveMobs(0);
    }
    DatabaseStatistics(const DatabaseStatistics& s)
    {
        set(s);
    }
    ~DatabaseStatistics(){};
    
    DatabaseStatistics& operator=(const DatabaseStatistics& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }

protected:

private:
    void set(const DatabaseStatistics& s)
    {
        MEMBER_ASSIGN(NumAlbs);
        MEMBER_ASSIGN(NumHibs);
        MEMBER_ASSIGN(NumMids);
        MEMBER_ASSIGN(NumMobs);
        MEMBER_ASSIGN(InfoIdSize);
    }

    DECL_MEMBER(unsigned int,NumAlbs);
    DECL_MEMBER(unsigned int,NumHibs);
    DECL_MEMBER(unsigned int,NumMids);
    DECL_MEMBER(unsigned int,NumMobs);
    DECL_MEMBER(unsigned int,LiveAlbs);
    DECL_MEMBER(unsigned int,LiveHibs);
    DECL_MEMBER(unsigned int,LiveMids);
    DECL_MEMBER(unsigned int,LiveMobs);
    DECL_MEMBER(unsigned int,InfoIdSize);

}; // end class DatabaseStatistics

class Database : public Thread
{
public:
    Database();
    virtual ~Database();

    typedef unsigned int id_type;
    typedef std::map<Database::id_type,Actor> actor_type;
    typedef std::map<Database::id_type,Actor>::iterator actor_iterator;
    typedef std::map<Database::id_type,Actor>::const_iterator const_actor_iterator;
    typedef std::map<Database::id_type,Actor>::value_type actor_map_value;
    typedef std::pair<actor_iterator,bool> actor_map_insert_result;

    typedef std::map<Database::id_type,Database::id_type>::iterator infoid_iterator;
    typedef std::map<Database::id_type,Database::id_type>::value_type infoid_map_value;
    typedef std::pair<infoid_iterator,bool> infoid_map_insert_result;
    template<class F> F IterateActors(F func)const
    {
        // lock the database
        AutoLock al(DBMutex);
        
        // return result
        return(std::for_each(Actors.begin(),Actors.end(),func));
    };

    template<class F> F UpdateAndIterateActors(F func)
    {
        // lock the database
        AutoLock al(DBMutex);
        
        // save current time
        const CheyenneTime CurrentTime(::Clock.Current());
        
        for(actor_iterator it=Actors.begin();it!=Actors.end();++it)
            {
            // move to current time
            IntegrateActorToCurrentTime(CurrentTime,it->second);
            
            // call functor
            func(*it);
            }

        // return result
        return(func);
    };
    enum DatabaseEvents
    {
        LocalUpdate,
        NetworkUpdate,
        MaintenanceUpdate,
        ActorCreated,
        ActorDeleted,
        ActorReassigned,
        MaintenanceIntervalDone,
        SharenetMessage,
        DatabaseReset,
        _LastEvent
    };

    void InstallFunctor(DatabaseEvents Event,std::auto_ptr<DatabaseFunctor>& Func)
    {
        // lock database for this
        AutoLock al(DBMutex);

        // use DatabaseFunctorWrapper::operator=(std::auto_ptr<DatabaseFunctor>&)
        ActorEvents[Event].operator=(Func);
        return;
    };

    Actor CopyActorById(const Database::id_type& info_id)const;
    bool CopyActorByName(const std::string name,Actor& Result)const;
    void GetDatabaseStatistics(DatabaseStatistics& stats)const;
    bool IsUncorrelatedStealth(void)const;
    Actor GetUncorrelatedStealthCenter(void)const;
    static Database::id_type GetUniqueId(const unsigned short region,const Database::id_type id_or_infoid);

    void RequestFullUpdate(void);

protected:
private:
    Database(const Database& s);
    Database& operator=(const Database& s);

    virtual DWORD Run(const bool& bContinue);
    void WaitForData(unsigned int timeout=250);
    void HandleSniffedMessage(const daocmessages::SniffedMessage* msg);
    void HandleShareMessage(const sharemessages::ShareMessage* msg);
    void DoMaintenance(void);
    void IntegrateActorToCurrentTime(const CheyenneTime& CurrentTime,Actor& ThisActor);
    void SendNetworkUpdate(const Actor& ThisActor,share_opcodes::c_opcode_t opcode);
    
    template<class MSG_T> void TransmitMessage(const MSG_T& msg)
    {
        // make buffer
        void* const transmission=msg.CreateTransmissionBuffer();
        
        // send buffer
        ActorEvents[Database::SharenetMessage](transmission,msg.GetTransmissionSize());
        
        // free buffer
        msg.FreeTransmissionBuffer(transmission);
    }

    void ResetDatabase(void);

    Actor* GetActorById(const Database::id_type& info_id);
    Database::id_type GetActorInfoIdFromId(const Database::id_type& id);
    actor_iterator GetActorIteratorById(const Database::id_type& id);
    Actor& InsertActorById(const Database::id_type& id,bool& bInserted);

    void DeleteActor(const Database::id_type& info_id);

    void UpdateActorByAge(Actor& ThisActor,const CheyenneTime& CurrentAge);

    actor_type Actors; // the map of all actors
    
    std::map<Database::id_type,Database::id_type> InfoIdMap; // map between id -> infoid. only used for player actors

    mutable MutexLock DBMutex;
    tsfifo<CheyenneMessage*>* MessageInputFifo; // input fifo

    std::vector<DatabaseFunctorWrapper> ActorEvents;

    const CheyenneTime OldActorThreshold;
    const float DeadReconingThreshold;
    const CheyenneTime MinNetworkTime;
    const CheyenneTime NetworkHeartbeat;
    
    // these are in global display-adjusted coordinates
    CheyenneTime UncorrelatedStealthTime;
    Actor UncorrelatedStealthCenter;
    
    bool bFullUpdateRequest; // true when a sharenet full update request is active

}; // end class Database

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
#ifndef DATABASE_H
#define DATABASE_H

#pragma once

#pragma warning(push)

// get rid of the stupid
// "identifier truncated" warnings
#pragma warning(disable : 4786)

#include "globaloperators.h"
// include here for struct defs
#include "nids.h"

#include <memory>
#include <map>
#include <vector>
#include <algorithm>
#include "locks.h"
#include "signals.h"
#include "sniffer.h"
#include "tsdeque.h"
#include "mapinfo.h"
#include "threads.h"
#include "actor.h"

class DatabaseFunctor
{
public:
    DatabaseFunctor(){};
    DatabaseFunctor(const DatabaseFunctor& s){};
    virtual ~DatabaseFunctor(){};

    void operator()(const Actor& a){DoIt(a);};
    void operator()(void){DoIt();};

protected:
    virtual void DoIt(const Actor& a){};
    virtual void DoIt(void){};

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
    DECL_MEMBER(unsigned int,InfoIdSize);

}; // end class DatabaseStatistics

class Database : public Thread
{
public:
    Database();
    virtual ~Database();

    typedef unsigned int id_type;
    typedef std::map<id_type,Actor> actor_type;
    typedef std::map<id_type,Actor>::iterator actor_iterator;
    typedef std::map<id_type,Actor>::const_iterator const_actor_iterator;
    typedef std::map<id_type,Actor>::value_type actor_map_value;
    typedef std::pair<actor_iterator,bool> actor_map_insert_result;

    typedef std::map<unsigned int,unsigned int>::iterator infoid_iterator;
    typedef std::map<unsigned int,unsigned int>::value_type infoid_map_value;
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

    Actor CopyActorById(const unsigned int& info_id)const;
    void GetDatabaseStatistics(DatabaseStatistics& stats)const;
    bool IsGroundTargetSet(void)const{return(bGroundTargetSet);};
    Motion GetGroundTarget(void)const{return(GroundTarget);};
    unsigned char GetGroundTargetRegion(void)const{return(GroundTargetRegion);};

protected:
private:
    Database(const Database& s);
    Database& operator=(const Database& s);

    virtual DWORD Run(const bool& bContinue);
    void WaitForData(unsigned int timeout=250);
    void HandleSniffedMessage(const daocmessages::SniffedMessage* msg);
    void DoMaintenance(void);
    void IntegrateActorToCurrentTime(const CheyenneTime& CurrentTime,Actor& ThisActor);

    void ResetDatabase(void);

    Actor* GetActorById(const unsigned int& info_id);
    unsigned int GetActorInfoIdFromId(const unsigned int& id);
    actor_iterator GetActorIteratorById(const unsigned int& id);
    Actor& InsertActorById(const unsigned int& id,bool& bInserted);

    void DeleteActor(const unsigned int& info_id);

    void UpdateActorByAge(Actor& ThisActor,const CheyenneTime& CurrentAge);

    actor_type Actors; // the map of all actors
    
    std::map<unsigned int,unsigned int> InfoIdMap; // map between id -> infoid. only used for player actors

    mutable MutexLock DBMutex;
    tsfifo<CheyenneMessage*>* MessageInputFifo; // input fifo

    std::vector<DatabaseFunctorWrapper> ActorEvents;

    const float SpeedCorrection;
    const CheyenneTime OldActorThreshold;
    
    // these are in global display-adjusted coordinates
    Motion GroundTarget;
    unsigned char GroundTargetRegion;
    bool bGroundTargetSet;

}; // end class Database

#pragma warning(pop)

#endif //DATABASE_H
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

#include <algorithm>
#include <limits>
#include "central.h"
#include "actor.h"

class ActorRenderFunctor
{
public:
    ActorRenderFunctor(const ActorRenderFunctor& s):central(s.central){};
    explicit ActorRenderFunctor(Central& s):central(s){};
    ~ActorRenderFunctor(){};

    void operator()(const Database::actor_map_value& s)
    {
        central.RenderActor(s.second);

        // done
        return;
    }

private:
    ActorRenderFunctor& operator=(const ActorRenderFunctor& s); // disallow
    Central& central;
};

class ClosestActorFinder
{
public:
    ClosestActorFinder(const ClosestActorFinder& s) :
        central(s.central),PointX(s.PointX),PointY(s.PointY),Dist(s.Dist),id(s.id){};
    
    explicit ClosestActorFinder(const Central& s,const float x,const float y) :
        central(s),PointX(x),PointY(y),id(0)
    {
        Dist=FLT_MAX;
    };
    
    ~ClosestActorFinder(){};

    void operator()(const Database::actor_map_value& s)
    {
        const Actor& ThisActor=s.second;
        Motion Position;

        central.GetRenderPosition(ThisActor,Position);

        //Logger << "[ClosestActorFinder] comparing <" << Position.GetXPos() << "," << Position.GetYPos() << "> with <"
               //<< PointX << "," << PointY << ">\n";

        float DelX=Position.GetXPos() - PointX;
        float DelY=Position.GetYPos() - PointY;

        float ThisDist=float(sqrt(DelX*DelX + DelY*DelY));

        // save
        if(ThisDist < Dist)
            {
            Dist=ThisDist;
            id=ThisActor.GetInfoId();
            }

        // done
        return;
    }

    float GetDist(void)const{return(Dist);};
    unsigned int GetId(void)const{return(id);};
private:
    ClosestActorFinder& operator=(const ClosestActorFinder& s); // disallow
    const Central& central;
    const float PointX;
    const float PointY;
    float Dist;
    unsigned int id;
};

class MaintenanceUpdateFunctor : public DatabaseFunctor
{
public:
    MaintenanceUpdateFunctor(Central& s):central(s){};
    explicit MaintenanceUpdateFunctor(Central& c,const MaintenanceUpdateFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~MaintenanceUpdateFunctor(){};


protected:
    virtual void DoIt(const Actor& a){central.OnMaintenanceUpdate(a);};

private:
    Central& central;
}; // end class MaintainanceUpdateFunctor

class NewActorFunctor : public DatabaseFunctor
{
public:
    NewActorFunctor(Central& s):central(s){};
    explicit NewActorFunctor(Central& c,const NewActorFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~NewActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){central.OnNewActor(a);};

private:
    Central& central;
}; // end class NewActorFunctor

class DeleteActorFunctor : public DatabaseFunctor
{
public:
    DeleteActorFunctor(Central& s):central(s){};
    explicit DeleteActorFunctor(Central& c,const DeleteActorFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~DeleteActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){central.OnDeleteActor(a);};

private:
    Central& central;
}; // end class DeleteActorFunctor

class ReassignActorFunctor : public DatabaseFunctor
{
public:
    ReassignActorFunctor(Central& s):central(s){};
    explicit ReassignActorFunctor(Central& c,const ReassignActorFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~ReassignActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){central.OnReassignActor(a);};

private:
    Central& central;
}; // end class ReassignActorFunctor

class MaintenanceIntervalDoneFunctor : public DatabaseFunctor
{
public:
    MaintenanceIntervalDoneFunctor(Central& s):central(s){};
    explicit MaintenanceIntervalDoneFunctor(Central& c,const MaintenanceIntervalDoneFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~MaintenanceIntervalDoneFunctor(){};


protected:
    virtual void DoIt(void){central.OnMaintenanceIntervalDone();};

private:
    Central& central;
}; // end class MaintenanceIntervalDoneFunctor

class SharenetMessageFunctor : public DatabaseFunctor
{
public:
    SharenetMessageFunctor(Central& s):central(s){};
    explicit SharenetMessageFunctor(Central& c,const SharenetMessageFunctor& s) : central(c),DatabaseFunctor(s){};
    virtual ~SharenetMessageFunctor(){};


protected:
    virtual void DoIt(const void* p,const unsigned int len){central.OnSharenetMessage(p,len);};

private:
    Central& central;
}; // end class SharenetMessageFunctor

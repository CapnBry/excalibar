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
#include <list>
#include <istream>
#include <string>

// predefine
class CheyenneTime;
class Actor;
class Database;

namespace csl
{
// predefine
class CSLScriptHost;
class CSLKeyboard;

struct EXECUTE_PARAMS
{
    CSLScriptHost* script_host;
    const CheyenneTime* current_time;
    const Actor* followed_actor;
    const Actor* targetted_actor;
    CSLKeyboard* keyboard;
    const Database* database;
};

class CSLCommandAPI
{
public:
    
    // first bool is success/failure
    // second is move to next command/stay on this command
    typedef std::pair<bool,bool> EXECUTE_STATUS; 
    
    CSLCommandAPI(){};
    virtual ~CSLCommandAPI(){};
    
    virtual bool Extract(std::istream& arg_stream)=0;
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params)=0;
    
    virtual csl::CSLCommandAPI* Clone(void)const=0;
    
protected:
private:

}; // end CSLCommandAPI

class CheckTargetHealthAndCall : public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new CheckTargetHealthAndCall(*this));};
protected:
private:
    int Health1;
    int Health2;
    int Health3;
    std::string Cmd1;
    std::string Cmd2;
    std::string Cmd3;
    bool bInvoked; // we have to act a little like CallScript
}; // end class CheckTargetHealthAndCall

class SetReferenceActor : public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new SetReferenceActor(*this));};
protected:
private:
    std::string Name;
}; // end class SetReferenceActor

class MoveToPoint : public CSLCommandAPI
{
public:
    MoveToPoint(){head_point=0;};
    ~MoveToPoint(){delete head_point;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToPoint(*this));};
protected:
private:
    float GetDistanceFromGoal(const Actor* reference)const;
    void Reinit(csl::EXECUTE_PARAMS& params);
    int x;
    int y;
    double time_limit;
    double start_time;
    double last_heading_check;
    bool moving;
    bool turning;
    bool close;
    csl::CSLCommandAPI* head_point;
}; // end class MoveToPoint

class MoveToActor : public CSLCommandAPI
{
public:
    MoveToActor(){proxy=0;};
    ~MoveToActor(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToActor(*this));};
protected:
private:
    std::string Name;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class MoveToActor

class MoveToTarget: public CSLCommandAPI
{
public:
    MoveToTarget(){proxy=0;};
    ~MoveToTarget(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToTarget(*this));};
protected:
private:
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class MoveToTarget

class MoveToPointRelative : public CSLCommandAPI
{
public:
    MoveToPointRelative(){proxy=0;};
    ~MoveToPointRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToPointRelative(*this));};
protected:
private:
    int x_relative;
    int y_relative;
    double  time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class MoveToPointRelative

class MoveToActorRelative : public CSLCommandAPI
{
public:
    MoveToActorRelative(){proxy=0;};
    ~MoveToActorRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToActorRelative(*this));};
protected:
private:
    std::string Name;
    float angle_radians;
    float distance;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class MoveToActorRelative

class MoveToTargetRelative : public CSLCommandAPI
{
public:
    MoveToTargetRelative(){proxy=0;};
    ~MoveToTargetRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new MoveToTargetRelative(*this));};
protected:
private:
    float angle_degrees;
    float distance;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class MoveToTargetRelative

class HeadTo : public CSLCommandAPI
{
public:
    HeadTo(){proxy=0;};
    ~HeadTo(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadTo(*this));};
protected:
private:
    float SetTurnDir(const float final_heading,const float current_heading);
    float heading_radians;
    double time_limit;
    double start_time;
    bool turn_left;
    csl::CSLCommandAPI* proxy;
}; // end class HeadTo

class HeadPoint : public CSLCommandAPI
{
public:
    HeadPoint(){proxy=0;};
    ~HeadPoint(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadPoint(*this));};
protected:
private:
    int x;
    int y;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadPoint

class HeadActor : public CSLCommandAPI
{
public:
    HeadActor(){proxy=0;};
    ~HeadActor(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadActor(*this));};
protected:
private:
    std::string Name;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadActor

class HeadTarget: public CSLCommandAPI
{
public:
    HeadTarget(){proxy=0;};
    ~HeadTarget(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadTarget(*this));};
protected:
private:
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadTarget

class HeadPointRelative : public CSLCommandAPI
{
public:
    HeadPointRelative(){proxy=0;};
    ~HeadPointRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadPointRelative(*this));};
protected:
private:
    int x;
    int y;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadPointRelative

class HeadActorRelative : public CSLCommandAPI
{
public:
    HeadActorRelative(){proxy=0;};
    ~HeadActorRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadActorRelative(*this));};
protected:
private:
    std::string Name;
    float angle_radians;
    float distance;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadActorRelative

class HeadTargetRelative : public CSLCommandAPI
{
public:
    HeadTargetRelative(){proxy=0;};
    ~HeadTargetRelative(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new HeadTargetRelative(*this));};
protected:
private:
    float angle_degrees;
    float distance;
    double time_limit;
    csl::CSLCommandAPI* proxy;
}; // end class HeadTargetRelative

class KeyboardPress: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardPress(*this));};
protected:
private:
    char ch;
}; // end class KeyboardPress

class KeyboardHold: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardHold(*this));};
protected:
private:
    char ch;
}; // end class KeyboardHold

class KeyboardRelease: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardRelease(*this));};
protected:
private:
    char ch;
}; // end class KeyboardRelease


class KeyboardString: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardString(*this));};
protected:
private:

    std::string Output;
}; // end class KeyboardString

class KeyboardVKey: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardVKey(*this));};
protected:
private:

    std::string VKey;
}; // end class KeyboardVKey

class KeyboardVKeyHold: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardVKeyHold(*this));};
protected:
private:

    std::string VKey;
}; // end class KeyboardVKeyHold

class KeyboardVKeyRelease: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new KeyboardVKeyRelease(*this));};
protected:
private:

    std::string VKey;
}; // end class KeyboardVKeyRelease

class CallScript: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new CallScript(*this));};
protected:
private:

    std::string ToCall;
    bool bInvoked;
}; // end class CallScript

class ExecuteScript: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new ExecuteScript(*this));};
protected:
private:

    std::string ToExecute;
    bool bInvoked;
}; // end class ExecuteScript

class RestartScript: public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new RestartScript(*this));};
protected:
private:
}; // end class RestartScript

class Delay : public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new Delay(*this));};
protected:
private:

    double param;
    double start_time;
}; // end class DebugString

class DebugString : public CSLCommandAPI
{
public:
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new DebugString(*this));};
protected:
private:

    std::string Output;
}; // end class DebugString

class InterceptActor : public CSLCommandAPI
{
public:
    InterceptActor(){move_point=0;};
    ~InterceptActor(){delete move_point;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new InterceptActor(*this));};
protected:
private:
    void Reinit(csl::EXECUTE_PARAMS& params);
    bool DoIntercept(csl::EXECUTE_PARAMS& params,const Actor& Target);
    
    std::string Name;
    double time_limit;
    double reference_velocity;
    double start_time;
    double last_target_check_time;
    float intercept_x;
    float intercept_y;
    csl::CSLCommandAPI* move_point;
}; // end class InterceptActor

class InterceptTarget : public CSLCommandAPI
{
public:
    InterceptTarget(){proxy=0;};
    ~InterceptTarget(){delete proxy;};
    virtual bool Extract(std::istream& arg_stream);
    virtual csl::CSLCommandAPI::EXECUTE_STATUS Execute(csl::EXECUTE_PARAMS& params);
    virtual csl::CSLCommandAPI* Clone(void)const{return(new InterceptTarget(*this));};
protected:
private:
    double time_limit;
    double reference_velocity;
    csl::CSLCommandAPI* proxy;
}; // end class InterceptTarget

class CSLSubroutine
{
public:
    CSLSubroutine()
    {
        // init to end
        command_iterator=Commands.end();
    };
    
    // we have to break the rules a little 
    // bit here: if a subroutine is being copied
    // but the source of the copy has been/is
    // being executed, then what do we set the
    // iterator to? The answer is that we must set
    // it to the end and live with an incomplete copy.
    CSLSubroutine(const CSLSubroutine& s)
    {
        // set
        set(s);
        // init to end
        command_iterator=Commands.end();
    }
    
    // we have to break the rules a little 
    // bit here: if a subroutine is being copied
    // but the source of the copy has been/is
    // being executed, then what do we set the
    // iterator to? The answer is that we must set
    // it to the end and live with an incomplete copy.
    CSLSubroutine& operator=(const CSLSubroutine& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }
    virtual ~CSLSubroutine()
    {
        std::list<CSLCommandAPI*>::iterator it;
        while(Commands.begin()!=Commands.end())
            {
            // save pointer
            CSLCommandAPI* cmd=*Commands.begin();
            // erase
            Commands.erase(Commands.begin());
            // delete
            delete cmd;
            }
    };
    void InsertCommand(CSLCommandAPI* cmd)
    {
        // we take ownership here
        Commands.insert(Commands.end(),cmd);
    }
    
    void Init(void)
    {
        // once the commands are loaded, we 
        // set the iterator to the beginning
        // so call Init() after all commands
        // have been loaded
        command_iterator=Commands.begin();
    }
    
    csl::CSLCommandAPI::EXECUTE_STATUS CSLSubroutine::Execute(csl::EXECUTE_PARAMS& params);
    std::string GetName(void)const{return(Name);};
    void SetName(const std::string& s){Name=s;};
    
protected:
private:
    void set(const CSLSubroutine& s)
    {
        // iterate and clone
        std::list<CSLCommandAPI*>::const_iterator it;
        for(it=s.Commands.begin();it != s.Commands.end();++it)
            {
            // insert Clone
            Commands.insert(Commands.end(),(*it)->Clone());
            }
        
        // set name
        Name=s.Name;
        
        // set to end :(
        command_iterator=Commands.end();
    }
    std::list<CSLCommandAPI*> Commands;
    std::list<CSLCommandAPI*>::iterator command_iterator;
    
    std::string Name;
}; // end class CSLSubroutine
} // end namespace csl

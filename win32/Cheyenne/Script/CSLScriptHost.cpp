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
#include <sstream>
#include "CSLScriptHost.h"
#include "..\Utils\Logger.h"
#include "..\Utils\Times.h"

extern CheyenneClock Clock;
extern logger_t Logger;

namespace csl
{
class ExecuteScriptCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        // skip ws and get script to execute
        std::string cmd;
        arg_stream >> std::ws >> cmd >> std::ws;
        
        if(host->ExecuteScript(cmd))
            {
            result_stream << "Executing \"" << cmd << "\" was successful.\r\n\r\n>";
            }
        else
            {
            result_stream << "Executing \"" << cmd << "\" FAILED.\r\n\r\n>";
            }
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new ExecuteScriptCmd(*this));};
}; // end ExecuteScriptCmd

class StopScriptCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        // skip ws and get script to execute
        std::string cmd;
        arg_stream >> std::ws >> cmd >> std::ws;
        
        if(host->StopScript(cmd))
            {
            result_stream << "Stop \"" << cmd << "\" was successful.\r\n\r\n>";
            }
        else
            {
            result_stream << "Stop \"" << cmd << "\" FAILED.\r\n\r\n>";
            }
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new StopScriptCmd(*this));};
}; // end StopScriptCmd

class StopAllScriptsCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        host->StopAllScripts();
        result_stream << "Stop all scripts was successful.\r\n\r\n>";
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new StopAllScriptsCmd(*this));};
}; // end StopAllScriptsCmd

class GetRunningScriptsCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        std::list<std::string> script_list;
        
        // get running scripts
        host->GetRunningScripts(script_list);        
        
        // send 'em back
        result_stream << unsigned int(script_list.size()) << " running scripts\r\n";
        std::list<std::string>::const_iterator it;
        for(it=script_list.begin();it!=script_list.end();++it)
            {
            result_stream << it->c_str() << " \r\n";
            }
        result_stream << "\r\n>";
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new GetRunningScriptsCmd(*this));};
}; // end GetRunningScriptsCmd

class GetAvailableScriptsCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        std::list<std::string> script_list;
        
        // get running scripts
        host->GetAvailableScripts(script_list);        
        
        // send 'em back
        result_stream << unsigned int(script_list.size()) << " available scripts\r\n";
        std::list<std::string>::const_iterator it;
        for(it=script_list.begin();it!=script_list.end();++it)
            {
            result_stream << *it << " \r\n";
            }
        result_stream << "\r\n>";
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new GetAvailableScriptsCmd(*this));};
}; // end GetAvailableScriptsCmd

class ReloadScriptsCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        // reload scripts
        host->ReloadScripts();
        
        // print new available script list
        std::list<std::string> script_list;
        
        // get running scripts
        host->GetAvailableScripts(script_list);        
        
        // send 'em back
        result_stream << unsigned int(script_list.size()) << " reload: available scripts\r\n";
        std::list<std::string>::const_iterator it;
        for(it=script_list.begin();it!=script_list.end();++it)
            {
            result_stream << *it << " \r\n";
            }
        result_stream << "\r\n\r\n>";
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new ReloadScriptsCmd(*this));};
}; // end ReloadScriptsCmd

class ExecCommandCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        // make a subroutine out of the command
        csl::CSLSubroutine* cmd=host->LoadSubroutine(arg_stream);
        
        // we own cmd now
        
        if(cmd)
            {
            if(host->ExecuteScript(cmd))
                {
                // host owns cmd now
                result_stream << "Executing \"" << cmd->GetName() << "\" was successful.\r\n\r\n>";
                }
            else
                {
                // we are responsible for deleting cmd
                delete cmd;
                result_stream << "Executing \"" << cmd->GetName() << "\" FAILED.\r\n\r\n>";
                }
            } // end if cmd
        else
            {
            // no cmd
            result_stream << "Parsing FAILED.\r\n\r\n>";
            } // end else not cmd
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new ExecCommandCmd(*this));};
}; // end ExecCommandCmd

class HelpCmd : public csl::CSLHostCommandAPI
{
public:
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)
    {
        result_stream << "CSLHostCommand help:\r\n"
                      << "ExecuteScript <script_name>\r\n"
                      << "GetRunningScripts\r\n"
                      << "GetAvailableScripts\r\n"
                      << "StopScript <script_name>\r\n"
                      << "StopAllScripts\r\n"
                      << "ReloadScripts\r\n"
                      << "ExecCommand\r\n"
                      << "Help, help, or ?\r\n"
                      << "I do not handle backspaces particularly well, sorry.\r\n\r\n>";
    }
    virtual csl::CSLHostCommandAPI* Clone(void)const{return(new HelpCmd(*this));};
}; // end HelpCmd

CSLScriptHost::CSLScriptHost()
{
    // init to end
    current_script=ScriptList.end();
    
    MessageInputFifo=0;
    MessageOutputFifo=0;
    database=0;
    
    // init host commands
    HostCommands.insert(hostcmd_value("ExecuteScript",new csl::ExecuteScriptCmd));
    HostCommands.insert(hostcmd_value("GetRunningScripts",new csl::GetRunningScriptsCmd));
    HostCommands.insert(hostcmd_value("GetAvailableScripts",new csl::GetAvailableScriptsCmd));
    HostCommands.insert(hostcmd_value("StopScript",new csl::StopScriptCmd));
    HostCommands.insert(hostcmd_value("StopAllScripts",new csl::StopAllScriptsCmd));
    HostCommands.insert(hostcmd_value("ReloadScripts",new csl::ReloadScriptsCmd));
    HostCommands.insert(hostcmd_value("ExecCommand",new csl::ExecCommandCmd));
    HostCommands.insert(hostcmd_value("Help",new csl::HelpCmd));
    HostCommands.insert(hostcmd_value("help",new csl::HelpCmd));
    HostCommands.insert(hostcmd_value("?",new csl::HelpCmd));
} // end CSLScriptHost

CSLScriptHost::~CSLScriptHost()
{
    // erase any remaining commands
    while(ScriptList.begin() != ScriptList.end())
        {
        // clean the stack
        CleanStack(*ScriptList.begin(),false);
        
        // erase it
        ScriptList.erase(ScriptList.begin());
        }
    
    // erase the host command list
    while(HostCommands.begin() != HostCommands.end())
        {
        // delete command
        delete HostCommands.begin()->second;
        HostCommands.erase(HostCommands.begin());
        }
    
} // end ~CSLScriptHost

void CSLScriptHost::GetRunningScripts(std::list<std::string>& output)const
{
    // lock
    AutoLock al(MyLock);
    
    const_script_list_iterator it=ScriptList.begin();
    
    while(it != ScriptList.end())
        {
        // insert script name at end of list
        output.insert(output.end(),it->top()->GetName());
        
        // go to next
        ++it;
        } // end for every script on the list
    
    // done
    return;
} // end GetRunningScripts

bool CSLScriptHost::StopScript(const std::string script_name)
{
    // we stop the entire script-stack
    // lock
    AutoLock al(MyLock);
    
    // find script by name
    script_list_iterator it=ScriptList.begin();
    
    while(it != ScriptList.end())
        {
        // check name
        if(it->top()->GetName() == script_name)
            {
            // stop this script (and trace)
            CleanStack(*it,true);
            
            // erase it
            ScriptList.erase(it);
            
            // restart script list
            current_script=ScriptList.begin();
            
            // done
            return(true);
            }
        
        // go to next
        ++it;
        } // end for every script on the list

    // did not find script
    return(false);
} // end StopScript

void CSLScriptHost::StopAllScripts(void)
{
    // we stop the entire list of script-stacks
    // lock
    AutoLock al(MyLock);
    
    while(ScriptList.begin() != ScriptList.end())
        {
        // stop this script (and trace)
        CleanStack(*ScriptList.begin(),true);
        
        // erase it
        ScriptList.erase(ScriptList.begin());
        
        } // end for every script on the list

    // restart script list
    current_script=ScriptList.begin();
    
    // done
    return;
} // end StopAllScripts

void CSLScriptHost::CleanStack(stack_t& st,bool bTrace)
{
    while(!st.empty())
        {
        csl::CSLSubroutine* sub=st.top();
        st.pop();
        if(bTrace)
            {
            ::Logger << "Subroutine: " << sub->GetName() << "\n";
            }
        delete sub;
        }
} // end CleanStack

bool CSLScriptHost::ExecuteScript(csl::CSLSubroutine* sub)
{
    // init subroutine
    sub->Init();
    
    // create a temporary stack and put
    // the subroutine on it
    stack_t temp_stack;
    temp_stack.push(sub);
    
    // add to the list
    ScriptList.insert(ScriptList.end(),temp_stack);
    
    // done
    return(true);
} // end ExecuteScript

bool CSLScriptHost::ExecuteScript(const std::string& script_name)
{
    // lock
    AutoLock al(MyLock);
    
    // execute script: create a new 
    // subroutine stack on the script list
    // and add the named subroutine to it
    
    // load subroutine
    CSLSubroutine* sub=ScriptLoader.LoadSubroutine(script_name);
    
    if(!sub)
        {
        // woops
        return(false);
        }

    // return execute status
    return(ExecuteScript(sub));
} // end ExecuteScript

bool CSLScriptHost::CallScript(const std::string& script_name,csl::EXECUTE_PARAMS& current_params)
{
    // lock
    AutoLock al(MyLock);

    // call script: push a new 
    // subroutine on the current
    // stack
    
    // check that we have a valid stack to work with
    if(current_script == ScriptList.end())
        {
        // do execute instead
        return(ExecuteScript(script_name));
        }

    // load subroutine
    CSLSubroutine* sub=ScriptLoader.LoadSubroutine(script_name);
    
    if(!sub)
        {
        // woops
        return(false);
        }

    // add to current stack
    (*current_script).push(sub);
    
    // done
    return(true);
} // end CallScript

bool CSLScriptHost::RestartScript(void)
{
    // lock
    AutoLock al(MyLock);

    // check that we have a valid stack to work with
    if(current_script == ScriptList.end())
        {
        // woops, cant do this if there is not
        // a script running!
        return(false);
        }

    // tell current subroutine on
    // "top of stack" to Init() -- 
    // this causes it to start executing
    // commands from the beginning
    (*current_script).top()->Init();

    // done
    return(true);
} // end RestartScript

DWORD CSLScriptHost::Run(const bool& bContinue)
{
    LOG_FUNC << "thread id " << GetCurrentThreadId() << " created\n";
    
    // recover the go param to get the message fifos --
    std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>* param;

    param=static_cast<std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>*>(GoParam);
    MessageInputFifo=param->first;
    MessageOutputFifo=param->second;
    
    // don't need this anymore
    delete param;
    
    // timing variables
    CheyenneTime LastRefresh=::Clock.Current();
    const CheyenneTime RefreshInterval(0.25);

    while(bContinue)
        {
        // wait for clock tick
        Clock.NewTimeSignal.wait(1000);
        
        // execute commands from fifo
        ExecFifoCommands();
        
        // only refresh every RefreshInterval
        if(LastRefresh + RefreshInterval <= ::Clock.Current())
            {
            // save last refresh
            LastRefresh=::Clock.Current();
            
            // refresh actors
            RefreshActors();
            }
        
        // execute scripts
        ExecScripts();
        } // end forever

    LOG_FUNC << "thread id " << GetCurrentThreadId() << " exiting\n";

    // done
    return(0);
} // end Run

void CSLScriptHost::ExecScripts(void)
{
    // lock
    AutoLock al(MyLock);
    
    // build params
    csl::EXECUTE_PARAMS script_params;
    
    ::CheyenneTime CurrentTime(::Clock.Current());
    
    script_params.script_host=this;
    script_params.current_time=&CurrentTime;
    script_params.keyboard=&Keyboard;
    script_params.followed_actor=&ReferenceActor;
    script_params.targetted_actor=&TargettedActor;
    script_params.database=database;
    
    // execute subroutines on the stacks in the list
    current_script=ScriptList.begin();
    
    while(current_script != ScriptList.end())
        {
        // execute script
        csl::CSLCommandAPI::EXECUTE_STATUS status=
            current_script->top()->Execute(script_params);
        
        // check status
        if(!status.first)
            {
            // first status == false means that
            // an error occured in the script,
            // we terminate this script
            
            // clean and trace stack
            ::Logger << "[CSLScriptHost::ExecScripts] Run-time script error! Reverse stack trace:\n";
            CleanStack(*current_script,true);
            
            // erase this subroutine stack from the list
            // to make the code simple, we bail on the rest
            // of the list this time around -- this will
            // cause the remainder of the commands on the 
            // list to NOT get executed this clock tick!
            ScriptList.erase(current_script);
            break;
            } // end if !status.first
        else if(status.second)
            {
            // second status == true means that
            // this subroutine is complete and can
            // be removed from the stack.
            // to make the code simple, we bail on the rest
            // of the list this time around -- this will
            // cause the remainder of the commands on the 
            // list to NOT get executed this clock tick!
            csl::CSLSubroutine* sub=current_script->top();
            current_script->pop();
            delete sub;
            
            if(current_script->empty())
                {
                // no more commands in this script, we 
                // can remove it from the list
                ScriptList.erase(current_script);
                }
            break;
            } // end else if status.second
            
        // go to next
        ++current_script;
        } // end for every script in the list
    
    // make sure it points to the end
    current_script=ScriptList.end();
    
    // done
    return;
} // end ExecScripts

void CSLScriptHost::ExecFifoCommands(void)
{
    std::string cmd;
    std::stringstream args;
    std::stringstream res;
    
    while(std::string* cmd_str=MessageInputFifo->Pop())
        {
        // clear streams
        args.seekg(0);
        args.seekp(0);
        args.str("");
        args.clear();

        res.seekg(0);
        res.seekp(0);
        res.str("");
        res.clear();
        
        // get command
        args << *cmd_str;
        args >> cmd >> std::ws;
        
        // find command
        hostcmd_iterator it=HostCommands.find(cmd);
        if(it!=HostCommands.end())
            {
            // valid command, execute it
            it->second->Execute(args,res,this);
            }
        else
            {
            // not a valid command
            res << "\"" << cmd << "\" is not a valid command\r\n>";
            }
        
        // send results
        if(res.str().length() != 0)
            {
            // send result back
            MessageOutputFifo->Push(new std::string(res.str()));
            }
        
        // done with this
        delete cmd_str;
        } // end for every message on the fifo
        
    // done
    return;
} // end ExecFifoCommands

void CSLScriptHost::SetReferenceActor(const Actor& NewReferenceActor)
{
    // lock
    AutoLock al(MyLock);

    // copy
    ReferenceActor=NewReferenceActor;
    
    // get target
    TargettedActor=database->CopyActorById(ReferenceActor.GetTargetId());
    
    // done
    return;
} // end SetReferenceActor

void CSLScriptHost::RefreshActors(void)
{
    // lock
    AutoLock al(MyLock);

    // copy by id returns an empty actor if the one we 
    // are looking for is not found
    
    // refresh followed actor
    ReferenceActor=database->CopyActorById(ReferenceActor.GetInfoId());
    
    // refresh target
    TargettedActor=database->CopyActorById(ReferenceActor.GetTargetId());

    // done
    return;
} // end RefreshActors

} // end namespace csl
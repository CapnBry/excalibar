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

#include "..\Utils\threads.h" // for thread def
#include "..\Utils\times.h" // for time def
#include "..\Utils\locks.h" // for locks def
#include "..\Utils\tsdeque.h" // for tsdeque
#include "..\Script\CSLLoader.h"
#include "..\Script\CSLKeyboard.h"
#include "..\Script\CSLCommand.h"
#include "..\Database\database.h" // for database
#include <list>
#include <stack>
#include <map>

namespace csl
{
class CSLScriptHost; // predefine
class CSLHostCommandAPI
{
public:
    CSLHostCommandAPI(){};
    virtual ~CSLHostCommandAPI(){};
    
    virtual void Execute(std::istream& arg_stream,std::ostream& result_stream,csl::CSLScriptHost* host)=0;
    virtual csl::CSLHostCommandAPI* Clone(void)const=0;
    
protected:
private:
}; // end class CSLHostCommand

class CSLScriptHost : public Thread
{
public:
    typedef std::stack<csl::CSLSubroutine*> stack_t;
    typedef std::list<stack_t> script_list_t;
    typedef script_list_t::const_iterator const_script_list_iterator;
    typedef script_list_t::iterator script_list_iterator;
    
    typedef std::map<std::string,csl::CSLHostCommandAPI*> hostcmd_t;
    typedef hostcmd_t::const_iterator const_hostcmd_iterator;
    typedef hostcmd_t::iterator hostcmd_iterator;
    typedef hostcmd_t::value_type hostcmd_value;

    CSLScriptHost();
    virtual ~CSLScriptHost();
    
    // script api
    bool ExecuteScript(const std::string& script_name);
    bool CallScript(const std::string& script_name,csl::EXECUTE_PARAMS& current_params);
    bool RestartScript(void);
    void GetRunningScripts(std::list<std::string>& output)const;
    bool StopScript(const std::string script_name);
    
    void StopAllScripts(void);
    void GetAvailableScripts(std::list<std::string>& names)const{ScriptLoader.GetAvailableScripts(names);};
    
    // api specifically for commands to interface with me
    void SetReferenceActor(const Actor& NewReferenceActor);
    
    // api to update the database, if it ever changes, we need to be told
    void ChangeDatabase(Database* pdb){database=pdb;};
    
protected:
private:
    virtual DWORD Run(const bool& bContinue);
    void ExecScripts(void);
    void ExecFifoCommands(void);
    void RefreshActors(void);
    
    void CleanStack(stack_t& st,bool bTrace);
    
    script_list_t ScriptList;
    csl::CSLLoader ScriptLoader;
    csl::CSLKeyboard Keyboard;
    script_list_iterator current_script;
    
    hostcmd_t HostCommands;
    
    tsfifo<std::string*>* MessageInputFifo;
    tsfifo<std::string*>* MessageOutputFifo;

    // actors
    Actor ReferenceActor;
    Actor TargettedActor;
    
    // reference to database
    Database* database;
    
    mutable MutexLock MyLock; // conceptually const
}; // end class CSLScriptHost


} // end namespace csl
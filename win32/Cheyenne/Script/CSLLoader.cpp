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
#include <winsock2.h> // for windows.h find files
#include "CSLLoader.h"
#include "..\Script\CSLCommand.h" // for script command defs
#include "..\Utils\Logger.h" // for logger def
#include <fstream> // for file stream defs
#include <sstream> // for string stream defs
#include <stdexcept> // for exceptions

extern logger_t Logger;
extern std::string InitialDir;

namespace csl
{
CSLLoader::CSLLoader()
{
    // init
    Init();
    
}  // end CSLLoader
CSLLoader::~CSLLoader()
{
    // remove all commands
    while(CmdLookup.begin() != CmdLookup.end())
        {
        lookup_iterator it=CmdLookup.begin();
        delete it->second;
        CmdLookup.erase(it);
        }
    
    // remove cached subroutines
    while(SubCache.begin() != SubCache.end())
        {
        cache_iterator it=SubCache.begin();
        delete it->second.second; // we can delete NULL pointers...
        SubCache.erase(it);
        }
} // end ~CSLLoader

void CSLLoader::Init(void)
{
    // init dictionary
    CmdLookup.insert(lookup_value("SetReferenceActor",new csl::SetReferenceActor));
    /*
    params:
    actor_name
    */
    CmdLookup.insert(lookup_value("MoveToPoint",new csl::MoveToPoint));
    /*
    params:
    x y time_limit
    */
    CmdLookup.insert(lookup_value("MoveToActor",new csl::MoveToActor));
    /*
    params:
    name time_limit
    */
    CmdLookup.insert(lookup_value("MoveToTarget",new csl::MoveToTarget));
    /*
    params:
    time_limit
    */
    CmdLookup.insert(lookup_value("MoveToPointRelative",new csl::MoveToPointRelative));
    /*
    params:
    x_relative y_relative time_limit
    */
    CmdLookup.insert(lookup_value("MoveToActorRelative",new csl::MoveToActorRelative));
    /*
    params:
    name angle_degrees distance_from time_limit
    */
    CmdLookup.insert(lookup_value("MoveToTargetRelative",new csl::MoveToTargetRelative));
    /*
    params:
    angle_degrees distance_from time_limit
    */
    CmdLookup.insert(lookup_value("HeadTo",new csl::HeadTo));
    /*
    params:
    heading_degrees time_limit
    */
    CmdLookup.insert(lookup_value("HeadPoint",new csl::HeadPoint));
    /*
    params:
    x y time_limit
    */
    CmdLookup.insert(lookup_value("HeadActor",new csl::HeadActor));
    /*
    params:
    name time_limit
    */
    CmdLookup.insert(lookup_value("HeadTarget",new csl::HeadTarget));
    /*
    params:
    time_limit
    */
    CmdLookup.insert(lookup_value("HeadPointRelative",new csl::HeadPointRelative));
    /*
    params:
    x_relative y_relative time_limit
    */
    CmdLookup.insert(lookup_value("HeadActorRelative",new csl::HeadActorRelative));
    /*
    params:
    name angle_degrees distance_from time_limit
    */
    CmdLookup.insert(lookup_value("HeadTargetRelative",new csl::HeadTargetRelative));
    /*
    params:
    angle_degrees distance_from time_limit
    */
    CmdLookup.insert(lookup_value("KeyboardPress",new csl::KeyboardPress));
    /*
    params:
    alphanumeric
    */
    CmdLookup.insert(lookup_value("KeyboardHold",new csl::KeyboardHold));
    /*
    params:
    alphanumeric
    */
    CmdLookup.insert(lookup_value("KeyboardRelease",new csl::KeyboardRelease));
    /*
    params:
    alphanumeric
    */
    CmdLookup.insert(lookup_value("KeyboardString",new csl::KeyboardString));
    /*
    params:
    string
    */
    CmdLookup.insert(lookup_value("KeyboardVKey",new csl::KeyboardVKey));
    /*
    params:
    <VK_*>
    */
    CmdLookup.insert(lookup_value("KeyboardVKeyHold",new csl::KeyboardVKey));
    /*
    params:
    <VK_*>
    */
    CmdLookup.insert(lookup_value("KeyboardVKeyRelease",new csl::KeyboardVKey));
    /*
    params:
    <VK_*>
    */
    CmdLookup.insert(lookup_value("ExecuteScript",new csl::ExecuteScript));
    /*
    params:
    script_name
    */
    CmdLookup.insert(lookup_value("CallScript",new csl::CallScript));
    /*
    params:
    script_name
    */
    CmdLookup.insert(lookup_value("RestartScript",new csl::RestartScript));
    /*
    params:
    <none>
    */
    CmdLookup.insert(lookup_value("Delay",new csl::Delay));
    /*
    params:
    time_seconds
    */
    CmdLookup.insert(lookup_value("DebugString",new csl::DebugString));
    /*
    params:
    string
    */
    CmdLookup.insert(lookup_value("CheckTargetHealthAndCall",new csl::CheckTargetHealthAndCall));
    /*
    params:
    health1 cmd1 health2 cmd2 health3 cmd3
    constraints: health1 > health2 > health3
    */
    
    // init cache
    WIN32_FIND_DATA find_data;
    ZeroMemory(&find_data,sizeof(find_data));
    std::ostringstream search_criteria;
    std::ostringstream initial_directory;
    std::ostringstream full_file_name;

    initial_directory << ::InitialDir << "scripts\\";
    search_criteria << initial_directory.str() << "*.csl";
    
    HANDLE hFind=FindFirstFile(search_criteria.str().c_str(),&find_data);
    
    if(!hFind || hFind==INVALID_HANDLE_VALUE)
        {
        // none found
        return;
        }
    
    // find all files (including the one we just found)
    do
        {
        // make sure its not a directory
        if(!(find_data.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY))
            {
            full_file_name.seekp(0);
            full_file_name.str("");
            full_file_name.clear();
            
            // get file name
            full_file_name << initial_directory.str() << find_data.cFileName;
            
            // get subroutine name
            std::string sub_name=GetSubroutineNameFromFile(full_file_name.str());
            
            // add to cache with pointer value of 0 so we know to load it later
            // when it is first used -- save a bit of memory doing this
            SubCache.insert(cache_value(sub_name,std::make_pair(full_file_name.str(),static_cast<csl::CSLSubroutine*>(0))));
            } // end if not a directory
        } while(FindNextFile(hFind,&find_data) != 0);
    
    // done with this
    FindClose(hFind);
    
    // done
    return;
} // end Init

void CSLLoader::GetAvailableScripts(std::list<std::string>& names)const
{
    // loop and get script names
    const_cache_iterator it=SubCache.begin();
    
    while(it != SubCache.end())
        {
        // add to end of list
        names.insert(names.end(),it->first);
        
        // go to next
        ++it;
        } // end for all available scripts
    
    // done
    return;
} // end GetAvailableScripts

std::string CSLLoader::GetSubroutineNameFromFile(const std::string& file_name)const
{
    std::ifstream file(file_name.c_str());
    
    if(!file.is_open())
        {
        // this had better have succeeded, we just found the damn file!
        throw(std::runtime_error("[CSLLoader::GetSubroutineNameFromFile] file not found!"));
        }
    
    // get subroutine name
    std::string sub_name;
    file >> std::ws >> sub_name;
    
    // done, return name
    return(sub_name);
} // end GetSubroutineNameFromFile

CSLSubroutine* CSLLoader::LoadSubroutine(const std::string& script_name)
{
    // first, check if it is in our cache
    cache_iterator cache_it=SubCache.find(script_name);
    if(cache_it==SubCache.end())
        {
        // that script does not exist!
        ::Logger << "[CSLLoader::LoadSubroutine] \"" << script_name << "\" does not exist!\n";
        return(0);
        }
    
    if(cache_it->second.second)
        {
        // second is the subroutine pointer: it is not NULL,
        // so we have loaded this before. Return a copy
        // of the pre-existing subroutine instead of loading
        // off disk again
        csl::CSLSubroutine* sub=new csl::CSLSubroutine(*cache_it->second.second);
        // init it
        sub->Init();
        // done
        return(sub);
        }
    
    // it was not in the cache, load from disk

    // first is the file name of the script file
    std::string ScriptFile(cache_it->second.first);
    
    // open file for it
    std::ifstream file(ScriptFile.c_str());
    
    if(!file.is_open())
        {
        ::Logger << "[CSLLoader::LoadSubroutine] unable to open " << ScriptFile.c_str() << "\n";
        return(0);
        }
    
    // get name
    std::string Name;
    file >> std::ws >> Name >> std::ws;
    
    if(Name.length() == 0)
        {
        ::Logger << "[CSLLoader::LoadSubroutine]  Names must be non-zero length!" << std::endl;
        return(0);
        }

    // create new subroutine
    CSLSubroutine* sub=new CSLSubroutine;
    
    // set name
    sub->SetName(Name);
    
    // string stream for each line
    std::stringstream line;
    
    // get every command
    while(file.good() && !file.eof())
        {
        // reset line
        line.seekg(0);
        line.seekp(0);
        line.clear();
        line.str("");
        
        // get line
        // get command
        std::string text_cmd;
        std::string line_string;
        std::getline(file,line_string);
        line.str(line_string);
        
        line >> text_cmd;// >> std::ws;
        
        // check for comment, if so then 
        // we ignore this line
        // also, if the line is empty, we ignore it
        if(text_cmd != "//" && text_cmd.length() != 0)
            {    
            // look it up
            const_lookup_iterator it=CmdLookup.find(text_cmd);
            
            // make sure we found it
            if(it == CmdLookup.end())
                {
                // didn't find it!
                ::Logger << "[CSLLoader::LoadSubroutine] While parsing sub routine \"" << sub->GetName() << "\"\n";
                ::Logger << "[CSLLoader::LoadSubroutine] Parse error on line \"" << line_string << "\"."
                        << " Unable to find command \"" << text_cmd << "\".\n";
                
                
                // delete this, we're done
                delete sub;
                return(0);
                } // end if command not found
                
            // command was found, clone our dictionary entry
            csl::CSLCommandAPI* cmd=it->second->Clone();
            
            // tell it to get its arguments
            if(!cmd->Extract(line))
                {
                ::Logger << "[CSLLoader::LoadSubroutine] While parsing sub routine \"" << sub->GetName() << "\"\n";
                ::Logger << "[CSLLoader::LoadSubroutine] Parse error on line \"" << line_string << "\"."
                        << " Unable to get arguments for command \"" << text_cmd << "\".\n";
                
                // delete these, we're done
                delete cmd;
                delete sub;
                return(0);
                }
            
            // add command to subroutine
            sub->InsertCommand(cmd);
            } // end if ! comment
            
        // skip whitespace
        file >> std::ws;
        } // end while commands remain in the file
    
    // tell the subroutine to init
    sub->Init();
    
    // add a copy to cache
    cache_it->second.second=new csl::CSLSubroutine(*sub);

    // done
    return(sub);
} // end LoadSubroutine

} // end namespace csl

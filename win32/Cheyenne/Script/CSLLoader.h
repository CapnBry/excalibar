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
#include <string>

namespace csl
{
class CSLCommandAPI;
class CSLSubroutine;

class CSLLoader
{
public:
    // for the command lookup table
    typedef std::map<std::string,csl::CSLCommandAPI*> lookup_t;
    typedef lookup_t::const_iterator const_lookup_iterator;
    typedef lookup_t::iterator lookup_iterator;
    typedef lookup_t::value_type lookup_value;
    
    // for the cache
    typedef std::pair<std::string,CSLSubroutine*> file_sub_pair;
    typedef std::map<std::string,file_sub_pair> cache_t;
    typedef cache_t::const_iterator const_cache_iterator;
    typedef cache_t::iterator cache_iterator;
    typedef cache_t::value_type cache_value;

    CSLLoader();
    virtual ~CSLLoader();
    
    CSLSubroutine* LoadSubroutine(const std::string& script_name);
    
    void GetAvailableScripts(std::list<std::string>& names)const;
    
protected:
private:
    void Init(void);
    std::string GetSubroutineNameFromFile(const std::string& file_name)const;
    
    lookup_t CmdLookup;
    cache_t SubCache;
}; // end class CSLLoader
} // end namespace csl
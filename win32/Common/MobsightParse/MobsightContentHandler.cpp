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
#include "stdafx.h"
#include "MobsightContentHandler.h"
#include <algorithm>

using std::wcout;
using std::endl;

MobsightContentHandlerImpl::MobsightContentHandlerImpl()
{
    bIdStore=false;
    bNameStore=false;
} // end MobsightContentHandlerImpl

MobsightContentHandlerImpl::~MobsightContentHandlerImpl()
{
} // end ~MobsightContentHandlerImpl

HRESULT MobsightContentHandlerImpl::startDocument()
{
    // done
    return(S_OK);
} // end startDocument

HRESULT MobsightContentHandlerImpl::endDocument()
{
    // done
    return(S_OK);
} // end endDocument
HRESULT MobsightContentHandlerImpl::startElement
    (
    const wchar_t * wszNamespaceUri,
    int cchNamespaceUri,
    const wchar_t * wszLocalName,
    int cchLocalName,
    const wchar_t* wszQName,
    int cchQName,
    ISAXAttributes * pAttributes 
    )
{
    std::wstringstream LocalName;
    
    for(int i=0;i<cchLocalName;++i)
        {
        LocalName << wszLocalName[i];
        }

    if(LocalName.str() == L"mob_id")
        {
        bIdStore=true;
        cur_id.seekg(0);
        cur_id.seekp(0);
        cur_id.clear();
        cur_id.str(std::wstring(L""));
        return(S_OK);
        }

    bIdStore=false;
    
    if(LocalName.str() == L"mobname")
        {
        bNameStore=true;
        cur_name.seekg(0);
        cur_name.seekp(0);
        cur_name.clear();
        cur_name.str(std::wstring(L""));
        return(S_OK);
        }
    
    bNameStore=false;

    // done
    return(S_OK);
} // end startElement

HRESULT MobsightContentHandlerImpl::endElement
    (
    const wchar_t * wszNamespaceUri,
    int cchNamespaceUri,
    const wchar_t * wszLocalName,
    int cchLocalName,
    const wchar_t* wszQName,
    int cchQName
    )
{
    std::wstringstream LocalName;
    
    for(int i=0;i<cchLocalName;++i)
        {
        LocalName << wszLocalName[i];
        }

    if(LocalName.str() == L"mob_id")
        {
        bIdStore=false;
        cur_id >> CurrentMob.id;
        return(S_OK);
        }

    if(LocalName.str() == L"mobname")
        {
        bNameStore=false;
        CurrentMob.name=cur_name.str();
        return(S_OK);
        }
    
    if(LocalName.str() == L"mob")
        {
        // end of mob, add CurrentMob to list
        MobList.insert(MobList.end(),CurrentMob);
        }

    // done
    return(S_OK);
} // end endElement

HRESULT MobsightContentHandlerImpl::characters
    (
    const wchar_t * pwchChars,
    int cchChars
    )
{
    std::wstringstream CurChars;
    
    for(int i=0;i<cchChars;++i)
        {
        CurChars << pwchChars[i];
        }
        
    if(bIdStore)
        {
        cur_id << CurChars.str();
        return(S_OK);
        }
    
    if(bNameStore)
        {
        cur_name << CurChars.str();
        return(S_OK);
        }
    
    // done
    return(S_OK);
} // end characters

void MobsightContentHandlerImpl::MakeMobList
    (
    std::list<NarrowMobDef>& OutputList
    )const
{
    std::list<MobDef>::const_iterator mobdef_it;
    // storage for value
    NarrowMobDef value;
    
    // this code was hoarked from a Boost mailing list. 
    // check www.boost.org for the Boost library
    
    typedef std::ctype<wchar_t> ctype_t;
    const ctype_t& ct = std::use_facet<ctype_t>(std::locale());
    
    for(mobdef_it=MobList.begin();mobdef_it!=MobList.end();++mobdef_it)
        {
        // assign value
        std::string result(mobdef_it->name.size(),std::string::value_type(0));
        
        ct.narrow(mobdef_it->name.data(), mobdef_it->name.data() +
        mobdef_it->name.size(),'@',&(*result.begin()));

        value.name=result;
        value.id=mobdef_it->id;
        
        // add to list
        OutputList.insert(OutputList.end(),value);
        } // end for each MobDef in the list
    
    // done
    return;
} // end MakeMobList

void MobsightContentHandlerImpl::FreeMobList(std::list<NarrowMobDef>& OutputList)const
{
    OutputList.clear();
} // end FreeMobList

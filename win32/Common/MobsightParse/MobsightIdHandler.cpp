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
#include "MobsightIdHandler.h"
#include <algorithm>

using std::wcout;
using std::endl;

MobsightIdHandlerImpl::MobsightIdHandlerImpl()
{
    bIdStore=false;
    bNameStore=false;
} // end MobsightIdHandlerImpl

MobsightIdHandlerImpl::~MobsightIdHandlerImpl()
{
} // end ~MobsightIdHandlerImpl

HRESULT MobsightIdHandlerImpl::startDocument()
{
    // done
    return(S_OK);
} // end startDocument

HRESULT MobsightIdHandlerImpl::endDocument()
{
    // done
    return(S_OK);
} // end endDocument
HRESULT MobsightIdHandlerImpl::startElement
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
    
    if(LocalName.str() == L"name")
        {
        bNameStore=true;
        cur_name.seekg(0);
        cur_name.seekp(0);
        cur_name.clear();
        cur_name.str(std::wstring(L""));
        return(S_OK);
        }
    
    bNameStore=false;

    if(LocalName.str() == L"zone")
        {
        bZoneStore=true;
        cur_zone.seekg(0);
        cur_zone.seekp(0);
        cur_zone.clear();
        cur_zone.str(std::wstring(L""));
        return(S_OK);
        }
    
    bZoneStore=false;
    
    if(LocalName.str() == L"x")
        {
        bXStore=true;
        cur_x.seekg(0);
        cur_x.seekp(0);
        cur_x.clear();
        cur_x.str(std::wstring(L""));
        return(S_OK);
        }
    
    bXStore=false;
    
    if(LocalName.str() == L"y")
        {
        bYStore=true;
        cur_y.seekg(0);
        cur_y.seekp(0);
        cur_y.clear();
        cur_y.str(std::wstring(L""));
        return(S_OK);
        }
    
    bYStore=false;
    
    if(LocalName.str() == L"z")
        {
        bZStore=true;
        cur_z.seekg(0);
        cur_z.seekp(0);
        cur_z.clear();
        cur_z.str(std::wstring(L""));
        return(S_OK);
        }
    
    bZStore=false;
    
    if(LocalName.str() == L"mob")
        {
        // delete current loc's
        CurrentMob.loc_list.clear();
        return(S_OK);
        }

    // done
    return(S_OK);
} // end startElement

HRESULT MobsightIdHandlerImpl::endElement
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

    if(LocalName.str() == L"name")
        {
        bNameStore=false;
        CurrentMob.name=cur_name.str();
        return(S_OK);
        }
    
    if(LocalName.str() == L"zone")
        {
        bZoneStore=false;
        cur_zone >> CurrentMob.CurrentLoc.zone;
        return(S_OK);
        }
    
    if(LocalName.str() == L"x")
        {
        bXStore=false;
        cur_x >> CurrentMob.CurrentLoc.x;
        return(S_OK);
        }
    
    if(LocalName.str() == L"y")
        {
        bYStore=false;
        cur_y >> CurrentMob.CurrentLoc.y;
        return(S_OK);
        }

    if(LocalName.str() == L"z")
        {
        bZStore=false;
        cur_z >> CurrentMob.CurrentLoc.z;
        return(S_OK);
        }

    if(LocalName.str() == L"mobseen")
        {
        // add to current loc list
        CurrentMob.loc_list.insert(CurrentMob.loc_list.end(),CurrentMob.CurrentLoc);
        return(S_OK);
        }

    // done
    return(S_OK);
} // end endElement

HRESULT MobsightIdHandlerImpl::characters
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
    
    if(bZoneStore)
        {
        cur_zone << CurChars.str();
        return(S_OK);
        }
    
    if(bXStore)
        {
        cur_x << CurChars.str();
        return(S_OK);
        }

    if(bYStore)
        {
        cur_y << CurChars.str();
        return(S_OK);
        }

    if(bZStore)
        {
        cur_z << CurChars.str();
        return(S_OK);
        }
        
    // done
    return(S_OK);
} // end characters

bool MobsightIdHandlerImpl::GetMobInfo
    (
    NarrowIdDef& MobInfo
    )const
{
    // this code was hoarked from a Boost mailing list. 
    // check www.boost.org for the Boost library
    
    typedef std::ctype<wchar_t> ctype_t;
    const ctype_t& ct = std::use_facet<ctype_t>(std::locale());
    
    // assign value
    std::string result(CurrentMob.name.size(),std::string::value_type(0));
    
    ct.narrow(CurrentMob.name.data(), CurrentMob.name.data() +
    CurrentMob.name.size(),'@',&(*result.begin()));

    MobInfo.name=result;
    MobInfo.id=CurrentMob.id;
    //MobInfo.zone=CurrentMob.zone;
    MobInfo.loc_list=CurrentMob.loc_list;
    
    // done
    return(true);
} // end MakeMobList

void MobsightIdHandlerImpl::FreeMobInfo(NarrowIdDef& MobInfo)const
{
    MobInfo.loc_list.clear();
} // end FreeMobInfo

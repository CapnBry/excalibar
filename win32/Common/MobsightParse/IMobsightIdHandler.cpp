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
#define _WIN32_DCOM 
#include "stdafx.h"
#include "MobsightParse.h"
#include "IMobsightIdHandler.h"
#include "MobsightIdHandler.h"

#include <sstream>

MobsightIdHandler::MobsightIdHandler() :
    impl(new MobsightIdHandlerImpl)
{
    // init COM
    CoInitializeEx(NULL,COINIT_MULTITHREADED);
} // end MobsightIdHandler

MobsightIdHandler::~MobsightIdHandler()
{
    delete impl;

    // un-init COM
    CoUninitialize();
} // end ~MobsightIdHandler

bool MobsightIdHandler::GetMobInfo
    (
    const std::wstring& MobsightURL,
    const int Id,
    NarrowIdDef& MobInfo
    )
{
    bool success=true;
    
    // first part of the url we need to go to
    std::wstringstream url;

    // document reader
    ISAXXMLReader* reader=NULL;
    
    // create reader
    /*
    HRESULT hr=CoCreateInstance
        (
        __uuidof(SAXXMLReader),
        NULL,
        CLSCTX_ALL,
        __uuidof(ISAXXMLReader),
        (void**)&reader
        );
    */
    HRESULT hr=CoCreateInstance
        (
        CLSID_SAXXMLReader30,
        NULL,
        CLSCTX_ALL,
        __uuidof(ISAXXMLReader),
        (void**)&reader
        );
    
    if(hr != S_OK)
        {
        success=false;
        goto clean_exit;
        }

    // assign the content handler
    hr=reader->putContentHandler(impl);
    if(hr != S_OK)
        {
        success=false;
        goto clean_exit;
        }
        
    // build url
    url << MobsightURL
        << L"?f=xml"
        << L"&m=" << Id
        << L"&agent=cheyenne";
    
    // parse the xml at this url
    hr=reader->parseURL(url.str().c_str());
    
    if(hr != S_OK)
        {
        success=false;
        goto clean_exit;
        }
    
    impl->GetMobInfo(MobInfo);
    
    clean_exit:
    
    if(reader)
        {
        // release reader
        reader->Release();
        }
    
    // done
    return(success);
} // end BuildMobList

void MobsightIdHandler::FreeMobInfo(NarrowIdDef& MobInfo)const
{
    impl->FreeMobInfo(MobInfo);
} // end FreeMobInfo

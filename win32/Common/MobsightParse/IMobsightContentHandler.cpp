#define _WIN32_DCOM 
#include "stdafx.h"
#include "MobsightParse.h"
#include "IMobsightContentHandler.h"
#include "MobsightContentHandler.h"

#include <sstream>

MobsightContentHandler::MobsightContentHandler() :
    impl(new MobsightContentHandlerImpl)
{
    // init COM
    CoInitializeEx(NULL,COINIT_MULTITHREADED);
} // end MobsightContentHandler

MobsightContentHandler::~MobsightContentHandler()
{
    delete impl;

    // un-init COM
    CoUninitialize();
} // end ~MobsightContentHandler
bool MobsightContentHandler::BuildMobList
    (
    const std::wstring& MobsightURL,
    const REALM realm,
    const unsigned char MinLevel,
    const unsigned char MaxLevel,
    std::list<NarrowMobDef>& List
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
        << L"&r=" << realm
        << L"&minlevel=" << MinLevel
        << L"&maxlevel=" << MaxLevel
        << L"&agent=cheyenne";
    
    // parse the xml at this url
    hr=reader->parseURL(url.str().c_str());
    
    if(hr != S_OK)
        {
        success=false;
        goto clean_exit;
        }
    
    // make the mob list
    impl->MakeMobList(List);
    
    clean_exit:
    
    if(reader)
        {
        // release reader
        reader->Release();
        }
    
    // done
    return(success);
} // end BuildMobList

void MobsightContentHandler::FreeMobList(std::list<NarrowMobDef>& List)const
{
    impl->FreeMobList(List);
} // end FreeMobList

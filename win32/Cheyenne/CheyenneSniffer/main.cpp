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
#define MAIN_FUNCTION

#include "global.h" // include project globals
#include "central.h" // include for main class
#include "..\Utils\codeutils.h" // for console redirection functions
#include <commctrl.h> // for common control defs

int WINAPI WinMain
    (
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow
    )
{
    // init common controls (why do I have to friggin do this M$???!?)
    INITCOMMONCONTROLSEX cc;
    cc.dwSize=sizeof(cc);
    cc.dwICC=ICC_LISTVIEW_CLASSES;
    InitCommonControlsEx(&cc);
    
    // raise process priority a bit
    // so we can contend with the
    // daoc client for the cpu
    SetPriorityClass(GetCurrentProcess(),HIGH_PRIORITY_CLASS);
    
    // attach a console and redirect printf/cout to it
    AttachConsole();
    
    // initialize the logger
    ::Logger.Init("cheyenne_sniffer.log");
    
    // load config
    g_Config.Load();
    
    WPARAM result=-1;
    
    {
    // save initial directory
    TCHAR* cd=new TCHAR[MAX_PATH+1];
    GetCurrentDirectory(MAX_PATH,cd);
    ::InitialDir=cd;
    
    delete cd;

    // make sure theres a trailing '\' in there
    if(::InitialDir.at(::InitialDir.length()-1) != '\\')
        {
        ::InitialDir+='\\';
        }
    }

    ::Logger << "[main] Hi!\n";

    // start clock
    ::Clock.Go();

    // read zone file
    if(!::Zones.ReadZoneFile())
        {
        MessageBox(NULL,"Zone file (mapinfo.txt) not found!","ERROR",MB_ICONEXCLAMATION|MB_OK);
        goto Exit;
        }
    
    // read region offsets file
    if(!::Zones.ReadRegionOffsets())
        {
        MessageBox(NULL,"Region offset file (regionoffsets.txt) not found!","ERROR",MB_ICONEXCLAMATION|MB_OK);
        goto Exit;
        }
    
    // main loop
    {
    Central central;
    result=central.Go(hInstance);
    }

    // exit
    Exit:

    // save config
    g_Config.Save();

    // stop the clock
    ::Clock.Stop();

    // save config
    //Config.Save();

    Logger << "[main] Bye!\n";

    // deallocate the console
    DetachConsole();
    
    // done
    return((int)result);
} // end WinMain

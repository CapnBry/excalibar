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

#include "global.h"
#include <commctrl.h>

#include "sharenet.h"
#include "central.h"

int WINAPI WinMain
    (
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow
    )
{
    WPARAM result=-1;

    Logger.Init("ShareServer.log");
    
    // allocate a console for debug output
    AttachConsole();

    LOG_FUNC << "Hi!\n";
    
    // init common controls
    {
    INITCOMMONCONTROLSEX xx;
    xx.dwSize=sizeof(xx);
    xx.dwICC=ICC_LISTVIEW_CLASSES;
    InitCommonControlsEx(&xx);
    }

    // load config
    Config.Load();

    // start clock
    Clock.Go();
    
    // enter main loop
    {
    Central* ShareCentral=new Central;

    result=ShareCentral->Go(hInstance);
    
    delete ShareCentral;
    }

    // stop the clock
    Clock.Stop();

    // save config
    Config.Save();
    
    // exit
    //Exit:

    LOG_FUNC << "Bye!\n";

    // free console
    DetachConsole();

    // done
    return(result);
} // end WinMain

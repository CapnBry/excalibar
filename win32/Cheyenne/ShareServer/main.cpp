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

// get rid of the stupid
// "identifier truncated" warnings
#pragma warning(disable : 4786)

#include "global.h"
#include <commctrl.h>

#include "sharenet.h"
#include "central.h"

// grab these for console output redirection
// this makes std::cout (and other stuff) work for GUI applications :)
#include <io.h>
#include <fcntl.h>
#include <conio.h>
// end console output redirection includes

void AttachConsole(void);
void DetachConsole(void);

int WINAPI WinMain
    (
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow
    )
{
    WPARAM result=-1;

    // allocate a console for debug output
    #ifdef CHEYENNE_DEBUG
    AttachConsole();
    #endif

    std::cout << "[main] Hi!\n";
    
    // init common controls
    {
    INITCOMMONCONTROLSEX xx;
    xx.dwSize=sizeof(xx);
    xx.dwICC=ICC_LISTVIEW_CLASSES;
    InitCommonControlsEx(&xx);
    }

    // start clock
    Clock.Go();
    
    // load config
    Config.Load();

    // enter main loop
    {
    Central* ShareCentral=new Central;

    result=ShareCentral->Go(hInstance);
    
    delete ShareCentral;
    }

    // save config
    Config.Save();
    
    // stop the clock
    Clock.Stop();

    // exit
    //Exit:

    std::cout << "[main] Bye!\n";

    // free console
    #ifdef CHEYENNE_DEBUG
    DetachConsole();
    #endif

    // done
    return(result);
} // end WinMain

void AttachConsole(void)
{
    // code copied from Microsoft Knowledge Base Article - 105305
    // and the silly article had an error in it (!)
    int hCrt;
    FILE *hf;

    AllocConsole();
    hCrt = _open_osfhandle((long) GetStdHandle(STD_OUTPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stdout = *hf;
    setvbuf( stdout, NULL, _IONBF, 0 );

    // do it again for stdin
    hCrt = _open_osfhandle((long) GetStdHandle(STD_INPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "r" );
    *stdin = *hf;
    setvbuf( stdin, NULL, _IONBF, 0 );

    // and again for stderr
    hCrt = _open_osfhandle((long) GetStdHandle(STD_ERROR_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stderr = *hf;
    setvbuf( stderr, NULL, _IONBF, 0 );

    // done
    return;
} // end AtttachConsole

void DetachConsole(void)
{
    // free the console
    FreeConsole();

    // done
    return;
} // end DetatchConsole

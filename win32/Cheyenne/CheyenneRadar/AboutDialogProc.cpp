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

#include "..\Utils\CodeUtils.h" // nice dialog utilities in here
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\mapinfo.h" // for the zones class
#include "..\Utils\times.h" // for the clock
#include "..\GLPPI\GLPPI.h" // for actor render prefs
#include "config.h" // for the config defs
#include "resource.h"
#include "main.h" 

INT_PTR CALLBACK AboutDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            TCHAR StrBuf[65535];
            LoadString(GetModuleHandle(NULL),IDS_DESCRIPTION,&StrBuf[0],sizeof(StrBuf)/sizeof(TCHAR));
            SetWindowText(GetDlgItem(hWnd,IDC_DESCRIPTION),&StrBuf[0]);
            
            LoadString(GetModuleHandle(NULL),IDS_COPYRIGHT,&StrBuf[0],sizeof(StrBuf)/sizeof(TCHAR));
            SetWindowText(GetDlgItem(hWnd,IDC_COPYRIGHT),&StrBuf[0]);
            
            LoadString(GetModuleHandle(NULL),IDS_LICENSE,&StrBuf[0],sizeof(StrBuf)/sizeof(TCHAR));
            HWND h=GetDlgItem(hWnd,IDC_LICENSE);
            SetWindowText(h,&StrBuf[0]);
            }
            break;
            
        case WM_COMMAND:
            // look at control id
            switch(LOWORD(wParam))
                {
                case IDOK:
                    EndDialog(hWnd,IDOK);
                    break;
                    
                default:
                    return(FALSE); // did not process message
                    break;
                } // end switch control ID
            break;
            
        default:
            return(FALSE); // did not process message
        } // end switch uMSg
    // return TRUE (processed message)
    return(TRUE);
} // end AboutDialogProc

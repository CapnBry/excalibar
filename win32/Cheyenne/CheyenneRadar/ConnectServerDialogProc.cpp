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

#include <iostream>
#include <string>
#include <algorithm>
#include <limits>
#include <sstream>
#include "..\Utils\CodeUtils.h" // nice dialog utilities in here
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\mapinfo.h" // for the zones class
#include "..\Utils\times.h" // for the clock
#include "..\GLPPI\GLPPI.h" // for actor render prefs
#include "config.h" // for the config defs
#include "resource.h"
#include "main.h" 

// this dialog box expects a std::pair<std::string,unsigned short>* as its lParam to WM_INITDIALOG!
// on IDOK, the passed in pointer is populated with the user selection

typedef std::pair<std::string,unsigned short> dialog_result_t;

void SyncDialogToConfig(HWND hWnd,const dialog_result_t *const param);

INT_PTR CALLBACK ConnectServerDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    dialog_result_t* param=(dialog_result_t*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
    
    switch(uMsg)
        {
        case WM_INITDIALOG:
            // store
            SetWindowLongPtr(hWnd,GWLP_USERDATA,lParam);
            
            // re-get this
            param=(dialog_result_t*)lParam;
            
            // sync
            SyncDialogToConfig(hWnd,param);
            break;
            
        case WM_COMMAND:
            // look at control id
            switch(LOWORD(wParam))
                {
                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;
                
                case IDOK:
                    {
                    // get server
                    GET_EDIT_STRING(hWnd,IDC_SERVER,param->first);
                    
                    std::string sport;
                    GET_EDIT_STRING(hWnd,IDC_PORT,sport);
                    std::stringstream ss;
                    ss << sport;
                    long lport;
                    ss >> lport;
                    
                    if(lport < 1 || lport > 65535)
                        {
                        MessageBox(hWnd,"Port must be [1,65535]","Data Error",MB_OK);
                        break;
                        }
                    else
                        {
                        param->second=(unsigned short)lport;
                        EndDialog(hWnd,IDOK);
                        }
                    }
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
} // end ConfigDialogProc

void SyncDialogToConfig(HWND hWnd,const dialog_result_t* const param)
{
    // set server
    SET_EDIT_STRING(hWnd,IDC_SERVER,param->first);
    
    // set port
    std::ostringstream oss;
    oss << param->second;
    SET_EDIT_STRING(hWnd,IDC_PORT,oss.str());
    
    // done
    return;
} // end SyncDialogToConfig

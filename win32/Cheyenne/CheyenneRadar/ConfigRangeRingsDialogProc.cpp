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

#include <string>
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

// this dialog box expects a std::pair<bool,unsigned int>* as its lParam to WM_INITDIALOG!
// on IDOK, the passed in pointer is populated with the user selection

typedef std::pair<bool,float> dialog_result_t;

void SyncRangeRingDialogToConfig(HWND hWnd,const dialog_result_t* const param);

INT_PTR CALLBACK ConfigRangeRingsDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
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
            SyncRangeRingDialogToConfig(hWnd,param);
            
            // move
            POINT pt;
            ::GetCursorPos(&pt);
            ::SetWindowPos
                (
                hWnd,
                0,
                pt.x-100,
                pt.y-50,
                0,
                0,
                SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOSIZE
                );
            break;
            
        case WM_COMMAND:
            // look at control id
            switch(LOWORD(wParam))
                {
                case IDCANCEL:
                    // clear flag -- this will disable the range ring
                    param->first=false;
                    EndDialog(hWnd,IDCANCEL);
                    break;
                
                case IDOK:
                    {
                    // get range
                    std::string str;
                    GET_EDIT_STRING(hWnd,IDC_RANGE,str);
                    std::stringstream ss;
                    ss << str;
                    ss >> param->second;
                    
                    if(param->second <= 0)
                        {
                        MessageBox(hWnd,"Range must be > 0!", "Error",MB_OK);
                        }
                    else
                        {
                        // set flag -- this will enable the range ring
                        param->first=true;
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

void SyncRangeRingDialogToConfig(HWND hWnd,const dialog_result_t* const param)
{
    // set to initial range
    std::stringstream ss;
    ss << param->second;
    SET_EDIT_STRING(hWnd,IDC_RANGE,ss.str());
    
    // done
    return;
} // end SyncDialogToConfig

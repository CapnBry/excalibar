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
#include <winsock2.h>
#include <sstream> // for string stream defs
#include "config.h" // for the config def
#include "..\Utils\CodeUtils.h" // for window helpers
#include "..\Script\CSLScriptHost.h" // for scripting
#include "resource.h" // for control ids
#include <commctrl.h> // for common control defs

BOOL WINAPI SetServerPortDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    Config* param=reinterpret_cast<Config*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            // store this pointer
            SetWindowLongPtr(hWnd,GWLP_USERDATA,LONG_PTR(lParam));
            
            param=reinterpret_cast<Config*>(lParam);
            
            // set initial port
            std::ostringstream port_string;
            port_string << param->GetServerPort();
            
            SET_EDIT_STRING(hWnd,IDC_SERVERPORT,port_string.str());
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    unsigned short server_port;
                    std::string port_string;
                    
                    GET_EDIT_STRING(hWnd,IDC_SERVERPORT,port_string);
                    
                    // convert to ushort
                    std::stringstream ss;
                    ss << port_string;
                    ss >> server_port;
                    
                    // error check
                    if(server_port == 0)
                        {
                        MessageBox(hWnd,"Port must be (0,65535]","Data entry error",MB_OK);
                        break;
                        }
                    
                    // if changed, tell user to restart
                    if(server_port != param->GetServerPort())
                        {
                        MessageBox(hWnd,"Changing the server port requires CheyenneSniffer to be restarted","Please restart",MB_OK);
                        }
                    
                    // save
                    param->SetServerPort(server_port);
                    }
                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;
                        
                default:
                    return(FALSE);
                    break;
                }
            }
            break;

        default:
            return(FALSE); // did not process message
            break;
        } // end switch message
    
    return(TRUE); // TRUE for processed message
} // end SetServerPortDlgProc

BOOL WINAPI ActivateScriptDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    csl::CSLScriptHost* param=reinterpret_cast<csl::CSLScriptHost*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            // store this pointer
            SetWindowLongPtr(hWnd,GWLP_USERDATA,LONG_PTR(lParam));
            
            param=reinterpret_cast<csl::CSLScriptHost*>(lParam);
            
            // get list of available scripts
            std::list<std::string> available_scripts;
            std::list<std::string>::iterator it;
            param->GetAvailableScripts(available_scripts);
            
            LVITEM item;
            ZeroMemory(&item,sizeof(item));
            item.mask=LVIF_TEXT;
            
            for(it=available_scripts.begin();it != available_scripts.end();++it)
                {
                // populate
                item.pszText=const_cast<char*>(it->c_str());
                ListView_InsertItem(GetDlgItem(hWnd,IDC_AVAILABLESCRIPTS),&item);
                } // end for all available scripts

            // get list of running scripts
            std::list<std::string> running_scripts;
            param->GetRunningScripts(running_scripts);
            
            for(it=running_scripts.begin();it != running_scripts.end();++it)
                {
                // populate
                item.pszText=const_cast<char*>(it->c_str());
                ListView_InsertItem(GetDlgItem(hWnd,IDC_RUNNINGSCRIPTS),&item);
                } // end for all available scripts

            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    // get list of selected items
                    std::list<std::string> selected_list;
                    GET_LISTVIEW_SELECTED_ITEMS(hWnd,IDC_AVAILABLESCRIPTS,selected_list);
                    
                    // activate the scripts
                    std::list<std::string>::const_iterator it;
                    
                    for(it=selected_list.begin();it!=selected_list.end();++it)
                        {
                        // activate this script
                        param->ExecuteScript(*it);
                        }
                    }
                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;
                        
                default:
                    return(FALSE);
                    break;
                }
            }
            break;

        default:
            return(FALSE); // did not process message
            break;
        } // end switch message
    
    return(TRUE); // TRUE for processed message
} // end ActivateScriptDlgProc

BOOL WINAPI StopScriptDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    csl::CSLScriptHost* param=reinterpret_cast<csl::CSLScriptHost*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            // store this pointer
            SetWindowLongPtr(hWnd,GWLP_USERDATA,LONG_PTR(lParam));
            
            param=reinterpret_cast<csl::CSLScriptHost*>(lParam);

            // get list of running scripts
            std::list<std::string> running_scripts;
            std::list<std::string>::iterator it;
            param->GetRunningScripts(running_scripts);
            
            LVITEM item;
            ZeroMemory(&item,sizeof(item));
            item.mask=LVIF_TEXT;

            for(it=running_scripts.begin();it != running_scripts.end();++it)
                {
                // populate
                item.pszText=const_cast<char*>(it->c_str());
                ListView_InsertItem(GetDlgItem(hWnd,IDC_RUNNINGSCRIPTS),&item);
                } // end for all available scripts
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    // get list of selected items
                    std::list<std::string> selected_list;
                    GET_LISTVIEW_SELECTED_ITEMS(hWnd,IDC_RUNNINGSCRIPTS,selected_list);
                    
                    // stop the scripts
                    std::list<std::string>::const_iterator it;
                    
                    for(it=selected_list.begin();it!=selected_list.end();++it)
                        {
                        // stop this script
                        param->StopScript(*it);
                        }
                    }
                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;
                        
                default:
                    return(FALSE);
                    break;
                }
            }
            break;

        default:
            return(FALSE); // did not process message
            break;
        } // end switch message
    
    return(TRUE); // TRUE for processed message
} // end StopScriptDlgProc

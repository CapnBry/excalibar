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

#include "global.h"
#include "centraldialogs.h"
#include <commctrl.h>
#include <sstream>
#include "resource.h"

void SET_CHECK_BOOL(HWND hwnd,UINT control,bool bool_value)
{
    SendDlgItemMessage(hwnd,control,BM_SETCHECK,(LPARAM)(bool_value ? BST_CHECKED:BST_UNCHECKED),0);
}

bool GET_CHECK_BOOL(HWND hwnd,UINT control)
{
    return(SendDlgItemMessage(hwnd,control,BM_GETCHECK,0,0)==BST_CHECKED?true:false);
}

void SET_EDIT_STRING(HWND hwnd,UINT control,const std::string& std_str)
{
    SendDlgItemMessage(hwnd,control,WM_SETTEXT,0,(LPARAM)(std_str.c_str()));
}

void GET_EDIT_STRING(HWND hwnd,UINT control,std::string& std_str) 
{
    char* edit_str;
    LRESULT len=1+SendDlgItemMessage(hwnd,control,WM_GETTEXTLENGTH,0,0);
    edit_str=new char[len];
    edit_str[0]='\0';
    SendDlgItemMessage(hwnd,control,WM_GETTEXT,len,(WPARAM)(edit_str));
    std_str=edit_str;
    delete[] edit_str;
}

BOOL WINAPI AllowedHostsDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    ShareNetConfig* param=(ShareNetConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(ShareNetConfig*)lParam;
            
            // init the list
            std::ostringstream oss;
            ShareNetConfig::host_list_type::const_iterator it;
            
            for(it=param->GetAllowedHostsList().begin();it!=param->GetAllowedHostsList().end();++it)
                {
                oss.str("");
                oss.seekp(0);
                oss.clear();
                oss << it->first << "/" << it->second;
                SendDlgItemMessage(hWnd,IDC_CURRENTHOSTS,LB_INSERTSTRING,(WPARAM)-1,(LPARAM)oss.str().c_str());
                }
            
            // now: we have a temporart ShareNetConfig. We empty the
            // allowed hosts list and will repopulate it when we exit
            param->EraseAllowedHosts();
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    // repopulate the ShareNetConfig with the 
                    // current info in the listbox
                    LRESULT num_items=SendDlgItemMessage
                        (
                        hWnd,
                        IDC_CURRENTHOSTS,
                        LB_GETCOUNT,
                        0,
                        0
                        );
                    if(num_items != LB_ERR)
                        {
                        char lb_text[128]; // this is bigger than anything we put in there
                        std::ostringstream host_ss;
                        std::ostringstream mask_ss;
                        for(LRESULT count=0;count<num_items;++count)
                            {
                            // reset string streams
                            host_ss.str("");
                            host_ss.seekp(0);
                            host_ss.clear();
                            mask_ss.str("");
                            mask_ss.seekp(0);
                            mask_ss.clear();
                            
                            // get text
                            SendDlgItemMessage
                                (
                                hWnd,
                                IDC_CURRENTHOSTS,
                                LB_GETTEXT,
                                (WPARAM)count,
                                (LPARAM)lb_text
                                );
                            
                            // split at the "/" and get the host mask
                            int ndx=0;
                            while(lb_text[ndx]!='/')
                                {
                                host_ss << lb_text[ndx];
                                ++ndx;
                                }
                            // increment past the '/'
                            ++ndx;
                            
                            // do the net mask
                            while(lb_text[ndx]!='\0')
                                {
                                mask_ss << lb_text[ndx];
                                ++ndx;
                                }
                            
                            // have host and mask, 
                            // add them to the ShareNetConfig list
                            ShareNetConfig::host_list_value_type value;
                            value.first=host_ss.str();
                            value.second=mask_ss.str();
                            param->InsertAllowedHost(value);
                            } // end for each item
                        }
                    }
                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;

                case IDC_DELETE:
                    {
                    LRESULT cur_sel=SendDlgItemMessage(hWnd,IDC_CURRENTHOSTS,LB_GETCURSEL,0,0);
                    
                    if(cur_sel != LB_ERR)
                        {
                        // have valid current selection, delete it
                        // from the list
                        SendDlgItemMessage
                            (
                            hWnd,
                            IDC_CURRENTHOSTS,
                            LB_DELETESTRING,
                            (WPARAM)cur_sel,
                            0
                            );
                        }
                    }
                    break;
                    
                case IDC_TRANSFER:
                    {
                    char host[32];
                    char mask[32];
                    // get text
                    SendDlgItemMessage(hWnd,IDC_HOSTMASK,WM_GETTEXT,(WPARAM)sizeof(host),(LPARAM)host);
                    SendDlgItemMessage(hWnd,IDC_NETMASK,WM_GETTEXT,(WPARAM)sizeof(mask),(LPARAM)mask);
                    
                    if(strlen(host)==0 || strlen(mask)==0)
                        {
                        MessageBox(hWnd,"You must supply a host and a netmask","Error",MB_OK);
                        break;
                        }

                    std::ostringstream oss;
                    oss << host << "/" << mask;
                    SendDlgItemMessage(hWnd,IDC_CURRENTHOSTS,LB_INSERTSTRING,(WPARAM)-1,(LPARAM)oss.str().c_str());
                    }
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
} // end AllowedHostsDlgProc

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
#include "..\Utils\CodeUtils.h" // nice dialog utilities in here
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\mapinfo.h" // for the zones class
#include "..\Utils\times.h" // for the clock
#include "..\GLPPI\GLPPI.h" // for actor render prefs
#include "config.h" // for the config defs
#include "resource.h"
#include "main.h" 

void SyncDialogToConfig(HWND hWnd);

INT_PTR CALLBACK ConfigDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(uMsg)
        {
        case WM_INITDIALOG:
            SyncDialogToConfig(hWnd);
            break;
            
        case WM_CLOSE:
            // re-enable menu
            EnableMenuItem(GetMenu(GetParent(hWnd)),ID_CONTROLDISPLAY_OPENCONFIGDIALOG,MF_BYCOMMAND|MF_ENABLED);
            EndDialog(hWnd,IDCLOSE);
            break;
            
        case WM_COMMAND:
            // look at control id
            switch(LOWORD(wParam))
                {
                case IDC_RAISEPRIORITY:
                    ::RadarConfig.SetRaisePriority(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_LOADZONETEXTURES:
                    ::RadarConfig.SetUseZoneTextures(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_LOADVECTORMAPS:
                    ::RadarConfig.SetUseVectorMaps(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWFRIENDLYNAME:
                    ::RadarConfig.ModifyPrefsSameRealm().SetRenderName(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWFRIENDLYSURNAME:
                    ::RadarConfig.ModifyPrefsSameRealm().SetRenderSurname(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWFRIENDLYHEALTH:
                    ::RadarConfig.ModifyPrefsSameRealm().SetRenderHealth(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWFRIENDLYGUILD:
                    ::RadarConfig.ModifyPrefsSameRealm().SetRenderGuild(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWFRIENDLYLEVEL:
                    ::RadarConfig.ModifyPrefsSameRealm().SetRenderLevel(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWENEMYNAME:
                    ::RadarConfig.ModifyPrefsEnemyRealm().SetRenderName(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWENEMYSURNAME:
                    ::RadarConfig.ModifyPrefsEnemyRealm().SetRenderSurname(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWENEMYHEALTH:
                    ::RadarConfig.ModifyPrefsEnemyRealm().SetRenderHealth(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWENEMYGUILD:
                    ::RadarConfig.ModifyPrefsEnemyRealm().SetRenderGuild(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWENEMYLEVEL:
                    ::RadarConfig.ModifyPrefsEnemyRealm().SetRenderLevel(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBNAME:
                    ::RadarConfig.ModifyPrefsMob().SetRenderName(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBSURNAME:
                    ::RadarConfig.ModifyPrefsMob().SetRenderSurname(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBHEALTH:
                    ::RadarConfig.ModifyPrefsMob().SetRenderHealth(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBGUILD:
                    ::RadarConfig.ModifyPrefsMob().SetRenderGuild(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBLEVEL:
                    ::RadarConfig.ModifyPrefsMob().SetRenderLevel(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDCLOSE:
                    // re-enable menu
                    EnableMenuItem(GetMenu(GetParent(hWnd)),ID_CONTROLDISPLAY_OPENCONFIGDIALOG,MF_BYCOMMAND|MF_ENABLED);
                    EndDialog(hWnd,IDCLOSE);
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

void SyncDialogToConfig(HWND hWnd)
{
    // system group
    SET_CHECK_BOOL(hWnd,IDC_RAISEPRIORITY,::RadarConfig.GetRaisePriority());
    SET_CHECK_BOOL(hWnd,IDC_LOADZONETEXTURES,::RadarConfig.GetUseZoneTextures());
    SET_CHECK_BOOL(hWnd,IDC_LOADVECTORMAPS,::RadarConfig.GetUseVectorMaps());
    
    // friendly group
    SET_CHECK_BOOL(hWnd,IDC_SHOWFRIENDLYNAME,::RadarConfig.GetPrefsSameRealm().GetRenderName());
    SET_CHECK_BOOL(hWnd,IDC_SHOWFRIENDLYSURNAME,::RadarConfig.GetPrefsSameRealm().GetRenderSurname());
    SET_CHECK_BOOL(hWnd,IDC_SHOWFRIENDLYGUILD,::RadarConfig.GetPrefsSameRealm().GetRenderGuild());
    SET_CHECK_BOOL(hWnd,IDC_SHOWFRIENDLYHEALTH,::RadarConfig.GetPrefsSameRealm().GetRenderHealth());
    SET_CHECK_BOOL(hWnd,IDC_SHOWFRIENDLYLEVEL,::RadarConfig.GetPrefsSameRealm().GetRenderLevel());
    
    // enemy group
    SET_CHECK_BOOL(hWnd,IDC_SHOWENEMYNAME,::RadarConfig.GetPrefsEnemyRealm().GetRenderName());
    SET_CHECK_BOOL(hWnd,IDC_SHOWENEMYSURNAME,::RadarConfig.GetPrefsEnemyRealm().GetRenderSurname());
    SET_CHECK_BOOL(hWnd,IDC_SHOWENEMYGUILD,::RadarConfig.GetPrefsEnemyRealm().GetRenderGuild());
    SET_CHECK_BOOL(hWnd,IDC_SHOWENEMYHEALTH,::RadarConfig.GetPrefsEnemyRealm().GetRenderHealth());
    SET_CHECK_BOOL(hWnd,IDC_SHOWENEMYLEVEL,::RadarConfig.GetPrefsEnemyRealm().GetRenderLevel());
    
    // mob group
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBNAME,::RadarConfig.GetPrefsMob().GetRenderName());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBSURNAME,::RadarConfig.GetPrefsMob().GetRenderSurname());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBGUILD,::RadarConfig.GetPrefsMob().GetRenderGuild());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBHEALTH,::RadarConfig.GetPrefsMob().GetRenderHealth());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBLEVEL,::RadarConfig.GetPrefsMob().GetRenderLevel());
    
    // done
    return;
} // end SyncDialogToConfig
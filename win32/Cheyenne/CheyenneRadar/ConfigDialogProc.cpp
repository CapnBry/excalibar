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
#include <sstream>
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
void RangeRingDialog(HWND hWnd,UINT control,bool& enable,float& range);
void SimplifyLinesDialog(HWND hWnd,UINT control,bool& enable,double& tolerance);
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
                case IDC_SIMPLIFYLINES:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        SimplifyLinesDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifySimplifyLines(),
                            ::RadarConfig.ModifySimplifyLinesTolerance()
                            );
                        std::stringstream ss;
                        ss << "Simplify Vector Maps (" << ::RadarConfig.GetSimplifyLinesTolerance() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetSimplifyLines(false);
                        }
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
                case IDC_UPDATEONRENDER:
                    ::RadarConfig.SetUpdateActorsOnRender(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWALBS:
                    ::RadarConfig.SetShowAlbs(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWHIBS:
                    ::RadarConfig.SetShowHibs(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMIDS:
                    ::RadarConfig.SetShowMids(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWMOBS:
                    ::RadarConfig.SetShowMobs(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SAVE_DAOC_MESSAGES:
                    ::RadarConfig.SetSaveDAoCMessages(GET_CHECK_BOOL(hWnd,LOWORD(wParam)));
                    break;
                case IDC_SHOWRANGERING1:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing1(),
                            ::RadarConfig.ModifyRangeRingRange1()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #1 (" << ::RadarConfig.GetRangeRingRange1() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing1(false);
                        }
                    break;
                case IDC_SHOWRANGERING2:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing2(),
                            ::RadarConfig.ModifyRangeRingRange2()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #2 (" << ::RadarConfig.GetRangeRingRange2() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing2(false);
                        }
                    break;
                case IDC_SHOWRANGERING3:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing3(),
                            ::RadarConfig.ModifyRangeRingRange3()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #3 (" << ::RadarConfig.GetRangeRingRange3() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing3(false);
                        }
                    break;
                case IDC_SHOWRANGERING4:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing4(),
                            ::RadarConfig.ModifyRangeRingRange4()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #4 (" << ::RadarConfig.GetRangeRingRange4() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing4(false);
                        }
                    break;
                case IDC_SHOWRANGERING5:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing5(),
                            ::RadarConfig.ModifyRangeRingRange5()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #5 (" << ::RadarConfig.GetRangeRingRange5() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing5(false);
                        }
                    break;
                case IDC_SHOWRANGERING6:
                    if(GET_CHECK_BOOL(hWnd,LOWORD(wParam))==true)
                        {
                        RangeRingDialog
                            (
                            hWnd,LOWORD(wParam),
                            ::RadarConfig.ModifyShowRangeRing6(),
                            ::RadarConfig.ModifyRangeRingRange6()
                            );
                        std::stringstream ss;
                        ss << "Show Range Ring #6 (" << ::RadarConfig.GetRangeRingRange6() << ")";
                        SET_EDIT_STRING(hWnd,LOWORD(wParam),ss.str());
                        } // end if checked
                    else
                        {
                        ::RadarConfig.SetShowRangeRing6(false);
                        }
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
    std::stringstream ss; // temp storage for strings

    // system group
    SET_CHECK_BOOL(hWnd,IDC_RAISEPRIORITY,::RadarConfig.GetRaisePriority());
    SET_CHECK_BOOL(hWnd,IDC_LOADZONETEXTURES,::RadarConfig.GetUseZoneTextures());
    SET_CHECK_BOOL(hWnd,IDC_LOADVECTORMAPS,::RadarConfig.GetUseVectorMaps());
    SET_CHECK_BOOL(hWnd,IDC_SIMPLIFYLINES,::RadarConfig.GetSimplifyLines());
    ss << "Simplify Vector Maps (" << ::RadarConfig.GetSimplifyLinesTolerance() << ")";
    SET_EDIT_STRING(hWnd,IDC_SIMPLIFYLINES,ss.str());
    ss.str("");
    SET_CHECK_BOOL(hWnd,IDC_SAVE_DAOC_MESSAGES,::RadarConfig.GetSaveDAoCMessages());
    
    
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
    
    // performance/feedback group
    SET_CHECK_BOOL(hWnd,IDC_UPDATEONRENDER,::RadarConfig.GetUpdateActorsOnRender());
    SET_CHECK_BOOL(hWnd,IDC_SHOWALBS,::RadarConfig.GetShowAlbs());
    SET_CHECK_BOOL(hWnd,IDC_SHOWHIBS,::RadarConfig.GetShowHibs());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMIDS,::RadarConfig.GetShowMids());
    SET_CHECK_BOOL(hWnd,IDC_SHOWMOBS,::RadarConfig.GetShowMobs());

    // range rings group
    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING1,::RadarConfig.GetShowRangeRing1());
    ss << "Show Range Ring #1 (" << ::RadarConfig.GetRangeRingRange1() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING1,ss.str());
    ss.str("");

    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING2,::RadarConfig.GetShowRangeRing2());
    ss << "Show Range Ring #2 (" << ::RadarConfig.GetRangeRingRange2() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING2,ss.str());
    ss.str("");

    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING3,::RadarConfig.GetShowRangeRing3());
    ss << "Show Range Ring #3 (" << ::RadarConfig.GetRangeRingRange3() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING3,ss.str());
    ss.str("");

    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING4,::RadarConfig.GetShowRangeRing4());
    ss << "Show Range Ring #4 (" << ::RadarConfig.GetRangeRingRange4() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING4,ss.str());
    ss.str("");

    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING5,::RadarConfig.GetShowRangeRing5());
    ss << "Show Range Ring #5 (" << ::RadarConfig.GetRangeRingRange5() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING5,ss.str());
    ss.str("");

    SET_CHECK_BOOL(hWnd,IDC_SHOWRANGERING6,::RadarConfig.GetShowRangeRing6());
    ss << "Show Range Ring #6 (" << ::RadarConfig.GetRangeRingRange6() << ")";
    SET_EDIT_STRING(hWnd,IDC_SHOWRANGERING6,ss.str());
    ss.str("");
    
    // done
    return;
} // end SyncDialogToConfig

void RangeRingDialog(HWND hWnd,UINT control,bool& enable,float& range)
{
    std::pair<bool,float>param(true,range);
    
    INT_PTR result=DialogBoxParam
        (
        (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
        MAKEINTRESOURCE(IDD_RANGERING),
        hWnd,
        (DLGPROC)ConfigRangeRingsDialogProc,
        (LPARAM)&param
        );

    if(result==IDOK)
        {
        enable=true;
        range=param.second;
        }
    else
        {
        // disable
        enable=false;
        
        // uncheck the control
        SET_CHECK_BOOL(hWnd,control,false);
        }
    // done
    return;
} // end RangeRingDialog

void SimplifyLinesDialog(HWND hWnd,UINT control,bool& enable,double& tolerance)
{
    std::pair<bool,double>param(true,tolerance);
    
    INT_PTR result=DialogBoxParam
        (
        (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
        MAKEINTRESOURCE(IDD_SIMPLIFYLINES),
        hWnd,
        (DLGPROC)SimplifyLinesDialogProc,
        (LPARAM)&param
        );

    if(result==IDOK)
        {
        enable=true;
        tolerance=param.second;
        }
    else
        {
        // disable
        enable=false;
        
        // uncheck the control
        SET_CHECK_BOOL(hWnd,control,false);
        }
    // done
    return;
} // end SimplifyLinesDialog
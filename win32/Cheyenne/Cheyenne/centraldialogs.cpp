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
#include "database.h"
#include "centraldialogs.h"
#include "central.h"
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

bool GetOpenFileName
    (
    HWND hWndOwner,
    std::string& file_name,
    const char* filters
    )
{
    OPENFILENAME ofn;
    char name[MAX_PATH+1];
    ZeroMemory(&ofn,sizeof(ofn));
    
    strncpy(name,file_name.c_str(),MAX_PATH);
    
    ofn.lStructSize=sizeof(ofn);
    ofn.hwndOwner=hWndOwner;
    ofn.lpstrFilter=filters;
    ofn.lpstrFile=name;
    ofn.nMaxFile=MAX_PATH;
    ofn.lpstrTitle="Select File";
    ofn.Flags=OFN_EXPLORER|OFN_FILEMUSTEXIST|OFN_HIDEREADONLY|OFN_PATHMUSTEXIST;
    ofn.lpstrDefExt="wav";
    
    if(GetOpenFileName(&ofn))
        {
        file_name=ofn.lpstrFile;
        return(true);
        }
    else
        {
        return(false);
        }
} // end GetOpenFileName

class ComboActorPopulator
{
public:
    ComboActorPopulator(const ComboActorPopulator& s) : hComboBox(s.hComboBox),CurrentSel(s.CurrentSel){};
    explicit ComboActorPopulator(HWND h,unsigned int curr_sel):hComboBox(h),CurrentSel(curr_sel){};
    ~ComboActorPopulator(){}

    void operator()(const Database::actor_map_value& s)
    {
        int ndx=SendMessage(hComboBox,CB_ADDSTRING,0,(LPARAM)s.second.GetName().c_str());
        SendMessage(hComboBox,CB_SETITEMDATA,(WPARAM)ndx,(LPARAM)s.second.GetInfoId());
        
        if(s.second.GetInfoId()==CurrentSel)
            {
            // select this one
            SendMessage(hComboBox,CB_SETCURSEL,WPARAM(ndx),0);
            }
            
        // done
        return;
    }

protected:
private:
    ComboActorPopulator& operator=(const ComboActorPopulator& s); // disallow
    HWND hComboBox;
    unsigned int CurrentSel;
}; // end class ComboActorPopulator

BOOL WINAPI CameraFollowDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    std::pair<Database*,unsigned int>* param=(std::pair<Database*,unsigned int>*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(std::pair<Database*,unsigned int>*)lParam;

            param->first->IterateActors(ComboActorPopulator(GetDlgItem(hWnd,IDC_ACTOR_CB),param->second));
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    int sel=SendDlgItemMessage(hWnd,IDC_ACTOR_CB,CB_GETCURSEL,0,0);

                    if(sel == CB_ERR)
                        {
                        MessageBox(hWnd,"You must select an actor!","Error",MB_OK);
                        return(TRUE);
                        }

                    param->second=(unsigned int)SendDlgItemMessage(hWnd,IDC_ACTOR_CB,CB_GETITEMDATA,(WPARAM)sel,0);
                    }
                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;

                case IDC_FOLLOWNONE:
                    // set to 0 to disable following
                    param->second=0;
                    EndDialog(hWnd,IDOK);
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
} // end CameraFollowDlgProc

BOOL WINAPI ConfigDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    CheyenneConfig* param=(CheyenneConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(CheyenneConfig*)lParam;
            SendDlgItemMessage(hWnd,IDC_RAISEPRIORITY,BM_SETCHECK,(WPARAM)param->GetRaisePriority()?BST_CHECKED:BST_UNCHECKED,0);

            SET_CHECK_BOOL(hWnd,IDC_PRINTUNKNOWNPACKETS,param->GetLogUnknownPackets());
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    param->SetRaisePriority(SendDlgItemMessage(hWnd,IDC_RAISEPRIORITY,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);

                    // change priority here
                    if(param->GetRaisePriority())
                        {
                        SetPriorityClass(GetCurrentProcess(),HIGH_PRIORITY_CLASS);
                        }
                    else
                        {
                        SetPriorityClass(GetCurrentProcess(),NORMAL_PRIORITY_CLASS);
                        }

                    param->ModifyLogUnknownPackets()=GET_CHECK_BOOL(hWnd,IDC_PRINTUNKNOWNPACKETS);

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
} // end ConfigDlgProc

BOOL WINAPI ConfigDisplayDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    CheyenneConfig* param=(CheyenneConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(CheyenneConfig*)lParam;

            SendDlgItemMessage(hWnd,IDC_TEXTPPI,BM_SETCHECK,(WPARAM)param->GetPPIText()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_GUILDPPI,BM_SETCHECK,(WPARAM)param->GetGuildInPPI()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_PPISURNAME,BM_SETCHECK,(WPARAM)param->GetSurnameInPPI()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_PPILEVEL,BM_SETCHECK,(WPARAM)param->GetLevelInPPI()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_PPIHEALTH,BM_SETCHECK,(WPARAM)param->GetHealthInPPI()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_AUTOFOLLOWFIRST,BM_SETCHECK,(WPARAM)param->GetAutoFollowFirstActor()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_RENDERMOBS,BM_SETCHECK,(WPARAM)param->GetRenderMOBs()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_RENDERPLAYERS,BM_SETCHECK,(WPARAM)param->GetRenderPlayers()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_RENDEROBJECTS,BM_SETCHECK,(WPARAM)param->GetRenderObjects()?BST_CHECKED:BST_UNCHECKED,0);
            SendDlgItemMessage(hWnd,IDC_FOLLOWHEADING,BM_SETCHECK,(WPARAM)param->GetMatchFollowedHeading()?BST_CHECKED:BST_UNCHECKED,0);
            
            // use macro, this was developed later to make the above code more readable
            SET_CHECK_BOOL(hWnd,IDC_PPIDEAD,param->GetDrawDeadActors());
            SET_CHECK_BOOL(hWnd,IDC_UPDATEWHENRENDERED,param->GetUpdateWhenRendered());
            SET_CHECK_BOOL(hWnd,IDC_TEXTURESINPPI,param->GetTexturesInPPI());
            SET_CHECK_BOOL(hWnd,IDC_VECTORMAPINPPI,param->GetVectorMapInPPI());
            SET_CHECK_BOOL(hWnd,IDC_VECTORONLYINFOLLOWED,param->GetVectorMapOnlyInFollowedZone());
            SET_CHECK_BOOL(hWnd,IDC_AUTOHOOKTARGET,param->GetAutoHookTarget());
            SET_CHECK_BOOL(hWnd,IDC_RENDERPGRAYMOBS,param->GetRenderGrayMobs());
            
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    param->SetPPIText(SendDlgItemMessage(hWnd,IDC_TEXTPPI,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetGuildInPPI(SendDlgItemMessage(hWnd,IDC_GUILDPPI,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetSurnameInPPI(SendDlgItemMessage(hWnd,IDC_PPISURNAME,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetLevelInPPI(SendDlgItemMessage(hWnd,IDC_PPILEVEL,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetHealthInPPI(SendDlgItemMessage(hWnd,IDC_PPIHEALTH,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetAutoFollowFirstActor(SendDlgItemMessage(hWnd,IDC_AUTOFOLLOWFIRST,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetRenderMOBs(SendDlgItemMessage(hWnd,IDC_RENDERMOBS,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetRenderPlayers(SendDlgItemMessage(hWnd,IDC_RENDERPLAYERS,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetRenderObjects(SendDlgItemMessage(hWnd,IDC_RENDEROBJECTS,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);
                    param->SetMatchFollowedHeading(SendDlgItemMessage(hWnd,IDC_FOLLOWHEADING,BM_GETCHECK,0,0)==BST_CHECKED ? true:false);

                    // use macro, this was developed later to make the above code more readable
                    param->ModifyDrawDeadActors() = GET_CHECK_BOOL(hWnd,IDC_PPIDEAD);
                    param->ModifyUpdateWhenRendered() = GET_CHECK_BOOL(hWnd,IDC_UPDATEWHENRENDERED);
                    param->ModifyTexturesInPPI() = GET_CHECK_BOOL(hWnd,IDC_TEXTURESINPPI);
                    param->ModifyVectorMapInPPI() = GET_CHECK_BOOL(hWnd,IDC_VECTORMAPINPPI);
                    param->ModifyVectorMapOnlyInFollowedZone() = GET_CHECK_BOOL(hWnd,IDC_VECTORONLYINFOLLOWED);

                    param->ModifyAutoHookTarget() = GET_CHECK_BOOL(hWnd,IDC_AUTOHOOKTARGET);
                    param->ModifyRenderGrayMobs() = GET_CHECK_BOOL(hWnd,IDC_RENDERPGRAYMOBS);

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
} // end ConfigDisplayDlgProc

BOOL WINAPI ConfigRangeRingsDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    CheyenneConfig* param=(CheyenneConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(CheyenneConfig*)lParam;
            char txt[16];

            SendDlgItemMessage(hWnd,IDC_ENABLED1,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[0].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[0].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS1,WM_SETTEXT,0,(LPARAM)txt);

            SendDlgItemMessage(hWnd,IDC_ENABLED2,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[1].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[1].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS2,WM_SETTEXT,0,(LPARAM)txt);

            SendDlgItemMessage(hWnd,IDC_ENABLED3,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[2].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[2].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS3,WM_SETTEXT,0,(LPARAM)txt);

            SendDlgItemMessage(hWnd,IDC_ENABLED4,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[3].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[3].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS4,WM_SETTEXT,0,(LPARAM)txt);

            SendDlgItemMessage(hWnd,IDC_ENABLED5,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[4].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[4].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS5,WM_SETTEXT,0,(LPARAM)txt);

            SendDlgItemMessage(hWnd,IDC_ENABLED6,BM_SETCHECK,(WPARAM)param->GetRangeRings().Rings[5].bEnabled?BST_CHECKED:BST_UNCHECKED,0);
            sprintf(txt,"%u",param->GetRangeRings().Rings[5].Radius);
            SendDlgItemMessage(hWnd,IDC_RADIUS6,WM_SETTEXT,0,(LPARAM)txt);

            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    char txt[16];

                    SendDlgItemMessage(hWnd,IDC_RADIUS1,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[0].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED1,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[0].Radius=atoi(txt);

                    SendDlgItemMessage(hWnd,IDC_RADIUS2,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[1].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED2,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[1].Radius=atoi(txt);

                    SendDlgItemMessage(hWnd,IDC_RADIUS3,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[2].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED3,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[2].Radius=atoi(txt);

                    SendDlgItemMessage(hWnd,IDC_RADIUS4,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[3].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED4,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[3].Radius=atoi(txt);

                    SendDlgItemMessage(hWnd,IDC_RADIUS5,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[4].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED5,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[4].Radius=atoi(txt);

                    SendDlgItemMessage(hWnd,IDC_RADIUS6,WM_GETTEXT,(WPARAM)sizeof(txt),(LPARAM)txt);
                    param->ModifyRangeRings().Rings[5].bEnabled=SendDlgItemMessage(hWnd,IDC_ENABLED6,BM_GETCHECK,0,0)==BST_CHECKED?true:false;
                    param->ModifyRangeRings().Rings[5].Radius=atoi(txt);
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
} // end ConfigRangeRingsDlgProc

BOOL WINAPI AboutDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(uMsg)
        {
        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    EndDialog(hWnd,IDOK);
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
} // end AboutDlgProc

BOOL WINAPI SetSoundsDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    CheyenneConfig* param=(CheyenneConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(CheyenneConfig*)lParam;

            SET_CHECK_BOOL(hWnd,IDC_PLAYALBCREATESOUND,param->GetPlaySoundOnAlbCreate());
            SET_EDIT_STRING(hWnd,IDC_ALBCREATESOUND,param->GetAlbSoundFile());

            SET_CHECK_BOOL(hWnd,IDC_PLAYHIBCREATESOUND,param->GetPlaySoundOnHibCreate());
            SET_EDIT_STRING(hWnd,IDC_HIBCREATESOUND,param->GetHibSoundFile());

            SET_CHECK_BOOL(hWnd,IDC_PLAYMIDCREATESOUND,param->GetPlaySoundOnMidCreate());
            SET_EDIT_STRING(hWnd,IDC_MIDCREATESOUND,param->GetMidSoundFile());
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    param->ModifyPlaySoundOnAlbCreate() = GET_CHECK_BOOL(hWnd,IDC_PLAYALBCREATESOUND);
                    GET_EDIT_STRING(hWnd,IDC_ALBCREATESOUND,param->ModifyAlbSoundFile());

                    param->ModifyPlaySoundOnHibCreate() = GET_CHECK_BOOL(hWnd,IDC_PLAYHIBCREATESOUND);
                    GET_EDIT_STRING(hWnd,IDC_HIBCREATESOUND,param->ModifyHibSoundFile());

                    param->ModifyPlaySoundOnMidCreate() = GET_CHECK_BOOL(hWnd,IDC_PLAYMIDCREATESOUND);
                    GET_EDIT_STRING(hWnd,IDC_MIDCREATESOUND,param->ModifyMidSoundFile());

                    EndDialog(hWnd,IDOK);
                    break;

                case IDCANCEL:
                    EndDialog(hWnd,IDCANCEL);
                    break;

                case MAKEWPARAM(IDC_BROWSE_ALB_SOUND,BN_CLICKED):
                    {
                    std::string file_name;
                    GET_EDIT_STRING(hWnd,IDC_ALBCREATESOUND,file_name);
                    if(GetOpenFileName(hWnd,file_name,"Wave Files (*.wav)\0*.wav\0\0"))
                        {
                        SET_EDIT_STRING(hWnd,IDC_ALBCREATESOUND,file_name);
                        }
                    }
                    break;
                    
                case MAKEWPARAM(IDC_BROWSE_HIB_SOUND,BN_CLICKED):
                    {
                    std::string file_name;
                    GET_EDIT_STRING(hWnd,IDC_HIBCREATESOUND,file_name);
                    if(GetOpenFileName(hWnd,file_name,"Wave Files (*.wav)\0*.wav\0\0"))
                        {
                        SET_EDIT_STRING(hWnd,IDC_HIBCREATESOUND,file_name);
                        }
                    }
                    break;

                case MAKEWPARAM(IDC_BROWSE_MID_SOUND,BN_CLICKED):
                    {
                    std::string file_name;
                    GET_EDIT_STRING(hWnd,IDC_MIDCREATESOUND,file_name);
                    if(GetOpenFileName(hWnd,file_name,"Wave Files (*.wav)\0*.wav\0\0"))
                        {
                        SET_EDIT_STRING(hWnd,IDC_MIDCREATESOUND,file_name);
                        }
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
} // end SetSoundsDlgProc

BOOL WINAPI ConfigShareNetDlgProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    CheyenneConfig* param=(CheyenneConfig*)GetWindowLong(hWnd,GWL_USERDATA);

    switch(uMsg)
        {
        case WM_INITDIALOG:
            {
            SetWindowLong(hWnd,GWL_USERDATA,(LONG)lParam);
            
            param=(CheyenneConfig*)lParam;

            SET_EDIT_STRING(hWnd,IDC_SHARENET_ADDRESS,param->GetShareNetAddress());
            SET_EDIT_STRING(hWnd,IDC_SHARENET_PORT,param->GetShareNetPort());
            }
            break;

        case WM_COMMAND:
            {
            switch(LOWORD(wParam))
                {
                case IDOK:
                    {
                    GET_EDIT_STRING(hWnd,IDC_SHARENET_ADDRESS,param->ModifyShareNetAddress());
                    GET_EDIT_STRING(hWnd,IDC_SHARENET_PORT,param->ModifyShareNetPort());
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
} // end ConfigShareNetDlgProc

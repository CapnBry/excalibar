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
#include "CSLKeyboard.h"
#include "..\Utils\CodeUtils.h" // for word_builder union
#include "..\Utils\logger.h" // for logger
#include <list>

extern logger_t Logger;

namespace csl
{
void BuildInput
    (
    INPUT* input, // caller is expected to allocate enough of these!
    const std::list<unsigned char>& vk_list,
    const bool Press,
    const bool Release,
    HKL hKL
    )
{
    std::list<unsigned char>::const_iterator it=vk_list.begin();
    int ndx=0;
    
    if(Press)
        {
        // loop and press keys in order
        while(it != vk_list.end())
            {
            // make event
            input[ndx].ki.dwExtraInfo=0;
            input[ndx].ki.dwFlags=0;
            input[ndx].ki.time=0;
            //input[ndx].ki.wScan=0;
            input[ndx].ki.wScan=MapVirtualKeyEx(*it,0,hKL);
            input[ndx].ki.wVk=(WORD)(*it);
            input[ndx].type=INPUT_KEYBOARD;
            // go to next
            ++ndx;
            ++it;
            }
        } // end if press

    // do not reset ndx here!
    
    if(Release)
        {
        // loop and un-press keys in reverse order
        
        std::list<unsigned char>::size_type count=vk_list.size();
        while(count>0)
            {
            // make event
            input[ndx].ki.dwExtraInfo=0;
            input[ndx].ki.dwFlags=KEYEVENTF_KEYUP;
            input[ndx].ki.time=0;
            //input[ndx].ki.wScan=0;
            input[ndx].ki.wScan=MapVirtualKeyEx(input[count-1].ki.wVk,0,hKL);
            input[ndx].ki.wVk=input[count-1].ki.wVk;
            input[ndx].type=INPUT_KEYBOARD;
            
            // go to next
            ++ndx;
            --count;
            }
        } // end if release
} // end BuildInput

InputWrapper* WrapInput(const unsigned char ch,const bool AlreadyVK,const bool Press,const bool Release)
{
    // get current keyboard layout -- cheat a little
    // because we assume that this will not change
    // during program execution and that
    // the target program is using the same keyboard layout
    // that we are using
    HKL hKl=GetKeyboardLayout(GetCurrentThreadId());
    
    // get virtual key code and shift/alt/ctrl modifiers
    word_builder wb;
    
    if(!AlreadyVK)
        {
        // get VK_*
        wb.word[0]=VkKeyScanEx(ch,hKl);
        }
    else
        {
        // already a VK_*, use it
        wb.word[0]=ch;
        }
    
    std::list<unsigned char> vk_list;
    
    if(wb.byte[1] != 0)
        {
        // some shift state flag is set
        if(wb.byte[1] & 0x01)
            {
            vk_list.insert(vk_list.end(),VK_SHIFT);
            }
        
        if(wb.byte[1] & 0x02)
            {
            vk_list.insert(vk_list.end(),VK_CONTROL);
            }

        if(wb.byte[1] & 0x04)
            {
            vk_list.insert(vk_list.end(),VK_MENU);
            }
        } // end if shift state set
    
    // add VK to list
    vk_list.insert(vk_list.end(),wb.byte[0]);
    
    // allocate
    unsigned int sz=(unsigned int)vk_list.size();
    if(Press && Release)
        {
        // this will take 2 strokes
        sz*=2;
        }
    InputWrapper* wrapper=new InputWrapper(sz);
    
    // build it
    BuildInput(&wrapper->input[0],vk_list,Press,Release,hKl);

    // done
    return(wrapper);
} // end WrapInput

CSLKeyboard::CSLKeyboard()
{
    // init the key maps
    InitKeyMaps();
} // end CSLKeyboard

CSLKeyboard::~CSLKeyboard()
{
    while(PRStringMap.begin()!=PRStringMap.end())
        {
        delete PRStringMap.begin()->second;
        PRStringMap.erase(PRStringMap.begin());
        }

    while(PStringMap.begin()!=PStringMap.end())
        {
        delete PStringMap.begin()->second;
        PStringMap.erase(PStringMap.begin());
        }

    while(RStringMap.begin()!=RStringMap.end())
        {
        delete RStringMap.begin()->second;
        RStringMap.erase(RStringMap.begin());
        }

    while(PRKeyMap.begin()!=PRKeyMap.end())
        {
        delete PRKeyMap.begin()->second;
        PRKeyMap.erase(PRKeyMap.begin());
        }

    while(PKeyMap.begin()!=PKeyMap.end())
        {
        delete PKeyMap.begin()->second;
        PKeyMap.erase(PKeyMap.begin());
        }

    while(RKeyMap.begin()!=RKeyMap.end())
        {
        delete RKeyMap.begin()->second;
        RKeyMap.erase(RKeyMap.begin());
        }
} // end ~CSLKeyboard

void CSLKeyboard::MAP_VKEY(const std::string& str,const unsigned char val)
{
    PRStringMap.insert(stringmap_value(str,WrapInput(val,true,true,true)));
    PStringMap.insert(stringmap_value(str,WrapInput(val,true,true,false)));
    RStringMap.insert(stringmap_value(str,WrapInput(val,true,false,true)));
} // end MAP_VKEY

void CSLKeyboard::InitKeyMaps(void)
{
    // insert keys range [1,254]
    for(unsigned char ch=1;ch<=254;++ch)
        {
        // press and release key map
        PRKeyMap.insert(keymap_value(ch,WrapInput(ch,false,true,true)));
        // press key map
        PKeyMap.insert(keymap_value(ch,WrapInput(ch,false,true,false)));
        // release key map
        RKeyMap.insert(keymap_value(ch,WrapInput(ch,false,false,true)));
        }
    
    // insert special strings:
    MAP_VKEY("VK_UP",VK_UP);
    MAP_VKEY("VK_DOWN",VK_DOWN);
    MAP_VKEY("VK_LEFT",VK_LEFT);
    MAP_VKEY("VK_RIGHT",VK_RIGHT);
    MAP_VKEY("VK_RETURN",VK_RETURN);
    MAP_VKEY("VK_ESCAPE",VK_ESCAPE);
    MAP_VKEY("VK_SPACE",VK_SPACE);
    MAP_VKEY("VK_HOME",VK_HOME);
    MAP_VKEY("VK_END",VK_END);
    MAP_VKEY("VK_PAUSE",VK_PAUSE);
    MAP_VKEY("VK_PRIOR",VK_PRIOR);
    MAP_VKEY("VK_NEXT",VK_NEXT);
    MAP_VKEY("VK_INSERT",VK_INSERT);
    MAP_VKEY("VK_DELETE",VK_DELETE);
    MAP_VKEY("VK_BACK",VK_BACK);
    MAP_VKEY("VK_TAB",VK_TAB);
    MAP_VKEY("VK_SHIFT",VK_SHIFT);
    MAP_VKEY("VK_SNAPSHOT",VK_SNAPSHOT);
    MAP_VKEY("VK_NUMPAD0",VK_NUMPAD0);
    MAP_VKEY("VK_NUMPAD1",VK_NUMPAD1);
    MAP_VKEY("VK_NUMPAD2",VK_NUMPAD2);
    MAP_VKEY("VK_NUMPAD3",VK_NUMPAD3);
    MAP_VKEY("VK_NUMPAD4",VK_NUMPAD4);
    MAP_VKEY("VK_NUMPAD5",VK_NUMPAD5);
    MAP_VKEY("VK_NUMPAD6",VK_NUMPAD6);
    MAP_VKEY("VK_NUMPAD7",VK_NUMPAD7);
    MAP_VKEY("VK_NUMPAD8",VK_NUMPAD8);
    MAP_VKEY("VK_NUMPAD9",VK_NUMPAD9);
    MAP_VKEY("VK_MULTIPLY",VK_MULTIPLY);
    MAP_VKEY("VK_ADD",VK_ADD);
    MAP_VKEY("VK_SEPARATOR",VK_SEPARATOR);
    MAP_VKEY("VK_SUBTRACT",VK_SUBTRACT);
    MAP_VKEY("VK_DECIMAL",VK_DECIMAL);
    MAP_VKEY("VK_DIVIDE",VK_DIVIDE);
    MAP_VKEY("VK_F1",VK_F1);
    MAP_VKEY("VK_F2",VK_F2);
    MAP_VKEY("VK_F3",VK_F3);
    MAP_VKEY("VK_F4",VK_F4);
    MAP_VKEY("VK_F5",VK_F5);
    MAP_VKEY("VK_F6",VK_F6);
    MAP_VKEY("VK_F7",VK_F7);
    MAP_VKEY("VK_F8",VK_F8);
    MAP_VKEY("VK_F9",VK_F9);
    MAP_VKEY("VK_F10",VK_F10);
    MAP_VKEY("VK_F11",VK_F11);
    MAP_VKEY("VK_F12",VK_F12);
    MAP_VKEY("VK_F13",VK_F13);
    MAP_VKEY("VK_F14",VK_F14);
    MAP_VKEY("VK_F15",VK_F15);
    MAP_VKEY("VK_F16",VK_F16);
    MAP_VKEY("VK_F17",VK_F17);
    MAP_VKEY("VK_F18",VK_F18);
    MAP_VKEY("VK_F19",VK_F19);
    MAP_VKEY("VK_F20",VK_F20);
    MAP_VKEY("VK_F21",VK_F21);
    MAP_VKEY("VK_F22",VK_F22);
    MAP_VKEY("VK_F23",VK_F23);
    MAP_VKEY("VK_F24",VK_F24);
    
    const_stringmap_iterator it=PRStringMap.begin();
    while(it!=PRStringMap.end())
        {
        Logger << "[CSLKeyboard::InitKeyMaps] PRStringMap: " << it->first << std::endl;
        ++it;
        }
    
} // end InitKeyMaps

bool CSLKeyboard::Press(const unsigned char key)
{
    const_keymap_iterator key_it=PKeyMap.find(key);
    
    if(key_it == PKeyMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        // found it, return send status
        return(key_it->second->Send());
        }    
} // end Press

bool CSLKeyboard::Release(const unsigned char key)
{
    const_keymap_iterator key_it=RKeyMap.find(key);
    
    if(key_it == RKeyMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        // found it, return send status
        return(key_it->second->Send());
        }    
} // end Release

bool CSLKeyboard::PressAndRelease(const unsigned char key)
{
    //SendKeyToWindow(key);
    //return(true);
    
    const_keymap_iterator key_it=PRKeyMap.find(key);
    
    if(key_it == PRKeyMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        // found it, return send status
        return(key_it->second->Send());
        }    
} // end PressAndRelease

bool CSLKeyboard::String(const std::string& key_string)
{
    //SendKeysToWindow(key_string);
    //return(true);
    
    std::string::const_iterator str_it=key_string.begin();
    
    // for every character
    while(str_it != key_string.end())
        {
        /*
        WORD vk=VkKeyScan(*str_it);
        keybd_event((unsigned char)vk,0,((vk&0xE0)==0xE0)?KEYEVENTF_EXTENDEDKEY:0,0);
        Sleep(100);
        keybd_event((unsigned char)vk,0,((vk&0xE0)==0xE0)?KEYEVENTF_EXTENDEDKEY|KEYEVENTF_KEYUP:KEYEVENTF_KEYUP,0);
        ++str_it;
        continue;
        */
        // press and release it
        if(!PressAndRelease(*str_it))
            {
            return(false);
            }
        
        // go to next
        ++str_it;
        } // end while -- for every character in the string
    
    // done
    return(true);
} // end String

bool CSLKeyboard::PressVK(const std::string vk_string)
{
    const_stringmap_iterator str_it=PStringMap.find(vk_string);
    
    if(str_it==PStringMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        return(str_it->second->Send());
        }
} // end PressVK

bool CSLKeyboard::ReleaseVK(const std::string vk_string)
{
    const_stringmap_iterator str_it=RStringMap.find(vk_string);
    
    if(str_it==RStringMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        return(str_it->second->Send());
        }
} // end ReleaseVK

bool CSLKeyboard::PressAndReleaseVK(const std::string vk_string)
{
    const_stringmap_iterator str_it=PRStringMap.find(vk_string);
    
    if(str_it==PRStringMap.end())
        {
        // didn't find it
        return(false);
        }
    else
        {
        return(str_it->second->Send());
        }
} // end PressAndReleaseVK

} // end namespace csl
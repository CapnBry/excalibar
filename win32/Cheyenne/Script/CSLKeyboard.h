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
#pragma once

// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include <string>
#include <map>

namespace csl
{
class InputWrapper
{
friend InputWrapper* WrapInput(const unsigned char ch,const bool AlreadyVK,const bool Press,const bool Release);
public:
    InputWrapper(UINT _size=1)
    {
        size=_size;
        input=new INPUT[size];
    }
    InputWrapper(const InputWrapper& s)
    {
        set(s);
    };
    
    ~InputWrapper()
    {
        delete[] input;
    }
    
    InputWrapper& operator=(const InputWrapper& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }
    bool Send(void)const
    {
        return(::SendInput(size,input,sizeof(INPUT))==size ? true:false);
    }
protected:
private:
    void set(const InputWrapper& s)
    {
        delete input;
        size=s.size;
        input=new INPUT[size];
        memcpy(input,&s.input,size*sizeof(INPUT));
    }
    
    INPUT* input;
    UINT size;
}; // end class InputWrapper

class CSLKeyboard
{
public:
    typedef std::map<unsigned char,InputWrapper*> keymap_t;
    typedef keymap_t::const_iterator const_keymap_iterator;
    typedef keymap_t::iterator keymap_iterator;
    typedef keymap_t::value_type keymap_value;
    
    typedef std::map<std::string,InputWrapper*> stringmap_t;
    typedef stringmap_t::const_iterator const_stringmap_iterator;
    typedef stringmap_t::iterator stringmap_iterator;
    typedef stringmap_t::value_type stringmap_value;

    CSLKeyboard();
    virtual ~CSLKeyboard();
    
    bool Press(const unsigned char key);
    bool Release(const unsigned char key);
    bool PressAndRelease(const unsigned char key);
    bool String(const std::string& key_string);
    bool PressVK(const std::string vk_string);
    bool ReleaseVK(const std::string vk_string);
    bool PressAndReleaseVK(const std::string vk_string);
protected:
private:
    void InitKeyMaps(void);
    
    void MAP_VKEY(const std::string& str,const unsigned char val);

    keymap_t PRKeyMap;
    keymap_t PKeyMap;
    keymap_t RKeyMap;

    stringmap_t PRStringMap;
    stringmap_t PStringMap;
    stringmap_t RStringMap;
    
}; // end class CSLKeyboard
} // end namespace csl
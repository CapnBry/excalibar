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

#include "MobsightTypes.h"
#include <iostream>
#include <sstream>

class MobsightIdHandlerImpl : public ISAXContentHandlerImpl
{
public:
    MobsightIdHandlerImpl();
    virtual ~MobsightIdHandlerImpl();

	// standard interface implementations inherited from
	// IUnknown that we have to implement
	HRESULT __stdcall QueryInterface(REFIID riid, void **ppv)
	{
		if (ppv == NULL)
		    {
			return E_POINTER;
		    }

		*ppv = NULL;

		if (InlineIsEqualGUID(riid, IID_IUnknown) ||
			InlineIsEqualGUID(riid, IID_ISAXContentHandler))
		    {
			*ppv = static_cast<ISAXContentHandler *>(this);
			return S_OK;
		    }

		return E_NOINTERFACE;
	}

	ULONG __stdcall AddRef()
	{
		return 1;
	}

	ULONG __stdcall Release()
	{
		return 1;
	}


    // overrides
    HRESULT __stdcall startDocument();
    HRESULT __stdcall endDocument();
    HRESULT __stdcall startElement
        (
        const wchar_t * wszNamespaceUri,
        int cchNamespaceUri,
        const wchar_t * wszLocalName,
        int cchLocalName,
        const wchar_t* wszQName,
        int cchQName,
        ISAXAttributes * pAttributes 
        );
    
    HRESULT __stdcall endElement
        (
        const wchar_t * wszNamespaceUri,
        int cchNamespaceUri,
        const wchar_t * wszLocalName,
        int cchLocalName,
        const wchar_t* wszQName,
        int cchQName
        );

    HRESULT __stdcall characters
        (
        const wchar_t * pwchChars,
        int cchChars
        );

    
    bool GetMobInfo(NarrowIdDef& MobInfo)const;
    void FreeMobInfo(NarrowIdDef& MobInfo)const;

protected:
private:
    std::wstringstream cur_id;
    std::wstringstream cur_name;
    std::wstringstream cur_zone;
    std::wstringstream cur_x;
    std::wstringstream cur_y;
    std::wstringstream cur_z;
    bool bIdStore;
    bool bNameStore;
    bool bZoneStore;
    bool bXStore;
    bool bYStore;
    bool bZStore;
    
    IdDef CurrentMob;
}; // end class MobsightIdHandlerImpl

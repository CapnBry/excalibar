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
#include "..\Utils\tsdeque.h" // for tsfifo class

class CheyenneMessage; // predefine this
class Database; // predefine this
class Sniffer; // predefine this
template<class STORAGE_T> class PktLoadSniffer; // predefine this
template<class T> class PacketStore; // predefine this
template<class API_IMPL>class MiniShareServer; // predefine this
class ShareNetClientData; // predefine this
class TelnetClientData; //predefine this
namespace csl {class CSLScriptHost;} // predefine this

typedef Sniffer SNIFFER_T; // use this for 'normal' sniffing operation
//typedef PktLoadSniffer<PacketStore<std::ifstream> > SNIFFER_T; // use this for loading saved packet files


class Central
{
friend class SharenetMessageFunctor;
public:
    Central();
    virtual ~Central();

    WPARAM Go(HINSTANCE hInst);

protected:
private:
    static LRESULT CALLBACK WindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
        void HandleMainWindowSizing(RECT* r);
        void HandleMainWindowSize(void);
        void HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
        void Draw(HDC hFront)const;

    void OnSharenetMessage(const void* p,const unsigned int len);

    bool Init(HINSTANCE hInst);
    Central(const Central& s); // disallow
    Central& operator=(const Central& s); // disallow

    // windows variables
    HWND hMainWnd;
    HINSTANCE hInstance;
    HFONT hTahoma;
    
    // common message fifo
    tsfifo<CheyenneMessage*>* MessageFifo;
    tsfifo<std::string*>* TelnetFifoSend;
    tsfifo<std::string*>* TelnetFifoReceive;
    // database
    Database* database;
    // sniffer
    SNIFFER_T* sniffer;
    // mini share server
    MiniShareServer<ShareNetClientData>* sharenet;
    // script host
    csl::CSLScriptHost* scripthost;
    // telnet mini server
    MiniShareServer<TelnetClientData>* scriptserver;

}; // end class Central

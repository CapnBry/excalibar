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

// macro for externs
#ifndef MAIN_FUNCTION
    #define EXTERN extern
#else
    #define EXTERN 
#endif

// some program-wide globals
#define RENDER_NOW (WM_USER+1)
EXTERN std::string InitialDir; // the initial directory
EXTERN logger_t Logger; // the logger
EXTERN MapInfo Zones; // zone info
EXTERN CheyenneClock Clock; // for the clock
EXTERN Config RadarConfig; // the radar config

// prototypes
LRESULT CALLBACK MainWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
LRESULT CALLBACK PPIWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
LRESULT CALLBACK DataWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
INT_PTR CALLBACK ConfigDialogProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam);
INT_PTR CALLBACK ConnectServerDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
INT_PTR CALLBACK ConfigRangeRingsDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
INT_PTR CALLBACK AboutDialogProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
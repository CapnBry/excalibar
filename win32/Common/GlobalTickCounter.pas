unit GlobalTickCounter;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows;
{$ENDIF !LINUX}
  
{$IFDEF GLOBAL_TICK_COUNTER}
var
  GlobalTickCount:  Cardinal;
procedure UpdateGlobalTickCount;
{$ELSE}
function GlobalTickCount : Cardinal;
procedure UpdateGlobalTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}

implementation

{$IFDEF LINUX}
procedure UpdateGlobalTickCount;
var
  tod:  timeval;
  tz:   timezone;
begin
{$IFDEF GLOBAL_TICK_COUNTER}
  gettimeofday(tod, tz);
  GlobalTickCount := (tod.tv_sec * 1000) + (tod.tv_usec div 1000);
{$ENDIF GLOBAL_TICK_COUNTER}
end;
{$ELSE}
procedure UpdateGlobalTickCount;
begin
  GlobalTickCount := GetTickCount;
end;
{$ENDIF !LINUX}

{$IFNDEF GLOBAL_TICK_COUNTER}
function GlobalTickCount : Cardinal;
begin
  Result := GetTickCount;
end;
{$ENDIF}

end.
 

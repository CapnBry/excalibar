unit GlobalTickCounter;

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
 

unit GlobalTickCounter;

interface

uses
  Windows;
  
{$IFDEF GLOBAL_TICK_COUNTER}
var
  GlobalTickCount:  Cardinal;
procedure UpdateGlobalTickCount;
{$ELSE}
function GlobalTickCount : Cardinal;
procedure UpdateGlobalTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}

implementation

procedure UpdateGlobalTickCount;
begin
{$IFDEF GLOBAL_TICK_COUNTER}
  GlobalTickCount := GetTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}
end;

{$IFNDEF GLOBAL_TICK_COUNTER}
function GlobalTickCount : Cardinal;
begin
  Result := GetTickCount;
end;
{$ENDIF}

end.
 
unit GlobalTickCounter;

interface

uses
  Windows;
  
var
  GlobalTickCount:  Cardinal;
  
procedure UpdateGlobalTickCount;

implementation

procedure UpdateGlobalTickCount;
begin
  GlobalTickCount := GetTickCount;
end;

end.
 
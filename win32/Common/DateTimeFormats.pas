unit DateTimeFormats;

interface

uses
  Windows, SysUtils;

function DateTimeToHL7TS(AVal: TDateTime) : string;

implementation

function BiasToStr(ABias: integer) : string;
begin
    { flip this because bias = UTC - localtime }
  ABias := -ABias;
  Result := Format('%2.2d%2.2d', [ABias div 60, ABias mod 60]);
  if ABias >= 0 then
    Result := '+' + Result;
end;

function CurrentTZOffset : string;
var
  tzi:    TIME_ZONE_INFORMATION;
  dwDST:  Cardinal;
begin
  FillChar(tzi, sizeof(TIME_ZONE_INFORMATION), 0);

  dwDST := GetTimeZoneInformation(tzi);
  case dwDST of
    TIME_ZONE_ID_UNKNOWN:
        Result := '';
    TIME_ZONE_ID_STANDARD:
        Result := BiasToStr(tzi.Bias + tzi.StandardBias);
    TIME_ZONE_ID_DAYLIGHT:
        Result := BiasToStr(tzi.Bias + tzi.DaylightBias);
    else
        Result := '';
  end;
end;

function DateTimeToHL7TS(AVal: TDateTime) : string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', AVal) + CurrentTZOffset;
end;


end.

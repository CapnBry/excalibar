unit StringParseHlprs;

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
  SysUtils;

type
  TParseCharSet = set of char;

const
  pcsFILENAME_CHARS = ['.', '0'..'9', ':', 'A'..'Z', '\', '_', 'a'..'z'];

function ParseWord(const sLine: string; var iStartPos: integer) : string;
function ParseWordEx(const sLine: string; var iStartPos: integer;
  arAllowedChars: TParseCharSet) : string;
function ParseInt(const sLine: string; var iPos: integer) : integer;
function ParseIntWCommas(const sLine: string; var iPos: integer) : integer;
function StringBeginsWith(const sLine, sTarget: string) : boolean;
function StringEndsWith(const sLine, sTarget: string) : boolean;
function StringContains(const sLine, sTarget: string) : boolean;
function RemoveThe(const sLine: string) : string;
function WildMatch(const AWild, AString: string) : boolean;

implementation

function ParseWord(const sLine: string; var iStartPos: integer) : string;
begin
  Result := '';
  while (iStartPos <= Length(sLine)) and
    not (sLine[iStartPos] in ['0'..'9', 'A'..'Z', 'a'..'z']) do
    inc(iStartPos);

  while (iStartPos <= Length(sLine)) and
    (sLine[iStartPos] in ['0'..'9', 'A'..'Z', 'a'..'z']) do begin
    Result := Result + sLine[iStartPos];
    inc(iStartPos);
  end;
end;

function ParseWordEx(const sLine: string; var iStartPos: integer;
  arAllowedChars: TParseCharSet) : string;
begin
  Result := '';
  while (iStartPos <= Length(sLine)) and
    not (sLine[iStartPos] in arAllowedChars) do
    inc(iStartPos);

  while (iStartPos <= Length(sLine)) and
    (sLine[iStartPos] in arAllowedChars) do begin
    Result := Result + sLine[iStartPos];
    inc(iStartPos);
  end;
end;

function ParseInt(const sLine: string; var iPos: integer) : integer;
begin
  Result := 0;
  while (iPos <= Length(sLine)) and
    not (sLine[iPos] in ['0'..'9']) do
    inc(iPos);

  while (iPos <= Length(sLine)) and
    (sLine[iPos] in ['0'..'9']) do begin
    Result := (Result * 10) + (ord(sLine[iPos]) - ord('0'));
    inc(iPos);
  end;    { while }
end;

function ParseIntWCommas(const sLine: string; var iPos: integer) : integer;
begin
  Result := 0;
  while (iPos <= Length(sLine)) and
    not (sLine[iPos] in ['0'..'9']) do
    inc(iPos);

  while (iPos <= Length(sLine)) and
    (sLine[iPos] in ['0'..'9', ',']) do begin
    if sLine[iPos] <> ',' then
      Result := (Result * 10) + (ord(sLine[iPos]) - ord('0'));
    inc(iPos);
  end;    { while }
end;

function StringBeginsWith(const sLine, sTarget: string) : boolean;
begin
  Result := StrLComp(PChar(Pointer(sLine)), PChar(Pointer(sTarget)), Length(sTarget)) = 0;
end;

function StringEndsWith(const sLine, sTarget: string) : boolean;
begin
  Result := (Length(sLine) >= Length(sTarget)) and
    (StrLComp(PChar(Pointer(sLine)) + Length(sLine) - Length(sTarget),
    PChar(Pointer(sTarget)), Length(sTarget)) = 0);
end;

function StringContains(const sLine, sTarget: string) : boolean;
begin
  Result := Pos(sTarget, sLine) <> 0;
end;

function RemoveThe(const sLine: string) : string;
begin
  if StrLIComp(PChar(Pointer(sLine)), 'the ', 4) = 0 then
    Result := copy(sLine, 4, Length(sLine))
  else
    Result := sLine;
end;

function WildMatch(const AWild, AString: string) : boolean;
{ Simple case-sensitive globbing wildcard search }
var
  s, w, cp, mp: PChar;
begin
  s := PChar(AString);
  w := PChar(AWild);
  mp := nil;
  cp := nil;

	while (s^ <> #0) and (w^ <> '*') do begin
    if (w^ <> s^) and (w^ <> '?') then begin
      Result := false;
      exit;
    end;

    inc(w);
    inc(s);
	end;

  while s^ <> #0 do begin
    if w^ = '*' then begin
      inc(w);
      if w^ = #0 then begin
        Result := true;
        exit;
      end;

      mp := w;
      cp := s + 1;
    end

    else if (w^ = s^) or (w^ = '?') then begin
      inc(w);
      inc(s);
    end

    else begin
      w := mp;
      s := cp;
      inc(cp);
    end;
	end;  { while s^ }

  while w^ = '*' do
    inc(w);

  Result := w^ = #0;
end;

end.

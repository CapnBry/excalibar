unit BoyerMoore;

//------------------------------------------------------------------------------
// Components        TTextSearch, TMemorySearch & TFileSearch                  .
// Version:          2.0                                                       .
// Date:             28 December 2001                                          .
// Compilers:        Delphi 3 - Delphi 6                                       .
// Author:           Angus Johnson - ajohnson@rpi.net.au                       .
// Copyright:        © 2001 Angus Johnson                                      .
//                                                                             .
// Description:      Delphi implementation of the                              .
//                   Boyer-Moore-Horspool search algorithm.                    .
//------------------------------------------------------------------------------

interface

uses
  windows, sysutils, classes;

type

  TBaseBoyMooSearch = class(TObject)
  private
     fStart          : pchar;
     fDataLength     : integer;
     fPos            : pchar;
     fEnd            : pchar;
     fPattern        : string;
     fPatLen         : integer;
     fPatInitialized : boolean;
     fCaseSensitive  : boolean;
     JumpShift       : integer;
     Shift           : array[#0..#255] of integer;
     CaseBlindTable  : array[#0..#255] of char;
     procedure InitPattern;
     procedure MakeCaseBlindTable;
     procedure SetCaseSensitive(CaseSensitive: boolean);
     function  FindCaseSensitive: integer;
     function  FindCaseInsensitive: integer;
  protected
     procedure ClearData;
     procedure SetPattern(Pattern: string);
  public
     constructor Create;
     destructor  Destroy; override;
     //The following Find functions return the 0 based offset of Pattern
     //else POSITION_EOF (-1) if the Pattern is not found  ...
     function  FindFirst: integer;
     function  FindNext: integer;
     function  FindFrom(StartPos: integer): integer;
     procedure SetData(Data: pchar; DataLength: integer);

     property  Pattern: string read fPattern write SetPattern;
     property  Data: pchar read fStart;
     property  DataLength: integer read fDataLength;
  published
     property CaseSensitive: boolean
       read fCaseSensitive write SetCaseSensitive;
  end;

const
  BOYERMOORE_POSITION_EOF = -1;

implementation


//------------------------------------------------------------------------------
// TBaseSearch methods ...
//------------------------------------------------------------------------------

procedure TBaseBoyMooSearch.MakeCaseBlindTable;
var
  i: char;
begin
  for i:= #0 to #255 do
     CaseBlindTable[i]:= ansilowercase(i)[1];
end;
//------------------------------------------------------------------------------

constructor TBaseBoyMooSearch.Create;
begin
  inherited Create;
  fStart := nil;
  fPattern := '';
  fPatLen := 0;
  MakeCaseBlindTable;
  fCaseSensitive := false;      //Default to case insensitive searches.
  fPatInitialized := false;
end;
//------------------------------------------------------------------------------

destructor TBaseBoyMooSearch.Destroy;
begin
  ClearData;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TBaseBoyMooSearch.ClearData;
begin
  fStart := nil;
  fPos := nil;
  fEnd := nil;
  fDataLength := 0;
end;
//------------------------------------------------------------------------------

procedure TBaseBoyMooSearch.SetPattern(Pattern: string);
begin
  if fPattern = Pattern then exit;
  fPattern := Pattern;
  fPatLen := length(Pattern);
  fPatInitialized := false;
end;
//------------------------------------------------------------------------------
procedure TBaseBoyMooSearch.SetData(Data: pchar; DataLength: integer);
begin
  ClearData;
  if (Data = nil) or (DataLength < 1) then exit;
  fStart := Data;
  fDataLength := DataLength;
  fEnd := fStart + fDataLength;
end;
//------------------------------------------------------------------------------

procedure TBaseBoyMooSearch.SetCaseSensitive(CaseSensitive: boolean);
begin
  if fCaseSensitive = CaseSensitive then exit;
  fCaseSensitive := CaseSensitive;
  fPatInitialized := false;
end;
//------------------------------------------------------------------------------

procedure TBaseBoyMooSearch.InitPattern;
var
  j: integer;
  i: char;
begin
  if fPatLen = 0 then exit;
  for i := #0 to #255 do Shift[i]:= fPatLen;
  if fCaseSensitive then
  begin
    for j := 1 to fPatLen-1 do
      Shift[fPattern[j]]:= fPatLen - j;
    JumpShift := Shift[fPattern[fPatLen]];
    Shift[fPattern[fPatLen]] := 0;
  end else
  begin
    for j := 1 to fPatLen-1 do
      Shift[CaseBlindTable[fPattern[j]]]:= fPatLen - j;
    JumpShift := Shift[CaseBlindTable[fPattern[fPatLen]]];
    Shift[CaseBlindTable[fPattern[fPatLen]]] := 0;
  end;
  fPatInitialized := true;
end;
//------------------------------------------------------------------------------

function TBaseBoyMooSearch.FindFirst: integer;
begin
  fPos := fStart+fPatLen-1;
  result := FindNext;
end;
//------------------------------------------------------------------------------

function TBaseBoyMooSearch.FindFrom(StartPos: integer): integer;
begin
  if StartPos < fPatLen-1 then
    fPos := fStart+fPatLen-1 else
    fPos := fStart+StartPos;
  result := FindNext;
end;
//------------------------------------------------------------------------------

function TBaseBoyMooSearch.FindNext: integer;
begin
  if (fPatLen = 0) then
  begin
     result := BOYERMOORE_POSITION_EOF;
     exit;
  end;
  if not fPatInitialized then InitPattern;
  if fPatLen >= fDataLength then fPos := fEnd;
  if fPos >= fEnd then
  begin
     fPos := fEnd;
     result := BOYERMOORE_POSITION_EOF;
     exit;
  end;
  if fCaseSensitive then
    result := FindCaseSensitive else
    result := FindCaseInsensitive;
end;
//------------------------------------------------------------------------------

function TBaseBoyMooSearch.FindCaseSensitive: integer;
var
  i: integer;
  j: pchar;
begin
  result:= BOYERMOORE_POSITION_EOF;
  while fPos < fEnd do
  begin
    i := Shift[fPos^];        //test last character first
    if i <> 0 then            //last char does not match
      inc(fPos,i)
    else
    begin                     //last char matches at least
      i := 1;
      j := fPos - fPatLen;
      while (i < fPatLen) and (fPattern[i] = (j+i)^) do inc(i);
      if (i = fPatLen) then
      begin
         result:= fPos-fStart-fPatLen+1;
         inc(fPos,fPatLen);
         break;               //FOUND!
      end
      else
        inc(fPos,JumpShift);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TBaseBoyMooSearch.FindCaseInsensitive: integer;
var
  i: integer;
  j: pchar;
begin
  result:= BOYERMOORE_POSITION_EOF;
  while fPos < fEnd do
  begin
    i := Shift[CaseBlindTable[fPos^]];   //test last character first
    if i <> 0 then                       //last char does not match
      inc(fPos,i)
    else
    begin                                //last char matches at least
      i := 1;
      j := fPos - fPatLen;
      while (i < fPatLen) and
            (CaseBlindTable[fPattern[i]] = CaseBlindTable[(j+i)^]) do inc(i);
      if (i = fPatLen) then
      begin
         result:= fPos-fStart-fPatLen+1;
         inc(fPos,fPatLen);
         break;                          //FOUND!
      end
      else
        inc(fPos,JumpShift);
    end;
  end;
end;

end.

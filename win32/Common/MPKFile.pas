unit MPKFile;

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
  Windows, SysUtils, Classes, zlib2, DateUtils, Contnrs, VCLMemStrms;

type
  TMPKDirectoryEntryRec = packed record
    Name:   array[0..255] of char;
    ModTime:      Cardinal; // UnixTimestamp
    Attribs:      Cardinal; // ??
    UncompOffset: integer;
    UncompSize:   integer;
    Offset:   integer;
    CompSize: integer;
    CRC32:    Cardinal;
  end;

  TMPKDirectoryEntry = class(TObject)
  private
    FCRC32: Cardinal;
    FCompressRatio: integer;
    FUncompressedSize: integer;
    FFileOffset: integer;
    FUncompressedOffset: integer;
    FFOUR: integer;
    FVAL: integer;
    FCompressedSize: integer;
    FName: string;
    FLastModified: TDateTime;
    procedure SetCompressedSize(const Value: integer);
    procedure SetUncompressedSize(const Value: integer);
    procedure RecalcCompressRatio;
  public
    constructor CreateFrom(AMPKDirRec: TMPKDirectoryEntryRec);

    property Name: string read FName write FName;
    property VAL: integer read FVAL write FVAL;
    property FOUR: integer read FFOUR write FFOUR;
    property UncompressedOffset: integer read FUncompressedOffset write FUncompressedOffset;
    property UncompressedSize: integer read FUncompressedSize write SetUncompressedSize;
    property FileOffset: integer read FFileOffset write FFileOffset;
    property CompressedSize: integer read FCompressedSize write SetCompressedSize;
    property CRC32: Cardinal read FCRC32 write FCRC32;
    property CompressRatio: integer read FCompressRatio;
    property LastModified: TDateTime read FLastModified;
  end;

  TMPKDirectoryEntryList = class(TObjectList)
  private
    function GetItems(Index: integer): TMPKDirectoryEntry;
  public
    function IndexOf(AName: string) : integer;
    function Has(AName: string) : boolean;

    property Items[Index: integer]: TMPKDirectoryEntry read GetItems; default;
  end;

  TMPKFile = class(TObject)
  private
    FS:   TFileStream;
    FActive:    boolean;
    FFileName:      string;
    FInternalName:  string;
    FDirectory:     TMPKDirectoryEntryList;
    FReadBuffer:    array[0..4095] of char;
    procedure SetFileName(const Value: string);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    procedure ExtractToFile(const AName, AFileName: string);
    procedure ExtractIdxToStream(AIndex: integer; strm: TStream);
    procedure ExtractToStream(const AName: string; strm: TStream);
    procedure ExtractToDirectory(const AName, ADirName: string);
    procedure ExtractAllToDirectory(const ADirName: string);
    function ExtractStream(const AName: string) : TMemoryStream;
    function ExtractStreamIdx(AIndex: integer) : TMemoryStream;

    property FileName: string read FFileName write SetFileName;
    property InternalName: string read FInternalName;
    property Directory: TMPKDirectoryEntryList read FDirectory;
  end;

implementation

{ TMPKFile }

procedure TMPKFile.Close;
begin
  FreeAndNil(FS);
  FDirectory.Clear;
  FInternalName := '';

  FActive := false;
end;

constructor TMPKFile.Create(const AFileName: string);
begin
  inherited Create;

  FDirectory := TMPKDirectoryEntryList.Create;
  SetFileName(AFileName);
end;

destructor TMPKFile.Destroy;
begin
  Close;
  FDirectory.Free;
  inherited Destroy;
end;

procedure TMPKFile.ExtractAllToDirectory(const ADirName: string);
var
  I:    integer;
begin
  for I := 0 to FDirectory.Count - 1 do
    ExtractToDirectory(FDirectory[I].Name, ADirName);
end;

procedure TMPKFile.ExtractIdxToStream(AIndex: integer; strm: TStream);
var
  iRead:  integer;
  zDS:    TDecompressionStream;
begin
  if AIndex = -1 then
    raise Exception.Create('Source stream not found in archive');

  FS.Seek(Integer(FDirectory[AIndex].FileOffset), soFromBeginning);
  zDS := TDecompressionStream.Create(FS);
//Write(GetTickCount);
  try
    repeat
      iRead := zDS.Read(FReadBuffer, sizeof(FReadBuffer));
      if iRead <> 0 then
        strm.Write(FReadBuffer, iRead);
    until iRead = 0;
//WriteLn(' ', GetTickCount);
  finally
    zDS.Free;
  end;
end;

function TMPKFile.ExtractStream(const AName: string): TMemoryStream;
var
  iIndex:     integer;
begin
  iIndex := FDirectory.IndexOf(AName);
  Result := ExtractStreamIdx(iIndex);
end;

function TMPKFile.ExtractStreamIdx(AIndex: integer): TMemoryStream;
var
  iStrmSize:  integer;
begin
  if AIndex <> -1 then
    iStrmSize := FDirectory[AIndex].FUncompressedSize
  else
    iStrmSize := 0;

    { for streams of less than 512KB (including 0 length ones, use
      an efficient VCL mem stream }
  if iStrmSize < (512 * 1024) then
    Result := TVCLMemoryStream.Create
  else
    Result := TMemoryStream.Create;

  Result.Size := iStrmSize;
  ExtractIdxToStream(AIndex, Result);
  Result.Seek(0, soFromBeginning);
end;

procedure TMPKFile.ExtractToDirectory(const AName, ADirName: string);
begin
  ExtractToFile(AName, IncludeTrailingPathDelimiter(ADirName) + AName);
end;

procedure TMPKFile.ExtractToFile(const AName, AFileName: string);
var
  fsOut:  TFileStream;
begin
  fsOut := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    ExtractToStream(AName, fsOut);
  finally
    fsOut.Free;
  end;
end;

procedure TMPKFile.ExtractToStream(const AName: string; strm: TStream);
var
  iIdx:   integer;
begin
  iIdx := FDirectory.IndexOf(AName);
  if iIdx = -1 then
    raise Exception.CreateFmt('Source stream [%s] not found in archive', [AName]);
    
  ExtractIdxToStream(iIdx, strm);
end;

procedure TMPKFile.Open;
var
  aSIG: array[0..3] of char;
  zDS:  TDecompressionStream;
  iRead:  integer;
  ms:   TMemoryStream;
  de:   TMPKDirectoryEntryRec;
begin
  if (FFileName = '') or FActive then
    exit;

  FS := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  FS.Read(aSIG, sizeof(aSIG));
  if StrLComp('MPAK', @aSIG, sizeof(aSIG)) <> 0 then
    raise Exception.Create('Not an MPAK file');

  FS.Seek(21, soFromBeginning);

    { first stream = name of file? }
  zDS := TDecompressionStream.Create(FS);
  iRead := zDS.Read(FReadBuffer, sizeof(FReadBuffer));
  SetString(FInternalName, FReadBuffer, iRead);

    { read the directory }
  zDS.ResetInflateState;
  ms := TVCLMemoryStream.Create;
  repeat
    iRead := zDS.Read(FReadBuffer, sizeof(FReadBuffer));
    if iRead > 0 then
      ms.Write(FReadBuffer, iRead);
  until iRead = 0;
  zDS.Free;

  ms.Position := 0;
  while ms.Position <> ms.Size do begin
    ms.Read(de, sizeof(de));
//    dt := (de.FOUR shl 32) or de.VAL;
//    dt2 := UnixToDateTime(dt);
//    dt2 := Now;
//    dt := dt div SecsPerDay;
      { fixup the offset to be relative to the start of the file }
    inc(de.Offset, FS.Position);
    FDirectory.Add(TMPKDirectoryEntry.CreateFrom(de));
  end;
  ms.Free;

  FActive := true;
end;

procedure TMPKFile.SetFileName(const Value: string);
begin
  Close;

  FFileName := Value;
  Open;
end;

{ TMPKDirectoryEntry }

constructor TMPKDirectoryEntry.CreateFrom(AMPKDirRec: TMPKDirectoryEntryRec);
begin
  inherited Create;

  FCRC32 := AMPKDirRec.CRC32;
  FUncompressedSize := AMPKDirRec.UncompSize;
  FFileOffset := AMPKDirRec.Offset;
  FUncompressedOffset := AMPKDirRec.UncompOffset;
//  FFOUR := AMPKDirRec.FOUR;
//  FVAL := AMPKDirRec.VAL;
  FLastModified := UnixToDateTime(AMPKDirRec.ModTime);
  FCompressedSize := AMPKDirRec.CompSize;
  FName := AMPKDirRec.Name;
  RecalcCompressRatio;
end;

procedure TMPKDirectoryEntry.RecalcCompressRatio;
begin
  if FUncompressedSize <> 0 then
    FCompressRatio := round((FUncompressedSize - FCompressedSize) * 100 / FUncompressedSize)
  else
    FCompressRatio := 0;
end;

procedure TMPKDirectoryEntry.SetCompressedSize(const Value: integer);
begin
  FCompressedSize := Value;
  RecalcCompressRatio;
end;

procedure TMPKDirectoryEntry.SetUncompressedSize(const Value: integer);
begin
  FUncompressedSize := Value;
  RecalcCompressRatio;
end;

{ TMPKDirectoryEntryList }

function TMPKDirectoryEntryList.GetItems(Index: integer): TMPKDirectoryEntry;
begin
  Result := TMPKDirectoryEntry(inherited Items[Index]);
end;

function TMPKDirectoryEntryList.Has(AName: string): boolean;
begin
  Result := IndexOf(AName) <> -1;
end;

function TMPKDirectoryEntryList.IndexOf(AName: string): integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(AName, Items[Result].Name) then
      exit;

  Result := -1;
end;

end.

unit GameNetPackets;

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
//  Libc,
{$ELSE}
//  Windows,
{$ENDIF !LINUX}
  Classes, SysUtils;

type
  TGNPackIPProtocol = (gnppTCP, gnppUDP);

  TGameNetPacket = class(TObject)
  private
    FPacketDataStart: PChar;
    FPacketDataPos:   PChar;
    FPacketDataEnd:   PChar;
    FSize:        Cardinal;
    FIsFromClient: boolean;
    FIPProtocol:  TGNPackIPProtocol;
    FHandlerName: string;
    FOwnsPacketData:  boolean;
    FConnectionID: Cardinal;
    function GetIsFromServer: boolean;
    procedure FreePacketData;
    function GetIPProtocolStr: string;
    function GetIsFromClientStr: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CopyDataToPacket(AData: Pointer; ASize: integer);
    procedure LinkDataToPacket(AData: Pointer; ASize: integer);
    procedure SaveToFile(const AFName: string);
    procedure LoadFromSream(AStrm: TStream);
    procedure SaveToStream(AStrm: TStream);
    procedure seek(iCount: integer);
    function getByte : BYTE;
    function getShort : WORD;
    function getLong : Cardinal;
    function getPascalString : string;
    function getNullTermString(AMinLen: integer) : string;
    procedure getBytes(var dest; iBytes: integer);
    function AsString : string;
    function EOF : boolean;

    property ConnectionID: Cardinal read FConnectionID write FConnectionID;
    property HandlerName: string read FHandlerName write FHandlerName;
    property Size: Cardinal read FSize;
    property IsFromClient: boolean read FIsFromClient write FIsFromClient;
    property IsFromServer: boolean read GetIsFromServer;
    property IsFromClientStr: string read GetIsFromClientStr;
    property IPProtocol: TGNPackIPProtocol read FIPProtocol write FIPProtocol;
    property IPProtocolStr: string read GetIPProtocolStr;
  end;

  TGNPacketEvent = procedure (Sender: TObject; APacket: TGameNetPacket) of Object;
  TGNPacketHandler = procedure (APacket: TGameNetPacket) of Object;

implementation

{$IFDEF LINUX}
  {$DEFINE PASCAL_GETS}  // assembler is not portable
{$ENDIF}

function BytesToStr(AData: Pointer; ADataSize: integer) : string;
var
  I:  integer;
  sHex1:  string;
  sHex2:  string;
  sAscii: string;
  b:      BYTE;
begin
  sHex1 := '';
  sHex2 := '';
  sAscii := '';
  Result := '';
  for I := 0 to ADataSize - 1 do begin
    if ((I mod 16) = 0) and (I <> 0) then begin
      Result := Result + sHex1 + '- ' + sHex2 + ' ' + sAscii + #13#10;
      sHex1 := '';
      sHex2 := '';
      sAscii := '';
    end;

    b := PBYTEARRAY(AData)[I];
    if (I mod 16) < 8 then
        sHex1 := sHex1 + IntToHex(b, 2) + ' '
    else
      sHex2 := sHex2 + IntToHex(b, 2) + ' ';
    if char(b) in [' '..'~'] then
      sAscii := sAscii + char(b)
    else
      sAscii := sAscii + '.';
  end; { for I }

  while Length(sHex1) < (8 * 3) do
    sHex1 := sHex1 + '   ';
  while Length(sHex2) < (8 * 3) do
    sHex2 := sHex2 + '   ';
  Result := Result + sHex1 + '- ' + sHex2 + ' ' + sAscii;
end;

{ TGameNetPacket }

function TGameNetPacket.AsString: string;
begin
  Result := BytesToStr(FPacketDataStart, FSize);
end;

procedure TGameNetPacket.CopyDataToPacket(AData: Pointer; ASize: integer);
begin
  FreePacketData;

  FOwnsPacketData := true;
  FSize := ASize;
  GetMem(FPacketDataStart, FSize);
  Move(AData^, FPacketDataStart^, FSize);

  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

constructor TGameNetPacket.Create;
begin
  inherited Create;
end;

destructor TGameNetPacket.Destroy;
begin
  FreePacketData;
  inherited Destroy;
end;

function TGameNetPacket.EOF: boolean;
begin
  Result := FPacketDataPos >= FPacketDataEnd;
end;

procedure TGameNetPacket.FreePacketData;
begin
  if Assigned(FPacketDataStart) then begin
    if FOwnsPacketData then
      FreeMem(FPacketDataStart);
    FPacketDataStart := nil;
  end;
end;

function TGameNetPacket.getByte: BYTE;
begin
  Result := BYTE(FPacketDataPos[0]);
  seek(1);
end;

procedure TGameNetPacket.getBytes(var dest; iBytes: integer);
begin
  Move(FPacketDataPos^, dest, iBytes);
  seek(iBytes);
end;

function TGameNetPacket.GetIPProtocolStr: string;
begin
  if FIPProtocol = gnppTCP then
    Result := 'TCP'
  else
    Result := 'UDP';
end;

function TGameNetPacket.GetIsFromClientStr: string;
begin
  if FIsFromClient then
    Result := ' TO  client'
  else
    Result := 'FROM client';
end;

function TGameNetPacket.GetIsFromServer: boolean;
begin
  Result := not FIsFromClient;
end;

{$IFDEF PASCAL_GETS}
function TGameNetPacket.getLong: Cardinal;
begin
  Result := (BYTE(FPacketDataPos[0]) shl 24) or (BYTE(FPacketDataPos[1]) shl 16) or
    (BYTE(FPacketDataPos[2]) shl 8) or BYTE(FPacketDataPos[3]);
  seek(4);
end;
{$ELSE}
function TGameNetPacket.getLong: Cardinal; assembler;
asm
  mov edx, [eax+offset(FPacketDataPos)]
  add [eax+offset(FPacketDataPos)], 4
  mov eax, [edx]
  bswap eax
end;
{$ENDIF}

function TGameNetPacket.getNullTermString(AMinLen: integer): string;
begin
  Result := '';
  while FPacketDataPos <= FPacketDataEnd do begin
    if FPacketDataPos^ = #0 then
      break;

    Result := Result + FPacketDataPos^;
    inc(FPacketDataPos);
    dec(AMinLen);
  end;    { while }

  if FPacketDataPos <= FPacketDataEnd then begin
      { skip trailing null }
    inc(FPacketDataPos);
    dec(AMinLen);
      { enforce minimum bytes read requirement }
    if AMinLen > 0 then
      seek(AMinLen);
  end;  { if pos < size }
end;

function TGameNetPacket.getPascalString: string;
var
  iLen: integer;
begin
  iLen := getByte;
  if iLen = 0 then
    Result := ''
  else begin
    SetString(Result, FPacketDataPos, iLen);
    seek(iLen);
  end;
end;

{$IFDEF PASCAL_GETS}
function TGameNetPacket.getShort: WORD;
begin
  Result := (BYTE(FPacketDataPos[0]) shl 8) or BYTE(FPacketDataPos[1]);
  seek(2);
end;
{$ELSE}
function TGameNetPacket.getShort: WORD; assembler;
asm
  mov edx, [eax+offset(FPacketDataPos)]
  add [eax+offset(FPacketDataPos)], 2
  movzx eax, WORD PTR [edx]
  xchg al, ah
end;
{$ENDIF}

procedure TGameNetPacket.LinkDataToPacket(AData: Pointer; ASize: integer);
begin
  FreePacketData;

  FOwnsPacketData := false;
  FSize := ASize;

  FPacketDataStart := AData;
  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

procedure TGameNetPacket.LoadFromSream(AStrm: TStream);
var
  b:  BYTE;
begin
  FreePacketData;
  AStrm.Read(FSize, sizeof(FSize));
  AStrm.Read(b, sizeof(b));
  FIsFromClient := b = 1;
  AStrm.Read(b, sizeof(b));
  FIPProtocol := TGNPackIPProtocol(b);

  FOwnsPacketData := true;
//  AStrm.Read(FSize, sizeof(FSize));
  GetMem(FPacketDataStart, FSize);
  AStrm.Read(FPacketDataStart^, FSize);

  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

procedure TGameNetPacket.SaveToFile(const AFName: string);
var
  fs:  TFileStream;
begin
  fs := TFileStream.Create(AFName, fmCreate or fmShareDenyWrite);
  SaveToStream(fs);
  fs.Free;
end;

procedure TGameNetPacket.SaveToStream(AStrm: TStream);
var
  b:  BYTE;
begin
  if FIsFromClient then
    b := 1
  else
    b := 0;
  AStrm.Write(b, sizeof(b));
  b := ord(FIPProtocol);
  AStrm.Write(b, sizeof(b));
  AStrm.Write(FSize, sizeof(FSize));
  AStrm.Write(FPacketDataStart^, FSize);
end;

procedure TGameNetPacket.seek(iCount: integer);
var
  pNewPos:    PChar;
begin
  pNewPos := FPacketDataPos + iCount;

  if pNewPos < FPacketDataStart then
    raise Exception.Create('GNPacket (' + FHandlerName + '): Seek before BOF');

  if pNewPos > FPacketDataEnd then
    raise Exception.Create('GNPacket (' + FHandlerName + '): Seek after EOF');

  FPacketDataPos := pNewPos;
end;

end.

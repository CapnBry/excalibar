unit DAOCPackets;

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
  Libc,
{$ELSE}
  Windows, WinSock,
{$ENDIF !LINUX}
  Classes, SysUtils, FrameFns;

type
  TDAOCCryptKey = array[0..11] of byte;

  TTCPFragment = class(TObject)
  private
    FEtherData:     Pointer;
    FEtherDataLen:  Cardinal;
    FPayloadDataPtr:  Pointer;
    FPayloadDataLen:  Cardinal;
    function GetSeqNo: Cardinal;
    function GetAckNo: Cardinal;
    function GetIsAck: boolean;
  public
    constructor CreateFrom(ASegment: TEthernetSegment);
    destructor Destroy; override;

    property PayloadDataPtr: Pointer read FPayloadDataPtr;
    property PayloadDataLen: Cardinal read FPayloadDataLen;
    property SeqNo: Cardinal read GetSeqNo;
    property AckNo: Cardinal read GetAckNo;
    property IsAck: boolean read GetIsAck;
  end;

  TDAOCIPProtocol = (daocpTCP, daocpUDP);

  TDAOCPacket = class(TObject)
  private
    FPacketDataStart: PChar;
    FPacketDataPos:   PChar;
    FPacketDataEnd:   PChar;
    FSize:        Cardinal;
    FIsFromClient: boolean;
    FIPProtocol:  TDAOCIPProtocol;
    FHandlerName: string;
    FOwnsPacketData:  boolean;
    FConnectionID: Cardinal;
    function GetIsFromServer: boolean;
    procedure FreePacketData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CopyDataToPacket(AData: Pointer; ASize: integer);
    procedure LinkDataToPacket(AData: Pointer; ASize: integer);
    procedure SaveToFile(const AFName: string);
    procedure LoadFromSream(AStrm: TStream);
    procedure SaveToStream(AStrm: TStream);
    procedure Decrypt(const AKey: TDAOCCryptKey);
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
    property IPProtocol: TDAOCIPProtocol read FIPProtocol write FIPProtocol;
  end;

  TPacketEvent = procedure (Sender: TObject; APacket: TDAOCPacket) of Object;
  
  TDAOCTCPPacketAssembler = class(TObject)
  private
    FFragmentList:    TList;
    FNextExpectedSeq: Cardinal;
    FPacketDataBuff:  Pointer;
    FPacketDataSize:  Cardinal;
    FPacketDataPos:   Cardinal;
    FIsFromClient:    boolean;
    FOtherSide:       TDAOCTCPPacketAssembler;

    procedure ClearFragmentList;
    procedure AppendFragmentToBuffer(AFragment: TTCPFragment);
    procedure InsertFragmentInOrder(AFragment: TTCPFragment);
  public
    constructor Create(AIsClient: boolean);
    destructor Destroy; override;

    procedure Clear;
    procedure AddFragment(AFragment: TTCPFragment);
    function ParsePacket(AThroughSeq: Cardinal; var APacket: TDAOCPacket) : boolean;

    property IsFromClient: boolean read FIsFromClient write FIsFromClient;
    property NextExpectedSeq: Cardinal read FNextExpectedSeq write FNextExpectedSeq;
    property OtherSide: TDAOCTCPPacketAssembler read FOtherSide write FOtherSide;
  end;

function DAOCCryptKeyToString(const ACryptKey: TDAOCCryptKey) : string;
procedure StringToDAOCCryptKey(const ACryptString: string; var ACryptKey: TDAOCCryptKey);
function BytesToStr(AData: Pointer; ADataSize: integer) : string;

implementation

// {$DEFINE CLEAR_PACKET_BUFFER}
// {$DEFINE PASCAL_GETS}

const
  MAX_EXPECTED_DAOC_PACKET_SIZE = 16384;

procedure ODS(const s: string);
begin
//  WriteLn(s);
//  OutputDebugString(PChar(s));
end;

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

function DAOCCryptKeyToString(const ACryptKey: TDAOCCryptKey) : string;
var
  I:  integer;
begin
  Result := '';
  for I := low(TDAOCCryptKey) to high(TDAOCCryptKey) do
    Result := Result + IntToHex(ACryptKey[I], 2);
end;

procedure StringToDAOCCryptKey(const ACryptString: string; var ACryptKey: TDAOCCryptKey);
var
  I:  integer;
begin
  if Length(ACryptString) <> ((high(TDAOCCryptKey) - low(TDAOCCryptKey) + 1) * 2) then
    raise Exception.Create('Invalid crypt key length');

  for I := low(TDAOCCryptKey) to high(TDAOCCryptKey) do
    ACryptKey[I] := StrToInt('$' + copy(ACryptString, (I * 2) + 1, 2));
end;

{ TTCPFragment }

constructor TTCPFragment.CreateFrom(ASegment: TEthernetSegment);
begin
  inherited Create;

  FEtherDataLen := ASegment.Size;
  FEtherData := ASegment.Data;
  ASegment.ReleaseData;  // ASegment no longer will have any data

    { The payload is lotated after the ether, ip, and tcp header, so add their
      sizes to the FEtherData pointer to get a pointer to payload }
  FPayloadDataPtr := Pointer(
    Cardinal(FEtherData) +
    sizeof(TEthernetHeader) +
    GetIPHeaderLen(PIPHeader(FEtherData)) +
    GetTCPHeaderLen(PTCPHeader(FEtherData))
  );

   { This was: FEtherDataLen - (Cardinal(FPayloadDataPtr) - Cardinal(FEtherData));
     but that does not take into account Ethernet Trailer which may come
     at the end of the packet }
  FPayloadDataLen := ntohs(PIPHeader(FEtherData)^.TotalLength) -
    GetIPHeaderLen(PIPHeader(FEtherData)) -
    GetTCPHeaderLen(PTCPHeader(FEtherData));

    { if the ethernet data len is less than the combined size of
      (EHeader + IHeader + THeader + DataLen) then throw it out
      and wait for the tcp stack to send it again }
  if FEtherDataLen < (FPayloadDataLen + sizeof(TEthernetHeader) +
    GetIPHeaderLen(PIPHeader(FEtherData)) +
    GetTCPHeaderLen(PTCPHeader(FEtherData))) then begin
    ODS('Ethernet frame does not have enough data to hold tcp data.  Dropped.');
    FPayloadDataLen := 0;
  end;

end;

destructor TTCPFragment.Destroy;
begin
  if Assigned(FEtherData) then
    FreeMem(FEtherData);

  inherited Destroy;
end;

function TTCPFragment.GetAckNo: Cardinal;
begin
  if Assigned(FEtherData) then
    Result := ntohl(PTCPHeader(FEtherData)^.AckNumber)
  else
    Result := 0;
end;

function TTCPFragment.GetIsAck: boolean;
begin
  if Assigned(FEtherData) then
    Result := FrameFns.IsAck(PTCPHeader(FEtherData))
  else
    Result := false;
end;

function TTCPFragment.GetSeqNo: Cardinal;
begin
  if Assigned(FEtherData) then
    Result := ntohl(PTCPHeader(FEtherData)^.SeqNumber)
  else
    Result := 0;
end;

{ TDAOCTCPPacketAssembler }

procedure TDAOCTCPPacketAssembler.AddFragment(AFragment: TTCPFragment);
begin
  if AFragment.PayloadDataLen = 0 then begin
    AFragment.Free;
    exit;
  end;

  if AFragment.SeqNo < FNextExpectedSeq then begin
    ODS(Format('Old packet received (%u,%d) when expecting (%u).  Discarding.',
      [AFragment.SeqNo, AFragment.PayloadDataLen, FNextExpectedSeq]));
    AFragment.Free;
    exit;
  end;

  InsertFragmentInOrder(AFragment);

    { if we've got more than 50 fragments unassembled then for some reason
      we're off or something.  Either these are old retransmits, or we've
      dropped a packet in the stream }
  if (FFragmentList.Count > 50) and (FFragmentList.Count mod 50 = 0) then
    ODS(IntToStr(FFragmentList.Count) + ' tcp fragments unassembled.');

(****
  if (AFragment.SeqNo + AFragment.PayloadDataLen) < FNextExpectedSeq then begin
    ODS('Old packet received.  Discarding.');
    AFragment.Free;
    exit;
  end;

  if FragmentIsNext(AFragment) then begin
    AppendFragmentToBuffer(AFragment);
    AFragment.Free;

      { if we added a Fragment to the buffer, we may be able to add the other
        fragments too.  Run through the list and check }
    AFragment := FindNextFragmentInList;
    while Assigned(AFragment) do begin
      AppendFragmentToBuffer(AFragment);
      AFragment.Free;
      AFragment := FindNextFragmentInList;
    end;
  end

  else
      { fragment arrived early.  Wait until we can put it in order }
    FFragmentList.Add(AFragment);

    { if we've got more than 50 fragments unassembled then for some reason
      we're off or something.  Either these are old retransmits, or we've
      dropped a packet in the stream }
  if (FFragmentList.Count > 50) and (FFragmentList.Count mod 50 = 0) then
    ODS(IntToStr(FFragmentList.Count) + ' tcp fragments unassembled.');

//  if FFragmentList.Count > 20 then begin
//    FFragmentList.SaveToFile('C:\Fragmentlist.log');
//    FFragmentList.Clear;
//    raise Exception.Create('Too many fragments, saved to log');
//  end;
***)
end;

procedure TDAOCTCPPacketAssembler.AppendFragmentToBuffer(AFragment: TTCPFragment);
begin
  if (FPacketDataPos + AFragment.PayloadDataLen) < FPacketDataSize then begin
    Move(AFragment.PayloadDataPtr^, (PChar(FPacketDataBuff) + FPacketDataPos)^,
      AFragment.PayloadDataLen);
    inc(FPacketDataPos, AFragment.PayloadDataLen);
  end

  else
    raise Exception.Create('Out of PacketDataBuffer in PacketAssembler');

  FNextExpectedSeq := AFragment.SeqNo + AFragment.PayloadDataLen;
end;

procedure TDAOCTCPPacketAssembler.Clear;
begin
  FPacketDataPos := 0;
  FNextExpectedSeq := 0;
  ClearFragmentList;
end;

procedure TDAOCTCPPacketAssembler.ClearFragmentList;
var
  I:  integer;
begin
  for I := 0 to FFragmentList.Count - 1 do
    TTCPFragment(FFragmentList[I]).Free;
  FFragmentList.Clear;
end;

constructor TDAOCTCPPacketAssembler.Create(AIsClient: boolean);
begin
  inherited Create;

  FFragmentList := TList.Create;

  FIsFromClient := AIsClient;
  FNextExpectedSeq := 0;
    { use a static buffer size.  If they send us any packet > MAX_EXPECTED_DAOC_PACKET_SIZE we'll drop data }
  FPacketDataSize := MAX_EXPECTED_DAOC_PACKET_SIZE;
  GetMem(FPacketDataBuff, FPacketDataSize);
  FPacketDataPos := 0;
{$IFDEF CLEAR_PACKET_BUFFER}
  FillChar(FPacketDataBuff^, FPacketDataSize, 0);
{$ENDIF}
end;

destructor TDAOCTCPPacketAssembler.Destroy;
begin
  ClearFragmentList;
  FFragmentList.Free;

  if Assigned(FPacketDataBuff) then
    FreeMem(FPacketDataBuff);
    
  inherited Destroy;
end;

procedure TDAOCTCPPacketAssembler.InsertFragmentInOrder(
  AFragment: TTCPFragment);
var
  I:  integer;
  dwSeqNo:    Cardinal;
begin
  for I := 0 to FFragmentList.Count - 1 do begin
    dwSeqNo := TTCPFragment(FFragmentList[I]).SeqNo;
    if AFragment.SeqNo = dwSeqNo then begin
      ODS('Replacing fragment: ' + IntToStr(dwSeqNo));
      TTCPFragment(FFragmentList[I]).Free;
      FFragmentList[I] := AFragment;
      exit;
    end;

    if AFragment.SeqNo < dwSeqNo then begin
      ODS('Fragment ' + IntToStr(AFragment.SeqNo) + ' arrived after ' + IntToStr(dwSeqNo));
      FFragmentList.Insert(I, AFragment);
      exit;
    end;
  end;

  FFragmentList.Add(AFragment);
end;

function TDAOCTCPPacketAssembler.ParsePacket(AThroughSeq: Cardinal; var APacket: TDAOCPacket): boolean;
var
  wExpectedPackSize:  WORD;
  dwNewSize:          Cardinal;
  pFragment:  TTCPFragment;
begin
  APacket := nil;
  Result := false;

  while FFragmentList.Count > 0 do begin
    pFragment := TTCPFragment(FFragmentList[0]);
    if pFragment.SeqNo >= AThroughSeq then
      break;

    if pFragment.SeqNo <> FNextExpectedSeq then begin
      ODS('Missing fragments: ' + IntToStr(FNextExpectedSeq) + ' - ' + IntToStr(pFragment.SeqNo));
      FPacketDataPos := 0;
    end;

      { also updates NextExpectedSeq }
    AppendFragmentToBuffer(pFragment);

    pFragment.Free;
    FFragmentList.Delete(0);
  end;  { while fragments to be assembled }

  if FPacketDataPos < 2 then
    exit;

    { The first two bytes of every DAoC application-layer packet is the size
      if the data which follows (in network byte order).  This value is not
      self-inclusive, so we need to read two more bytes than indicated. }
  wExpectedPackSize := ntohs(PWORD(FPacketDataBuff)^);

  if wExpectedPackSize > MAX_EXPECTED_DAOC_PACKET_SIZE then begin
    ODS('Suspiciously large packet expected, attempting resync');
    ClearFragmentList;
    FPacketDataPos := 0;
    exit;
  end;

    { clients have 10+2 unstated bytes, server has 1+2 }
  if FIsFromClient then
    inc(wExpectedPackSize, 12)
  else
    inc(wExpectedPackSize, 3);

  if FPacketDataPos < wExpectedPackSize then
    exit;

  APacket := TDAOCPacket.Create;
    { the first 2 bytes be we added above to account for the ExpectedPacketSize }
  APacket.CopyDataToPacket(PChar(FPacketDataBuff) + 2, wExpectedPackSize - 2);
  Result := true;

  if wExpectedPackSize >= FPacketDataPos then
    FPacketDataPos := 0
  else begin
      { scoot all the data after this packet down to the beginning }
    dwNewSize := FPacketDataPos - wExpectedPackSize;
    Move(PChar(FPacketDataBuff)[wExpectedPackSize], FPacketDataBuff^, dwNewSize);
    FPacketDataPos := dwNewSize;
  end;

{$IFDEF CLEAR_PACKET_BUFFER}
  FillChar(Pointer(Cardinal(FPacketDataBuff) + FPacketDataPos)^, FPacketDataSize - FPacketDataPos, 0);
{$ENDIF}
end;

{ TDAOCPacket }

function TDAOCPacket.AsString: string;
begin
  Result := BytesToStr(FPacketDataStart, FSize);
end;

procedure TDAOCPacket.CopyDataToPacket(AData: Pointer; ASize: integer);
begin
  FreePacketData;

  FOwnsPacketData := true;
  FSize := ASize;
  GetMem(FPacketDataStart, FSize);
  Move(AData^, FPacketDataStart^, FSize);

  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

constructor TDAOCPacket.Create;
begin
  inherited Create;
end;

procedure TDAOCPacket.Decrypt(const AKey: TDAOCCryptKey);
var
  data_pos: Cardinal;
  key_pos:  Cardinal;
  status_vect:  Cardinal;
  seed_1:   Cardinal;
  seed_2:   Cardinal;
  work_val: Cardinal;
  pData:    PChar;
begin
  if not Assigned(FPacketDataStart) then
    exit;
  if FSize = 0 then
    exit;

  pData := FPacketDataStart;
  data_pos := 0;
  key_pos := 0;
  status_vect := 0;
  seed_1 := 1;
  seed_2 := 2;

  repeat
    if key_pos = sizeof(TDAOCCryptKey) then
      key_pos := 0;

    work_val := AKey[key_pos] + data_pos + key_pos;
    seed_1 := work_val * seed_1 + 1;
    seed_2 := work_val + seed_2;

    status_vect := status_vect + (seed_1 * seed_2);
    pData[data_pos] := Char((BYTE(pData[data_pos]) xor status_vect) and $ff);

    inc(data_pos);
    inc(key_pos);
  until data_pos = FSize;
end;

destructor TDAOCPacket.Destroy;
begin
  FreePacketData;
  inherited Destroy;
end;

function TDAOCPacket.EOF: boolean;
begin
  Result := FPacketDataPos >= FPacketDataEnd;
end;

procedure TDAOCPacket.FreePacketData;
begin
  if Assigned(FPacketDataStart) then begin
    if FOwnsPacketData then
      FreeMem(FPacketDataStart);
    FPacketDataStart := nil;
  end;
end;

function TDAOCPacket.getByte: BYTE;
begin
  Result := BYTE(FPacketDataPos[0]);
  seek(1);
end;

procedure TDAOCPacket.getBytes(var dest; iBytes: integer);
begin
  Move(FPacketDataPos^, dest, iBytes);
  seek(iBytes);
end;

function TDAOCPacket.GetIsFromServer: boolean;
begin
  Result := not FIsFromClient;
end;

{$IFDEF PASCAL_GETS}
function TDAOCPacket.getLong: Cardinal;
begin
  Result := (BYTE(FPacketDataPos[0]) shl 24) or (BYTE(FPacketDataPos[1]) shl 16) or
    (BYTE(FPacketDataPos[2]) shl 8) or BYTE(FPacketDataPos[3]);
  seek(4);
end;
{$ELSE}
function TDAOCPacket.getLong: Cardinal; assembler;
asm
  mov edx, [eax+offset(FPacketDataPos)]
  add [eax+offset(FPacketDataPos)], 4
  mov eax, [edx]
  bswap eax
end;
{$ENDIF}

function TDAOCPacket.getNullTermString(AMinLen: integer): string;
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

function TDAOCPacket.getPascalString: string;
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
function TDAOCPacket.getShort: WORD;
begin
  Result := (BYTE(FPacketDataPos[0]) shl 8) or BYTE(FPacketDataPos[1]);
  seek(2);
end;
{$ELSE}
function TDAOCPacket.getShort: WORD; assembler;
asm
  mov edx, [eax+offset(FPacketDataPos)]
  add [eax+offset(FPacketDataPos)], 2
  movzx eax, WORD PTR [edx]
  xchg al, ah
end;
{$ENDIF}

procedure TDAOCPacket.LinkDataToPacket(AData: Pointer; ASize: integer);
begin
  FreePacketData;

  FOwnsPacketData := false;
  FSize := ASize;

  FPacketDataStart := AData;
  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

procedure TDAOCPacket.LoadFromSream(AStrm: TStream);
var
  b:  BYTE;
begin
  FreePacketData;
  AStrm.Read(FSize, sizeof(FSize));
  AStrm.Read(b, sizeof(b));
  FIsFromClient := b = 1;
  AStrm.Read(b, sizeof(b));
  FIPProtocol := TDAOCIPProtocol(b);

  FOwnsPacketData := true;
  GetMem(FPacketDataStart, FSize);
  AStrm.Read(FPacketDataStart^, FSize);

  FPacketDataPos := FPacketDataStart;
  FPacketDataEnd := FPacketDataStart + FSize;
end;

procedure TDAOCPacket.SaveToFile(const AFName: string);
var
  fs:  TFileStream;
begin
  fs := TFileStream.Create(AFName, fmCreate or fmShareDenyWrite);
  SaveToStream(fs);
  fs.Free;
end;

procedure TDAOCPacket.SaveToStream(AStrm: TStream);
var
  b:  BYTE;
begin
  AStrm.Write(FSize, sizeof(FSize));
  if FIsFromClient then
    b := 1
  else
    b := 0;
  AStrm.Write(b, sizeof(b));
  b := ord(FIPProtocol);
  AStrm.Write(b, sizeof(b));
  AStrm.Write(FPacketDataStart^, FSize);
end;

procedure TDAOCPacket.seek(iCount: integer);
var
  pNewPos:    PChar;
begin
  pNewPos := FPacketDataPos + iCount;

  if pNewPos < FPacketDataStart then
    raise Exception.Create('DAOCPacket: Seek before BOF');

  if pNewPos > FPacketDataEnd then
    raise Exception.Create('DAOCPacket: Seek after EOF');

  FPacketDataPos := pNewPos;
end;

end.

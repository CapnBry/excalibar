unit FrameFns;

interface

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Winsock, Windows,
{$ENDIF !LINUX}
  SysUtils, Classes, SyncObjs;
  
type
  PEthernetHeader = ^TEthernetHeader;
  TEthernetHeader = packed record
    DestAddr:     array[0..5] of byte;
    SrcAddr:      array[0..5] of byte;
    FrameType:    WORD;
  end;

  PEthernetFrame = ^TEthernetFrame;
  TEthernetFrame = packed record
    Header:       TEthernetHeader;
    Data:         array[0..1499] of byte;
  end;

  PIPHeader = ^TIPHeader;
  TIPHeader = packed record
    EtherHead:      TEthernetHeader;
    VersionAndHeaderLen:      byte;
    ServiceType:    byte;
    TotalLength:    WORD;
    FragmentID:     WORD;
    Flags:          WORD;
    TTL:            byte;
    Protocol:       byte;
    CheckSum:       WORD;
    SrcAddr:        Cardinal;
    DestAddr:       Cardinal;
  end;

  PTCPHeader = ^TTCPHeader;
  TTCPHeader = packed record
    IPHead:         TIPHeader;
    SrcPort:        WORD;
    DestPort:       WORD;
    SeqNumber:      Cardinal;
    AckNumber:      Cardinal;
    DataOffsetAndReserved:  byte;
    ECNAndControlBits:      byte;
    Window:         WORD;
    Checksum:       WORD;
    UrgentPointer:  WORD;
    TCPOption:      array[0..3] of byte;
  end;

  PUDPHeader = ^TUDPHeader;
  TUDPHeader = packed record
    IPHead:         TIPHeader;
    SrcPort:        WORD;
    DestPort:       WORD;
    TotalLen:       WORD;
    Checksum:       WORD;
  end;

    { 28 bytes total }
  TNXPacketHeader = packed record
    qwTicks: Int64;
    wSize1: WORD;
    wSize2: WORD;
    Buff:  array[0..15] of byte;
  end;

  TNXFileHeader = packed record
    SIG:          array[0..11] of char;
    dwStart:      Cardinal;  // UNIX time
    dwNumPackets: Cardinal;
    Buff:         array[20..51] of byte;
    qwTicks:      Int64;
    dwVal:        Cardinal;
    Padding:      array[64..127] of byte;
  end;

  TEthernetSegment = class(TObject)
  private
    FDataLen:   Cardinal;
    FData:      Pointer;
  protected
    FNext:    TEthernetSegment;
  public
    constructor CreateFor(AData: Pointer; ADataLen: Cardinal);
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream; ACapLen: Cardinal);
    procedure Clear;
    procedure ReleaseData;

    function AsEthernet: PEthernetFrame;
    function AsIP : PIPHeader;
    function AsTCP : PTCPHeader;
    function AsUDP : PUDPHeader;
    function AsString : string;

    property Size: Cardinal read FDataLen;
    property Data: Pointer read FData;
  end;

  TEthernetSegmentList = class(TObject)
  private
    FListHead:  TEthernetSegment;
    FListTail:  TEthernetSegment;
    FListLock:  TCriticalSection;
    FCount:     integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ASegment: TEthernetSegment);
    procedure Clear;
    function GetFirst(var ASegment: TEthernetSegment) : boolean;

    property Count: integer read FCount;
  end;

  TEthernetSegmentNotify = procedure (Sender: TObject; ASegment: TEthernetSegment) of Object;

const
    { NET_ consts are in network byte order }
  NET_IP_TYPE = $0008;

  IP_TYPE = $0800;
  ICMP_TYPE = $0806;

  SOL_TCP = $06;
  SOL_UDP = $11;

function my_inet_ntoa(inaddr : Cardinal) : string;
function my_inet_htoa(inaddr : Cardinal) : string;
function EtherFrameToString(pFrame : PEthernetFrame) : string;
function IPHeaderToString(pHeader : PIPHeader) : string;
function TCPHeaderToString(pHeader : PTCPHeader) : string;
function UDPHeaderToString(pHeader : PUDPHeader) : string;
function GetIPHeaderLen(PHeader: PIPHeader) : WORD;
function GetTCPHeaderLen(pHeader: PTCPHeader) : WORD;

function IsFin(pFrame : PTCPHeader) : boolean;
function IsSyn(pFrame : PTCPHeader) : boolean;
function IsRst(pFrame : PTCPHeader) : boolean;
function IsPsh(pFrame : PTCPHeader) : boolean;
function IsAck(pFrame : PTCPHeader) : boolean;
function IsUrg(pFrame : PTCPHeader) : boolean;

implementation

function my_inet_ntoa(inaddr : Cardinal) : string;
var
  inadd:    in_addr;
begin
  inadd.S_addr := inaddr;
  Result := inet_ntoa(inadd);
end;

function my_inet_htoa(inaddr : Cardinal) : string;
var
  inadd:    in_addr;
begin
  inadd.S_addr := htonl(inaddr);
  Result := inet_ntoa(inadd);
end;

function EtherFrameToString(pFrame : PEthernetFrame) : string;
begin
  if not Assigned(pFrame) then
    exit;

  with pFrame^.Header do begin
    Result := Format('Destination: [%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x]'#13#10,
      [DestAddr[0], DestAddr[1], DestAddr[2], DestAddr[3],
      DestAddr[4], DestAddr[5]]);
    Result := Result + Format('Source: [%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x]'#13#10,
      [SrcAddr[0], SrcAddr[1], SrcAddr[2], SrcAddr[3],
       SrcAddr[4], SrcAddr[5]]);
    Result := Result + Format('Type: 0x%4.4x'#13#10, [htons(FrameType)]);

    case htons(FrameType) of
      IP_TYPE:
        Result := Result + IPHeaderToString(PIPHeader(pFrame));
      ICMP_TYPE:
        Result := Result + 'ICMP Protocol'#13#10;
      else
        Result := Result + 'UnSupported packet type'#13#10
    end;   { case }
  end;  { with }
end;

function IPHeaderToString(pHeader : PIPHeader) : string;
begin
  Result := '-- Internet Protocol (IP) --'#13#10;
  with pHeader^ do begin
    Result := Result + Format('Version: %d'#13#10, [(VersionAndHeaderLen and $F0) shr 4]);
    Result := Result + Format('Header Length: %d'#13#10, [VersionAndHeaderLen and $0F]);
    Result := Result + Format('Service Type: 0x%2.2x'#13#10, [ServiceType]);
    Result := Result + Format('Total Length: %d'#13#10, [ntohs(TotalLength)]);
    Result := Result + Format('Fragment ID: %d'#13#10, [FragmentID]);
    Result := Result + Format('Flags: 0x%4.4x'#13#10, [Flags]);
    Result := Result + Format('TTL: %d'#13#10, [TTL]);
    Result := Result + Format('Protocol Type: 0x%2.2x'#13#10, [Protocol]);
    Result := Result + Format('Checksum: 0x%4.4x'#13#10, [htons(Checksum)]);
    Result := Result + Format('IP Address: %s', [my_inet_ntoa(SrcAddr)]);
    Result := Result + Format('->%s'#13#10, [my_inet_ntoa(DestAddr)]);

    case pHeader^.Protocol of
      SOL_TCP:
        Result := Result + TCPHeaderToString(PTCPHeader(pHeader));
      SOL_UDP:
        Result := Result + UDPHeaderToString(PUDPHeader(pHeader));
      else
        Result := Result + 'Unsupported IP type'#13#10;
    end;  { case }
  end;  { with pHeader }
end;

function TCPHeaderToString(pHeader : PTCPHeader) : string;
var
  iHeaderBytes:  integer;
begin
  Result := '-- Transmission Control Protocol (TCP) --'#13#10;
  with pHeader^ do begin
    Result := Result + Format('Port: %d', [htons(SrcPort)]);
    Result := Result + Format('->%d'#13#10, [htons(DestPort)]);
    Result := Result + Format('Sequence Number: %u'#13#10, [htonl(SeqNumber)]);
    Result := Result + Format('Acknowledgement Number: %u'#13#10, [htonl(AckNumber)]);
    Result := Result + Format('Data Offset: %d'#13#10, [(DataOffsetAndReserved and $F0) shr 4]);
    Result := Result + Format('Reserved: %d'#13#10, [DataOffsetAndReserved and $0F]);
    Result := Result + Format('ECN: 0x%2.2x'#13#10, [(ECNAndControlBits and $C0) shr 6]);
    Result := Result + Format('Control Bits: 0x%2.2x'#13#10, [(ECNAndControlBits and $3F)]);
    Result := Result + Format('Window Size: %d'#13#10, [htons(Window)]);
    Result := Result + Format('Checksum: 0x%4.4x'#13#10, [htons(Checksum)]);
    Result := Result + Format('Urgent Pointer: 0x%4.4x'#13#10, [htons(UrgentPointer)]);

    iHeaderBytes := GetTCPHeaderLen(pHeader);
    if iHeaderBytes >= 24 then begin
      Result := Result + Format('TCP Option: 0x%2.2x%2.2x%2.2x%2.2x'#13#10,
        [TCPOption[0], TCPOption[1], TCPOption[2], TCPOption[3]]);

      if iHeaderBytes > 24 then
        Result := Result + Format('FramePadding: %d bytes'#13#10, [iHeaderBytes - 24]);
    end;
  end;  { with }
end;

function UDPHeaderToString(pHeader : PUDPHeader) : string;
begin
  Result := '-- User Datagram Protocol ---'#13#10;
  with pHeader^ do begin
    Result := Result + Format('Port: %d', [htons(SrcPort)]);
    Result := Result + Format('->%d'#13#10, [htons(DestPort)]);
    Result := Result + Format('Total Length: %d'#13#10, [htons(TotalLen)]);
    Result := Result + Format('Checksum: 0x%4.4x'#13#10, [htons(CheckSum)]);
  end;  { with }
end;

function IsFin(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $01) <> 0;
end;

function IsSyn(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $02) <> 0;
end;

function IsRst(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $04) <> 0;
end;

function IsPsh(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $08) <> 0;
end;

function IsAck(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $10) <> 0;
end;

function IsUrg(pFrame : PTCPHeader) : boolean;
begin
  Result := (pFrame^.ECNAndControlBits and $20) <> 0;
end;

function GetIPHeaderLen(PHeader: PIPHeader) : WORD;
(*** IP header length in bytes ***)
begin
    { header length is in 32-bit words }
  Result := (pHeader^.VersionAndHeaderLen and $0F shl 2);
end;

function GetTCPHeaderLen(pHeader: PTCPHeader) : WORD;
(*** TCP header length in bytes ***)
begin
    { header length is in 32-bit words }
    { same as shifting right 4 and then multiplying by 4 }
  Result := (pHeader^.DataOffsetAndReserved and $F0) shr 2;
end;

{ TEthernetSegment }

function TEthernetSegment.AsEthernet: PEthernetFrame;
begin
  Result := PEthernetFrame(FData);
end;

function TEthernetSegment.AsIP: PIPHeader;
begin
  Result := PIPHeader(FData);
end;

function TEthernetSegment.AsString: string;
begin
  Result := EtherFrameToString(AsEthernet);
end;

function TEthernetSegment.AsTCP: PTCPHeader;
begin
  Result := PTCPHeader(FData);
end;

function TEthernetSegment.AsUDP: PUDPHeader;
begin
  Result := PUDPHeader(FData);
end;

procedure TEthernetSegment.Clear;
begin
  if Assigned(FData) then begin
    FreeMem(FData);
    FData := nil;
  end;

  FDataLen := 0;
end;

constructor TEthernetSegment.CreateFor(AData: Pointer; ADataLen: Cardinal);
begin
  inherited Create;
  FDataLen := ADataLen;
  GetMem(FData, FDataLen);
  Move(AData^, FData^, FDataLen);
end;

destructor TEthernetSegment.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TEthernetSegment.LoadFromStream(AStream: TStream; ACapLen: Cardinal);
begin
  Clear;
  AStream.Read(FDataLen, sizeof(FDataLen));
  GetMem(FData, FDataLen);
  if (ACapLen > 0) and (ACapLen < FDataLen) then begin
    AStream.Read(FData^, ACapLen);
    FillChar((PChar(FData) + ACapLen)^, FDataLen - ACapLen, 0);
  end
  else
  AStream.Read(FData^, FDataLen);
end;

procedure TEthernetSegment.ReleaseData;
(*** Like clear, but does not free the memory ***)
begin
  FData := nil;
  FDataLen := 0;
end;

procedure TEthernetSegment.SaveToStream(AStream: TStream);
begin
  AStream.Write(FDataLen, sizeof(FDataLen));
  AStream.Write(FData^, FDataLen);
end;

{ TEthernetSegmentList }

procedure TEthernetSegmentList.Add(ASegment: TEthernetSegment);
begin
  FListLock.Enter;
  try
    ASegment.FNext := nil;

    if Assigned(FListHead) then begin
      FListTail.FNext := ASegment;
      FListTail := ASegment;
    end
    else begin
      FListHead := ASegment;
      FListTail := ASegment;
    end;

    inc(FCount);
  finally
    FListLock.Leave;
  end;
end;

procedure TEthernetSegmentList.Clear;
begin
  FListLock.Enter;
  try
    FCount := 0;
    while Assigned(FListHead) do begin
      FListTail := FListHead.FNext;  // using Tail as a temp pointer, no longer tail
      FListHead.Free;
      FListHead := FListTail;
    end;

    FListHead := nil;
    FListTail := nil;
  finally
    FListLock.Leave;
  end;
end;

constructor TEthernetSegmentList.Create;
begin
  inherited Create;
  FCount := 0;

  FListHead := nil;
  FListTail := nil;

  FListLock := TCriticalSection.Create;
end;

destructor TEthernetSegmentList.Destroy;
begin
  Clear;
  FreeAndNil(FListLock);
  inherited Destroy;
end;

function TEthernetSegmentList.GetFirst(var ASegment: TEthernetSegment): boolean;
begin
  FListLock.Enter;
  try
    ASegment := FListHead;
    if Assigned(FListHead) then begin
      FListHead := FListHead.FNext;
        { if this is the last item, clear the tail }
      if not Assigned(FListHead) then
        FListTail := nil;
      dec(FCount);
      Result := true;
    end

    else
      Result := false;
  finally
    FListLock.Leave;
  end;
end;

end.

unit FrameFns;

interface

uses
  Winsock, SysUtils, Windows;
  
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
    SrcAddr:        DWORD;
    DestAddr:       DWORD;
  end;

  PTCPHeader = ^TTCPHeader;
  TTCPHeader = packed record
    IPHead:         TIPHeader;
    SrcPort:        WORD;
    DestPort:       WORD;
    SeqNumber:      DWORD;
    AckNumber:      DWORD;
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
    dwStart:      DWORD;  // UNIX time
    dwNumPackets: DWORD;
    Buff:         array[20..51] of byte;
    qwTicks:      Int64;
    dwVal:        DWORD;
    Padding:      array[64..127] of byte;
  end;

const
    { NET_ consts are in network byte order }
  NET_IP_TYPE = $0008;

  IP_TYPE = $0800;
  ICMP_TYPE = $0806;

  SOL_TCP = $06;
  SOL_UDP = $11;

function my_inet_ntoa(inaddr : DWORD) : string;
function my_inet_htoa(inaddr : DWORD) : string;
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

function my_inet_ntoa(inaddr : DWORD) : string;
var
  inadd:    in_addr;
begin
  inadd.S_addr := inaddr;
  Result := inet_ntoa(inadd);
end;

function my_inet_htoa(inaddr : DWORD) : string;
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

end.

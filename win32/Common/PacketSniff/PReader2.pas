unit PReader2;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Contnrs, Registry, Messages,
  NtDdNDIS, Packet32, PCAP, bpf, FrameFns
{$IFDEF VER130}
,Forms
{$ENDIF}
;

type
  TEthernetSegment = class(TObject)
  private
    FDataLen: DWORD;
    FData: Pointer;
  protected
    FNext:    TEthernetSegment;
  public
    constructor CreateFor(AData: Pointer; ADataLen: DWORD);
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream; ACapLen: DWORD);
    procedure Clear;
    procedure ReleaseData;

    function AsEthernet: PEthernetFrame;
    function AsIP : PIPHeader;
    function AsTCP : PTCPHeader;
    function AsUDP : PUDPHeader;
    function AsString : string;

    property Size: DWORD read FDataLen;
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

  TPacketReader2 = class(TThread)
  private
    FDeviceName:    string;
    FDeviceList:    TStringList;
    FAdapterList:   TStringList;
    FActive:        boolean;
    FAdapter:       PAdapter;
    FPacket:        PPacket;
    FPacketBuffer:  Pointer;
    FAdapterBuffSize: integer;
    FPacketBuffSize:  integer;
    FReadTimeout:     integer;
    FBPFilter: Pbpf_program;
    hSegmentNotifyWhd:  HWND;
    FSegmentList:   TEthernetSegmentList;

    FOnEthernetSegment: TEthernetSegmentNotify;

    function GetAdapterList: TStrings;
    procedure SetActive(const Value: boolean);
    procedure SetDeviceName(const Value: string);
    function GetDeviceList: TStrings;
  protected
    procedure InitializeAdapter;
    procedure InitializePacket;
    procedure DestroyAdapter;
    procedure DestroyPacket;
    procedure DoPacketCapLoop;
    procedure DoProcessPacket;
    procedure DoProcessPacketSegment(pData: Pointer; caplen, datalen: DWORD);
    procedure SegmentNotifyProc(var Msg: TMessage);
    procedure ProcessSegmentList;
    procedure Execute; override;
  public
    constructor CreateInst;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure WaitForClose;

    property Active: boolean read FActive write SetActive;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property AdapterList: TStrings read GetAdapterList;
    property DeviceList: TStrings read GetDeviceList;
    property BPFilter: Pbpf_program read FBPFilter write FBPFilter;

    property OnEthernetSegment: TEthernetSegmentNotify read FOnEthernetSegment
      write FOnEthernetSegment;
  end;

implementation

const
  WM_SEGMENT_NOTIFY = WM_USER + 1;

function DeviceNameToAdapterName_W2k(const sDevice: string) : string;
(*** Convert a Win2k device: \DEVICE\NPF_{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
  to the real adapter name like: LNE100TX Fast Ethernet Adapter Version 1.0 ***)
var
  Reg:  TRegINIFile;
  sl:   TStringList;
  I:    integer;
begin
  if StrLIComp(PChar(sDevice), '\Device\NPF_', 12) <> 0 then begin
    Result := sDevice;
    exit;
  end;

  Result := copy(sDevice, 13, Length(sDevice) - 12);

  Reg := TRegINIFile.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\NetworkCards') then begin
      sl := TStringList.Create;
      Reg.ReadSections(sl);
      for I := 0 to sl.Count - 1 do begin
        if Reg.ReadString(sl[I], 'ServiceName', '') = Result then begin
          Result := Reg.ReadString(sl[I], 'Description', Result);
          break;
        end;
      end;  { for }
      sl.Free;
    end;  { if openkey }
  finally
    Reg.Free;
  end;
end;

{ TPacketReader2 }

procedure TPacketReader2.Close;
begin
  if not FActive then
    exit;

  FActive := false;
  if Suspended then
    Resume;
end;

constructor TPacketReader2.CreateInst;
begin
  FAdapterBuffSize := 512 * 1024;
  FPacketBuffSize := 96 * 1024;
  FReadTimeout := 1000;

  FSegmentList := TEthernetSegmentList.Create;

  hSegmentNotifyWhd := AllocateHWnd(SegmentNotifyProc);

  inherited Create(true);
end;

destructor TPacketReader2.Destroy;
begin
  Terminate;
  if Suspended then
    Resume;

  if Assigned(FAdapterList) then
    FreeAndNil(FAdapterList);

  WaitFor;

  FreeAndNil(FSegmentList);

  inherited Destroy;
end;

procedure TPacketReader2.DestroyAdapter;
begin
  if not Assigned(FAdapter) then
    exit;

  PacketCloseAdapter(FAdapter);
  FAdapter := nil;
end;

procedure TPacketReader2.DestroyPacket;
begin
  if not Assigned(FPacket) then
    exit;

  PacketFreePacket(FPacket);
  FPacket := nil;

  FreeMem(FPacketBuffer);
  FPacketBuffer := nil;
end;

procedure TPacketReader2.DoPacketCapLoop;
begin
  if not Assigned(FPacket) then
    InitializePacket;
  if not Assigned(FAdapter) then
    InitializeAdapter;

  if PacketReceivePacket(FAdapter, FPacket, true) then
    if FPacket.ulBytesReceived > 0 then
      DoProcessPacket;
end;

procedure TPacketReader2.DoProcessPacket;
var
  dwCntLeft:  DWORD;
  bhdr:       Pbpf_hdr;
  dwActualBytesInSegment: DWORD;
begin
    { we have to have at least enough bytes to have a header }
  if FPacket.ulBytesReceived < sizeof(Tbpf_hdr) then
    exit;

  dwCntLeft := FPacket.ulBytesReceived;
  bhdr := Pbpf_hdr(FPacketBuffer);
  while dwCntLeft > 0 do begin
    DoProcessPacketSegment(Pointer(DWORD(bhdr) + bhdr^.bh_hdrlen),
      bhdr^.bh_caplen, bhdr^.bh_datalen);

    dwActualBytesInSegment := BPF_WORDALIGN(bhdr^.bh_hdrlen + bhdr^.bh_caplen);
    bhdr := Pbpf_hdr(DWORD(bhdr) + dwActualBytesInSegment);
    dec(dwCntLeft, dwActualBytesInSegment);
  end;  { while cnt left }

  PostMessage(hSegmentNotifyWhd, WM_SEGMENT_NOTIFY, 0, 0);
end;

procedure TPacketReader2.DoProcessPacketSegment(pData: Pointer; caplen,
  datalen: DWORD);
var
  pSegment:   TEthernetSegment;
begin
  // OutputDebugString(PChar(Format('PacketSegment 0x%8.8p cap(%d) data(%d)',
  //  [pData, caplen, datalen])));

  if caplen <> datalen then
    exit;

  if datalen < sizeof(TIPHeader) then
    exit;

  if not (PIPHeader(pData)^.Protocol in [SOL_TCP, SOL_UDP]) then
    exit;

  pSegment := TEthernetSegment.CreateFor(pData, datalen);
  FSegmentList.Add(pSegment);
end;

procedure TPacketReader2.Execute;
begin
  Priority := tpHigher;
  while not Terminated do begin
    if FActive then
      DoPacketCapLoop
    else if not Terminated then begin
      DestroyAdapter;
      DestroyPacket;
      Suspend;
    end;
  end;  { while not Terminated }

  DestroyAdapter;
  DestroyPacket;
end;

function TPacketReader2.GetAdapterList: TStrings;
var
  I: Integer;
begin
  if not Assigned(FAdapterList) then begin
    FAdapterList := TStringList.Create;

      { set up the adapter list with real names }
    for I := 0 to GetDeviceList.Count - 1 do
      FAdapterList.Add(DeviceNameToAdapterName_W2k(FDeviceList[I]));
  end;  { if not assigned FAdapterList }

  Result := FAdapterList;
end;

function TPacketReader2.GetDeviceList: TStrings;
var
  s:    string;
begin
  if not Assigned(FDeviceList) then begin
    FDeviceList := TStringList.Create;
    FDeviceList.CommaText := Pcap_getAdapternames(',', s);
  end;

  Result := FDeviceList;
end;

procedure TPacketReader2.InitializeAdapter;
begin
  if Assigned(FAdapter) then
    exit;

  FAdapter := PacketOpenAdapter(PChar(FDeviceName));
 	if not PacketSetHwFilter(FAdapter, NDIS_PACKET_TYPE_ALL_LOCAL) then  // NDIS_PACKET_TYPE_PROMISCUOUS)
    OutputDebugString('PacketSetHwFilter failed');
  if Assigned(FBPFilter) then
    if not PacketSetBpf(FAdapter, FBPFilter) then
      OutputDebugString('PacketSetBpf failed');
  if not PacketSetBuff(FAdapter, FAdapterBuffSize) then
    OutputDebugString('PacketSetBuff failed');
  if not PacketSetReadTimeout(FAdapter, FReadTimeout) then
    OutputDebugString('PacketSetReadTimeout failed');
end;

procedure TPacketReader2.InitializePacket;
begin
  if Assigned(FPacket) then
    exit;

  FPacket := PacketAllocatePacket;
  GetMem(FPacketBuffer, FPacketBuffSize);
  PacketInitPacket(FPacket, FPacketBuffer, FPacketBuffSize);   
end;

procedure TPacketReader2.Open;
begin
  if FActive then
    exit;

  if FDeviceName = '' then
    raise Exception.Create('DeviceName not assigned');
    
  FActive := true;
  if Suspended then
    Resume;
end;

procedure TPacketReader2.ProcessSegmentList;
var
  pSegment:   TEthernetSegment;
begin
  while FSegmentList.GetFirst(pSegment) do begin
    if Assigned(FOnEthernetSegment) then
      FOnEthernetSegment(Self, pSegment);
    pSegment.Free;
  end;  { while pSegment }
end;

procedure TPacketReader2.SegmentNotifyProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_SEGMENT_NOTIFY then
    ProcessSegmentList;
end;

procedure TPacketReader2.SetActive(const Value: boolean);
begin
  if Value then
    Open
  else
    Close;
end;

procedure TPacketReader2.SetDeviceName(const Value: string);
begin
  FDeviceName := Value;
end;

procedure TPacketReader2.WaitForClose;
begin
  Close;
  while not Suspended do
    sleep(100);
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

constructor TEthernetSegment.CreateFor(AData: Pointer; ADataLen: DWORD);
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

procedure TEthernetSegment.LoadFromStream(AStream: TStream; ACapLen: DWORD);
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

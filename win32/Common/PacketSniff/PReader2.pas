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
    FPromiscuous:   boolean;

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
    function DeviceNameForAdapter(const AAdapterName: string) : string;

    property Active: boolean read FActive write SetActive;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property AdapterList: TStrings read GetAdapterList;
    property DeviceList: TStrings read GetDeviceList;
    property BPFilter: Pbpf_program read FBPFilter write FBPFilter;
    property Promiscuous: boolean read FPromiscuous write FPromiscuous; 

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
  DeallocateHWnd(hSegmentNotifyWhd);
  
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

function TPacketReader2.DeviceNameForAdapter(const AAdapterName: string): string;
var
  iIdx:   integer;
begin
  iIdx := GetAdapterList.IndexOf(AAdapterName);
  if iIdx <> -1 then
    Result := GetDeviceList[iIdx]
  else
    Result := '';
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
var
  wCaptureMode: DWORD;
begin
  if Assigned(FAdapter) then
    exit;

  FAdapter := PacketOpenAdapter(PChar(FDeviceName));
  if FPromiscuous then
    wCaptureMode := NDIS_PACKET_TYPE_PROMISCUOUS
  else
    wCaptureMode := NDIS_PACKET_TYPE_ALL_LOCAL;
 	if not PacketSetHwFilter(FAdapter, wCaptureMode) then
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

end.

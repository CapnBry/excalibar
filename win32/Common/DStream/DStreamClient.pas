unit DStreamClient;

interface

uses
  SysUtils, Classes, IdException, IdTCPClient, DStreamDefs;

type
  TDStreamDAOCConnectionDetailsNotify = procedure (Sender: TObject;
    AConnectionID: Cardinal; AServerIP: Cardinal; AServerPort: WORD;
    AClientIP: Cardinal; AClientPort: WORD) of object;
  TDStreamDAOCDataNotify = procedure (Sender: TObject;
    AConnectionID: Cardinal; AIsFromClient: boolean; AIsTCP: boolean;
    AData: Pointer; ADataLen: integer) of object;

  TDStreamClient = class(TThread)
  private
    FClientSock:    TIdTCPClient;
    FCurrentPacket: PDStream_Header;

    FPort: integer;
    FHost: string;
    FOnDSPacket: TDStreamPacketNotify;
    FActive: boolean;
    FReconnectDelay: Cardinal;
    FOnDAOCDisconnect: TDStreamDAOCConnectionDetailsNotify;
    FOnDAOCConnect: TDStreamDAOCConnectionDetailsNotify;
    FOnDAOCData: TDStreamDAOCDataNotify;
  protected
    procedure DoRead;
    procedure DoConnect;
    procedure DoOnConnectError(E: EIdSocketError);
    procedure DoOnNewPacket;
    procedure Execute; override;

    procedure DoOnDAOCConnectionOpened(AData: Pointer); virtual;
    procedure DoOnDAOCConnectionClosed(AData: Pointer); virtual;
    procedure DoOnDAOCConnectionData(AData: Pointer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetConnectString(const AHostPort: string);
    procedure Shutdown;
    procedure Open;
    procedure Close;

    property Active: boolean read FActive write FActive;
    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property ReconnectDelay: Cardinal read FReconnectDelay write FReconnectDelay;

    property OnDSPacket: TDStreamPacketNotify read FOnDSPacket write FOnDSPacket;
    property OnDAOCConnect: TDStreamDAOCConnectionDetailsNotify read FOnDAOCConnect write FOnDAOCConnect;
    property OnDAOCDisconnect: TDStreamDAOCConnectionDetailsNotify read FOnDAOCDisconnect write FOnDAOCDisconnect;
    property OnDAOCData: TDStreamDAOCDataNotify read FOnDAOCData write FOnDAOCData; 
  end;

implementation

{ TDStreamClient }

procedure TDStreamClient.Close;
begin
  if not FActive then
    exit;

  FActive := false;
  if FClientSock.Connected then
    FClientSock.Disconnect;
end;

constructor TDStreamClient.Create;
begin
  FClientSock := TIdTCPClient.Create(nil);
  FPort := DEFAULT_DSTREAM_PORT;
  FReconnectDelay := 1000;

  inherited Create(false);
end;

destructor TDStreamClient.Destroy;
begin
  FreeAndNil(FClientSock);
  inherited;
end;

procedure TDStreamClient.DoConnect;
begin
  FClientSock.Host := FHost;
  FClientSock.Port := FPort;
  try
    FClientSock.Connect;
  except
    On E: EIdSocketError do
      DoOnConnectError(E);
  end;
end;

procedure TDStreamClient.DoOnConnectError(E: EIdSocketError);
begin
  sleep(FReconnectDelay);
  ; // ??
end;

procedure TDStreamClient.DoOnDAOCConnectionClosed(AData: Pointer);
begin
  if Assigned(FOnDAOCDisconnect) then
    with PDStream_DAOCConnectionDetails(AData)^ do
      FOnDAOCDisconnect(Self, ConnectionID, ServerIP, ServerPort, ClientIP, ClientPort);
end;

procedure TDStreamClient.DoOnDAOCConnectionData(AData: Pointer);
var
  IsFromClient: boolean;
  IsTCP:  boolean;
  pData:  Pointer;
begin
  with PDStream_DAOCDataHeader(AData)^ do begin
    IsFromClient := Origin = 1;
    IsTCP := Protocol = 0;
    pData := Pointer(Cardinal(AData) + sizeof(TDStream_DAOCDataHeader));

    if Assigned(FOnDAOCData) then
      FOnDAOCData(Self, ConnectionID, IsFromClient, IsTCP, pData, DataSize);
  end;  { with PDStream_DAOCDataHeader^ }     
end;

procedure TDStreamClient.DoOnDAOCConnectionOpened(AData: Pointer);
begin
  if Assigned(FOnDAOCConnect) then
    with PDStream_DAOCConnectionDetails(AData)^ do
      FOnDAOCConnect(Self, ConnectionID, ServerIP, ServerPort, ClientIP, ClientPort);
end;

procedure TDStreamClient.DoOnNewPacket;
  { this function is synchronized to the main thread }
var
  pData:  Pointer;
begin

  if Assigned(FOnDSPacket) then
    FOnDSPacket(Self, FCurrentPacket);

  pData := DStreamPointToData(FCurrentPacket);
  
  case FCurrentPacket^.command_id of
    DPACKET_DAOC_CONNECTION_OPENED:
        DoOnDAOCConnectionOpened(pData);
    DPACKET_DAOC_CONNECTION_CLOSED:
        DoOnDAOCConnectionClosed(pData);
    DPACKET_DAOC_DATA:
        DoOnDAOCConnectionData(pData);
  end;
end;

procedure TDStreamClient.DoRead;
var
  RecvHeader:   TDStream_Header;
  pRecvBuffer:  PDStream_Header;
  pDataDest:    Pointer;
begin
  FClientSock.ReadBuffer(RecvHeader, sizeof(RecvHeader));
  if RecvHeader.total_length > sizeof(RecvHeader) then begin
      { get enough memory to store the total new packet, and copy the
        existing header into it }
    GetMem(pRecvBuffer, RecvHeader.total_length);
    Move(RecvHeader, pRecvBuffer^, sizeof(RecvHeader));

      { point past the header for the next read }
    pDataDest := DStreamPointToData(pRecvBuffer);
    FClientSock.ReadBuffer(pDataDest^, RecvHeader.total_length - sizeof(RecvHeader));

    FCurrentPacket := pRecvBuffer;
    Synchronize(DoOnNewPacket);
    FreeMem(pRecvBuffer);
  end;
end;

procedure TDStreamClient.Execute;
begin
  while not Terminated do begin
    if FActive then
      if FClientSock.Connected then
        DoRead
      else
        DoConnect
    else
      sleep(1000);  // todo:  do a wait here.  Polling is fucking asinine
  end;  { while !Terminated }
end;

procedure TDStreamClient.Open;
begin
  if FActive then
    exit;

  FActive := true;
end;

procedure TDStreamClient.SetConnectString(const AHostPort: string);
var
  iPos:   integer;
begin
  iPos := Pos(':', AHostPort);
  if iPos <> 0 then begin
    FHost := copy(AHostPort, 1, iPos - 1);
    FPort := StrToIntDef(copy(AHostPort, iPos + 1, Length(AHostPort)), DEFAULT_DSTREAM_PORT);
  end
  else begin
    FHost := AHostPort;
    FPort := DEFAULT_DSTREAM_PORT;
  end;
end;

procedure TDStreamClient.Shutdown;
begin
  Close;
  FClientSock.Disconnect;
  Terminate;
  // BRY: Should I check to make sure that this isn't the thread folding back
  // on itself?  Should be safe.
  WaitFor;
end;

end.

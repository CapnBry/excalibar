unit DStreamServer;

(****************************************************************************
**
** Copyright (C) 2004 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  Classes, SysUtils, Contnrs, WinSock, ScktComp, DStreamDefs;

const
    { Pipe command types }
  DPIPE_DATA = $00;
  DPIPE_CONNECTED = $01;
  DPIPE_DISCONNECTED = $02;
  DPIPE_LOG = $03;

  MAX_WRITE_BUFFER_SIZE = 64 * 1024 * 1024;

type
  TStringNotify = procedure (Sender: TObject; const s: string) of object;
  
  PChunkedQueueItem = ^TChunkedQueueItem;
  TChunkedQueueItem = packed record
    Next: PChunkedQueueItem;
    ReadPos:  WORD;
    Size:     WORD;  // vv Everything here and after should match dstream_header vv
    CommandID:BYTE;
    Data:     BYTE;  // data follows R-
  end;

  TChunkedQueue = class(TObject)
  private
    FHead:  PChunkedQueueItem;
    FTail:  PChunkedQueueItem;
    FTotalSize: Cardinal;
    procedure Clear;
    procedure Add(AItem: PChunkedQueueItem);
    function GetDataPtr: Pointer;
    function GetDataSize: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(ACommandID: BYTE; const Buffer; ASize: WORD);
    procedure UsedData(ASize: Cardinal);

    property TotalSize: Cardinal read FTotalSize;
      { current chunk }
    property DataPtr: Pointer read GetDataPtr;
    property DataSize: Cardinal read GetDataSize;
  end;

  TDStreamClientHandler = class(TObject)
  private
    FSocket:      TCustomWinSocket;
    FWriteBuffer: TChunkedQueue;
//    FReadCommand: BYTE;
//    FReadExpectedLength: WORD;
    FReadBuffer:  TStream;
    FAuthRequired: boolean;
    FAuthenticated: boolean;
    FSquelchList:   TList;
    function GetRemoteHost: string;
  protected
    procedure SOCKWrite;
    procedure SOCKRead;

    function SendHELO : boolean;
    procedure AddSquelch(AConnectionID: Cardinal);
    procedure ResumeConnection(AConnectionID: Cardinal);
    function ConnectionCTS(AConnectionID: Cardinal) : boolean;
    procedure GenerateRandomHash(ADest: PChar; ASize: integer);
  public
    constructor Create(ASocket: TCustomWinSocket);
    destructor Destroy; override;

    procedure Close;
    function SendCommand(ACommandID: BYTE; const Buff; ASize: WORD) : boolean;

    property AuthRequired: boolean read FAuthRequired write FAuthRequired;
    property Authenticated: boolean read FAuthenticated write FAuthenticated;
    property RemoteHost: string read GetRemoteHost;
  end;

  TDStreamServer = class(TObjectList)
  private
    FServerSock:  TServerSocket;
    FAuthRequired: boolean;
    FDAOCConnections: TList;
    FOnLog: TStringNotify;

    function GetItems(I: integer): TDStreamClientHandler;
    procedure AddDAOCConnection(AConn: PDStream_DAOCConnectionDetails);
    procedure DeleteDAOCConnection(AConnectionID: Cardinal);
    procedure ClearDAOCConnections;
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function SocketDesc(Socket: TCustomWinSocket) : string;

    procedure AddNew(ASocket: TCustomWinSocket);
    procedure DeleteBySocket(ASocket: TCustomWinSocket);
    function IndexOfSocket(ASocket: TCustomWinSocket) : integer;
    function FindSocket(ASocket: TCustomWinSocket) : TDStreamClientHandler;
    function GetPort: integer;
    procedure SetPort(const Value: integer);
  protected
    procedure Log(const s: string);
    procedure SOCKClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SOCKClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SOCKClientWrite(Sender: TObject; Socket: TCustomWinSocket);
    procedure SOCKClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure SOCKClientError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DAOCData(AData: Pointer; ADataLen: integer);
    procedure DAOCConnect(AData: Pointer; ADataLen: integer);
    procedure DAOCDisconnect(AData: Pointer; ADataLen: integer);

    procedure SendCommandToAll(ACommandID: BYTE; const Buff; ASize: WORD);
    procedure SendLOG(ALevel: BYTE; const AMsg: string);

    procedure Open;
    procedure Close;

    property Active: boolean read GetActive write SetActive;
    property AuthRequired: boolean read FAuthRequired write FAuthRequired;
    property Items[I: integer]: TDStreamClientHandler read GetItems; default;
    property Port: integer read GetPort write SetPort;
    
    property OnLog: TStringNotify read FOnLog write FOnLog;
  end;

implementation

{ TChunkedQueue }

procedure TChunkedQueue.Add(AItem: PChunkedQueueItem);
begin
  if not Assigned(FHead) then begin
    FHead := AItem;
    FTail := AItem;
    FTotalSize := AItem^.Size;
  end
  else begin
    FTail^.Next := AItem;
    FTail := AItem;
    inc(FTotalSize, AItem^.Size);
  end;
end;

procedure TChunkedQueue.Clear;
var
  tmpItem:  PChunkedQueueItem;
begin
  while Assigned(FHead) do begin
    tmpItem := FHead;
    FHead := FHead^.Next;

    FreeMem(tmpItem);
  end;

  FHead := nil;
  FTail := nil;
  FTotalSize := 0;
end;

constructor TChunkedQueue.Create;
begin
  inherited;
end;

destructor TChunkedQueue.Destroy;
begin
  Clear;
  inherited;
end;

function TChunkedQueue.GetDataPtr: Pointer;
begin
  if Assigned(FHead) then
    Result := Pointer(Cardinal(@FHead^.Size) + FHead^.ReadPos)
  else
    Result := nil;
end;

function TChunkedQueue.GetDataSize: Cardinal;
begin
  if Assigned(FHead) then
    Result := FHead^.Size - FHead^.ReadPos
  else
    Result := 0;
end;

procedure TChunkedQueue.UsedData(ASize: Cardinal);
var
  tmpItem:  PChunkedQueueItem;
begin
  if Assigned(FHead) then begin
    inc(FHead^.ReadPos, ASize);
    dec(FTotalSize, ASize);

    if FHead^.ReadPos >= FHead^.Size then begin
      tmpItem := FHead;
      FHead := FHead^.Next;
      FreeMem(tmpItem);

      if not Assigned(FHead) then
        FTail := nil;
    end;  { if finished chunk }
  end;  { if have a head }
end;

procedure TChunkedQueue.Write(ACommandID: BYTE; const Buffer; ASize: WORD);
var
  tmpItem:  PChunkedQueueItem;
  iTotalSize: integer;
begin
  iTotalSize := ASize + sizeof(TChunkedQueueItem) - 1;
  GetMem(tmpItem, iTotalSize);

  tmpItem^.Next := nil;
  tmpItem^.ReadPos := 0;
    { size includes header(size + commandid) }
  tmpItem^.Size := ASize + sizeof(FHead^.Size) + sizeof(FHead^.CommandID);
  tmpItem^.CommandID := ACommandID;
  Move(Buffer, tmpItem^.Data, ASize);
  Add(tmpItem);
end;

{ TDStreamClientHandler }

procedure TDStreamClientHandler.AddSquelch(AConnectionID: Cardinal);
begin
  if not ConnectionCTS(AConnectionID) then
    FSquelchList.Add(Pointer(AConnectionID));
end;

procedure TDStreamClientHandler.Close;
begin
  FSocket.Close;
end;

function TDStreamClientHandler.ConnectionCTS(AConnectionID: Cardinal): boolean;
{ Returns true if the connection ID and the wildcard connection ID (0)
  are not in the squelch list }
begin
  Result := (FSquelchList.IndexOf(Pointer(AConnectionID)) = -1) and
    (FSquelchList.IndexOf(Pointer(0)) = -1);
end;

constructor TDStreamClientHandler.Create(ASocket: TCustomWinSocket);
begin
  inherited Create;

  FSocket := ASocket;
  FSocket.Data := Pointer(Self);
  FWriteBuffer := TChunkedQueue.Create;
  FReadBuffer := TMemoryStream.Create;
  FSquelchList := TList.Create;
end;

destructor TDStreamClientHandler.Destroy;
begin
  FSocket := nil;  // we do not own
  FSquelchList.Free;
  FReadBuffer.Free;
  FWriteBuffer.Free;
  inherited;
end;

procedure TDStreamClientHandler.GenerateRandomHash(ADest: PChar;
  ASize: integer);
begin
  while ASize > 0 do begin
    ADest^ := char(random(26) + 97);
    inc(ADest);
    dec(ASize);
  end;
end;

function TDStreamClientHandler.GetRemoteHost: string;
begin
  Result := FSocket.RemoteHost;
end;

procedure TDStreamClientHandler.ResumeConnection(AConnectionID: Cardinal);
begin
  if AConnectionID = 0 then
    FSquelchList.Clear
  else
    FSquelchList.Remove(Pointer(AConnectionID));
end;

function TDStreamClientHandler.SendCommand(ACommandID: BYTE; const Buff; ASize: WORD) : boolean;
begin
  if FWriteBuffer.TotalSize > MAX_WRITE_BUFFER_SIZE then
    Result := false
  else begin
    Result := true;

    if (ACommandID = DPACKET_DAOC_DATA) and not ConnectionCTS(PCardinal(@Buff)^) then
      exit;

    FWriteBuffer.Write(ACommandID, Buff, ASize);
      { check for send }
    SOCKWrite;
  end;
end;

function TDStreamClientHandler.SendHELO : boolean;
var
  OutBuff: array[0..16] of BYTE;
begin
  PInteger(@OutBuff[0])^ := $4d545344;
  PInteger(@OutBuff[4])^ := DSTREAM_PROTOCOL_VER;
  if FAuthRequired then begin
    OutBuff[8] := 8;
    GenerateRandomHash(@OutBuff[9], 8);
  end
  else
    OutBuff[8] := 0;

  Result := SendCommand(DPACKET_HELO, OutBuff, sizeof(OutBuff));
end;

procedure TDStreamClientHandler.SOCKRead;
var
  Buff:   array[0..4095] of BYTE;
  iRead:  integer;
begin
  repeat
    iRead := FSocket.ReceiveBuf(Buff, sizeof(Buff));
  until iRead <= 0;
end;

procedure TDStreamClientHandler.SOCKWrite;
var
  iSent:  integer;
begin
  if FWriteBuffer.TotalSize = 0 then
    exit;

  repeat
    iSent := FSocket.SendBuf(FWriteBuffer.DataPtr^, FWriteBuffer.DataSize);
    if iSent <> -1 then
      FWriteBuffer.UsedData(iSent);
  until (iSent = -1) or (FWriteBuffer.TotalSize = 0);
end;

{ TDStreamServer }

procedure TDStreamServer.AddDAOCConnection(AConn: PDStream_DAOCConnectionDetails);
var
  pTmp:   PDStream_DAOCConnectionDetails;
begin
  New(pTmp);
  pTmp^ := AConn^;
  FDAOCConnections.Add(pTmp);
end;

procedure TDStreamServer.AddNew(ASocket: TCustomWinSocket);
var
  tmpItem:  TDStreamClientHandler;
begin
  tmpItem := TDStreamClientHandler.Create(ASocket);
  tmpItem.AuthRequired := FAuthRequired;
  { LOCK }
  Add(tmpItem);

  tmpItem.SendHELO;
end;

procedure TDStreamServer.ClearDAOCConnections;
var
  I:    integer;
begin
  for I := 0 to FDAOCConnections.Count - 1 do
    Dispose(FDAOCConnections[I]);
  FDAOCConnections.Clear;
end;

constructor TDStreamServer.Create;
begin
  inherited Create(true);
  FDAOCConnections := TList.Create;

  FServerSock := TServerSocket.Create(nil);
  FServerSock.Port := DSTREAM_DEFAULT_PORT;
  FServerSock.OnClientConnect := SOCKClientConnect;
  FServerSock.OnClientDisconnect := SOCKClientDisconnect;
  FServerSock.OnClientWrite := SOCKClientWrite;
  FServerSock.OnClientRead := SOCKClientRead;
  FServerSock.OnClientError := SOCKClientError;
end;

procedure TDStreamServer.DAOCConnect(AData: Pointer; ADataLen: integer);
begin
  if ADataLen <> sizeof(TDStream_DAOCConnectionDetails) then begin
    //Log('Short DAOCConnect read from pipe');
    exit;
  end;

  AddDAOCConnection(PDStream_DAOCConnectionDetails(AData));
  SendCommandToAll(DPACKET_DAOC_CONNECTION_OPENED, AData^, ADataLen);
end;

procedure TDStreamServer.DAOCData(AData: Pointer; ADataLen: integer);
begin
  SendCommandToAll(DPACKET_DAOC_DATA, AData^, ADataLen);
end;

procedure TDStreamServer.DAOCDisconnect(AData: Pointer; ADataLen: integer);
begin
  if ADataLen <> sizeof(TDStream_DAOCConnectionDetails) then begin
    //Log('Short DAOCDisconnect read from pipe');
    exit;
  end;

  DeleteDAOCConnection(PDStream_DAOCConnectionDetails(AData)^.ConnectionID);
  SendCommandToAll(DPACKET_DAOC_CONNECTION_CLOSED, AData^, ADataLen);
end;

procedure TDStreamServer.DeleteBySocket(ASocket: TCustomWinSocket);
var
  iIdx: integer;
begin
  { LOCK }
  iIdx := IndexOfSocket(ASocket);
  if iIdx <> -1 then
    Delete(iIdx);
end;

procedure TDStreamServer.DeleteDAOCConnection(AConnectionID: Cardinal);
var
  I:    integer;
begin
  for I := 0 to FDAOCConnections.Count - 1 do
    if PDStream_DAOCConnectionDetails(FDAOCConnections[I])^.ConnectionID = AConnectionID then begin
      Dispose(FDAOCConnections[I]);
      FDAOCConnections.Delete(I);
      exit;
    end;
end;

destructor TDStreamServer.Destroy;
begin
  ClearDAOCConnections;
  FDAOCConnections.Free;
  FServerSock.Free;
  inherited;
end;

function TDStreamServer.FindSocket(ASocket: TCustomWinSocket): TDStreamClientHandler;
//var
//  iIdx:   integer;
begin
(*
  { LOCK }
  iIdx := IndexOfSocket(ASocket);
  if iIdx <> -1 then
    Result := Items[iIdx]
  else
    Result := nil;
*)
  Result := TDStreamClientHandler(ASocket.Data);
end;

function TDStreamServer.GetActive: boolean;
begin
  Result := FServerSock.Active;
end;

function TDStreamServer.GetItems(I: integer): TDStreamClientHandler;
begin
  Result := TDStreamClientHandler(inherited Items[I]);
end;

function TDStreamServer.IndexOfSocket(ASocket: TCustomWinSocket): integer;
begin
  { LOCK }
  for Result := 0 to Count - 1 do
    if Items[Result].FSocket = ASocket then
      exit;
  Result := -1;
end;

procedure TDStreamServer.Log(const s: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, s);
end;

procedure TDStreamServer.SendCommandToAll(ACommandID: BYTE; const Buff; ASize: WORD);
var
  I:    integer;
begin
  { LOCK }
  I := 0;
  while I < Count do
    if Items[I].SendCommand(ACommandID, Buff, ASize) then
      inc(I)
    else
      Items[I].Close;
end;

procedure TDStreamServer.SendLOG(ALevel: BYTE; const AMsg: string);
var
  s:  string;
begin
  s := char(ALevel) + char(Length(AMsg)) + AMsg;
  SendCommandToAll(DPACKET_LOG, s[1], Length(s));
end;

procedure TDStreamServer.SetActive(const Value: boolean);
begin
  if Value then
    Open
  else
    Close;
end;

function TDStreamServer.SocketDesc(Socket: TCustomWinSocket): string;
begin
  Result := Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort);
end;

procedure TDStreamServer.SOCKClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  AddNew(Socket);
  Log(Format('New client connected %s, total %d', [SocketDesc(Socket), Count]));
end;

procedure TDStreamServer.SOCKClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  DeleteBySocket(Socket);
  Log(Format('Client %s disconnected, total %d', [SocketDesc(Socket), Count]));
end;

procedure TDStreamServer.SOCKClientError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
var
  pClient:  TDStreamClientHandler;
begin
  pClient := FindSocket(Socket);
  if Assigned(pClient) and
    ((ErrorEvent = eeDisconnect) or (ErrorCode = WSAECONNRESET)) then begin
    Log(Format('Socket error %d on %s.  Closing connection', [
      ErrorCode, SocketDesc(Socket)]));
    Socket.Close;
    ErrorCode := 0;
  end
  else
    Log(Format('Unhandled error %d during %d', [ErrorCode, ord(ErrorEvent)]));
end;

procedure TDStreamServer.SOCKClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  pClient:  TDStreamClientHandler;
begin
  pClient := FindSocket(Socket);
  if Assigned(pClient) then
    pClient.SOCKRead;
end;

procedure TDStreamServer.SOCKClientWrite(Sender: TObject; Socket: TCustomWinSocket);
var
  pClient:  TDStreamClientHandler;
begin
  pClient := FindSocket(Socket);
  if Assigned(pClient) then
    pClient.SOCKWrite;
end;

procedure TDStreamServer.Close;
begin
  if Active then begin
    Clear;
    FServerSock.Active := false;
    Log('DStream server closed');
  end;
end;

procedure TDStreamServer.Open;
begin
  if not Active then begin
    FServerSock.Active := true;
    Log('DStream server active on port ' + IntToStr(FServerSock.Port));
  end;
end;

function TDStreamServer.GetPort: integer;
begin
  Result := FServerSock.Port;
end;

procedure TDStreamServer.SetPort(const Value: integer);
begin
  FServerSock.Port := Value;
end;

end.

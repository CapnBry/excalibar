unit PipeThread;

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
  Windows, Messages, Classes, SysUtils;

type
  TPipeDataNotify = procedure (Sender: TObject; AData: Pointer; ADataLen: integer) of object;

  TNamedPipeThread = class(TThread)
  private
    FMsgList:     TThreadList;
    FNotifyWnd:   HWND;
    FPipe:        THANDLE;
    FDataBuff:    Pointer;
    FDataBuffSize: integer;
    FPipeName: string;
    FOnPipeData: TPipeDataNotify;
    FDataBuffLen: integer;
    FPipeOverlap:   TOverlapped;
    FConnected: boolean;
    FOnPipeDisconnected: TNotifyEvent;
    FOnPipeConnected:   TNotifyEvent;
    function GetActive: boolean;
    procedure SetPipeName(const Value: string);
    procedure SetDataBuffSize(const Value: integer);
    procedure ClearMsgList;
  protected
    procedure Execute; override;
    procedure DoOnPipeData;
    procedure DoOnPipeDisconnected;
    procedure DoOnPipeConnected;
    function SetupPipe : boolean;
    procedure CleanupPipe;
    procedure NotifyWndProc(var Msg: TMessage);
  public
    constructor Create(const APipeName: string);
    destructor Destroy; override;

    procedure Shutdown;

    property Active: boolean read GetActive;
    property Connected: boolean read FConnected;
    property DataBuffSize: integer read FDataBuffLen write SetDataBuffSize;
    property PipeName: string read FPipeName write SetPipeName;
    property OnPipeConnected: TNotifyEvent read FOnPipeConnected write FOnPipeConnected;
    property OnPipeDisconnected: TNotifyEvent read FOnPipeDisconnected write FOnPipeDisconnected;
    property OnPipeData: TPipeDataNotify read FOnPipeData write FOnPipeData;
  end;

implementation

const
  WM_DATA_AVAIL = WM_USER + 1;
  
{ TNamedPipeThread }

procedure TNamedPipeThread.CleanupPipe;
begin
  if FPipe <> INVALID_HANDLE_VALUE then begin
    CloseHandle(FPipe);
    CloseHandle(FPipeOverlap.hEvent);
    FPipe := INVALID_HANDLE_VALUE;
    FPipeOverlap.hEvent := INVALID_HANDLE_VALUE;
  end;
end;

procedure TNamedPipeThread.ClearMsgList;
var
  I:    integer;
begin
  with FMsgList.LockList do
  try
    for I := 0 to Count - 1 do
      Dispose(Items[I]);
    Clear;
  finally
    FMsgList.UnlockList;
  end;
end;

constructor TNamedPipeThread.Create(const APipeName: string);
begin
  inherited Create(true);
  FMsgList := TThreadList.Create;
  FPipe := INVALID_HANDLE_VALUE;
  FPipeName := APipeName;
  SetDataBuffSize(4*1024);
  FNotifyWnd := AllocateHWnd(NotifyWndProc); 
end;

destructor TNamedPipeThread.Destroy;
begin
  if FDataBuffSize <> 0 then
    FreeMem(FDataBuff);

  ClearMsgList;
  FreeAndNil(FMsgList);
  DeallocateHWnd(FNotifyWnd);
  
  inherited;
end;

procedure TNamedPipeThread.DoOnPipeConnected;
begin
  if Assigned(FOnPipeConnected) then
    FOnPipeConnected(Self);
end;

procedure TNamedPipeThread.DoOnPipeData;
var
  pMsg:     Pointer;
  dwSize:   Cardinal;
  pData:    Pointer;
begin
  repeat
    with FMsgList.LockList do
      try
        if Count > 0 then begin
          pMsg := First;
          Delete(0);
        end
        else
          pMsg := nil;
      finally
        FMsgList.UnlockList;
      end;

    if Assigned(pMsg) then begin
        { first 4 bytes are size }
      dwSize := PCardinal(pMsg)^;
        { followed by actual data }
      pData := Pointer(Cardinal(pMsg) + sizeof(Cardinal));
      if Assigned(FOnPipeData) then
        FOnPipeData(Self, pData, dwSize);
      FreeMem(pMsg);
    end;  { if msg }
  until not Assigned(pMsg);
end;

procedure TNamedPipeThread.DoOnPipeDisconnected;
begin
  if Assigned(FOnPipeDisconnected) then
    FOnPipeDisconnected(Self);
end;

procedure TNamedPipeThread.Execute;
var
  dwBytes:      Cardinal;
  bReadSuccess: boolean;
  pMsg:         Pointer;
  bDidRead:     boolean;
begin
  if not SetupPipe then
    exit;

  bReadSuccess := false;

  while not Terminated do begin
    if not FConnected then
      ConnectNamedPipe(FPipe, @FPipeOverlap);
      { we might want to check the result of the connect operation.  it should
        return false on success (since we're going overlapped).  GetLastError
        should return ERROR_IO_PENDING.  If it returns ERROR_PIPE_CONNECTED,
        we need to manually set our FPipeOverlap.hEvent because otherwise the
        wait below won't fire.  This probably is a non issue in our case, because
        the server is always going to come up before a client tries to connect to it }

    WaitForSingleObject(FPipeOverlap.hEvent, INFINITE);

    if not Terminated then begin
      if FConnected then begin
        bDidRead := false;
        if bReadSuccess or GetOverlappedResult(FPipe, FPipeOverlap, dwBytes, true) then begin
          repeat
            if dwBytes > 0 then begin
              bDidRead := true;
              GetMem(pMsg, dwBytes + sizeof(Cardinal));
              PCardinal(pMsg)^ := dwBytes;
              Move(FDataBuff^, (PChar(pMsg) + 4)^, dwBytes);
              with FMsgList.LockList do
                Add(pMsg);
              FMsgList.UnlockList;
            end;

            bReadSuccess := ReadFile(FPipe, FDataBuff^, FDataBuffSize, dwBytes, @FPipeOverlap);
          until not bReadSuccess;

          if bDidRead then
            PostMessage(FNotifyWnd, WM_DATA_AVAIL, 0, 0);
        end  { if connected }
        else begin
          FConnected := false;
          DisconnectNamedPipe(FPipe);
          Synchronize(DoOnPipeDisconnected);
        end;
      end

      else begin
        FConnected := GetOverlappedResult(FPipe, FPipeOverlap, dwBytes, true);
        if FConnected then begin
          Synchronize(DoOnPipeConnected);
          bReadSuccess := ReadFile(FPipe, FDataBuff^, FDataBuffSize, dwBytes, @FPipeOverlap);
        end;
      end;  { if not connected }
    end;
  end;  { while !Terminated }

  CleanupPipe;
end;

function TNamedPipeThread.GetActive: boolean;
begin
  Result := FPipe <> INVALID_HANDLE_VALUE;
end;

procedure TNamedPipeThread.NotifyWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DATA_AVAIL then
    DoOnPipeData;
end;

procedure TNamedPipeThread.SetDataBuffSize(const Value: integer);
begin
  if Active then
    raise Exception.Create('Can not change buffer size of active pipe');

  if FDataBuffSize <> 0 then
    ReallocMem(FDataBuff, Value)
  else
    GetMem(FDataBuff, Value);

  FDataBuffSize := Value;
end;

procedure TNamedPipeThread.SetPipeName(const Value: string);
begin
  FPipeName := Value;
end;

function TNamedPipeThread.SetupPipe : boolean;
begin
  FPipe := CreateNamedPipe(PChar('\\.\pipe\' + FPipeName),
    PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE,
    1, 0, 16 * 1024, 0, nil);

  Result := FPipe <> INVALID_HANDLE_VALUE;
  if Result then begin
    FillChar(FPipeOverlap, sizeof(FPipeOverlap), 0);
    FPipeOverlap.hEvent := CreateEvent(nil, true, false, nil);
  end;
end;

procedure TNamedPipeThread.Shutdown;
begin
  Terminate;
  if Active then
    SetEvent(FPipeOverlap.hEvent);
  WaitFor;
end;

end.

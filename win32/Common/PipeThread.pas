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
  Windows, Classes, SysUtils;

type
  TPipeDataNotify = procedure (Sender: TObject; AData: Pointer; ADataLen: integer) of object;

  TNamedPipeThread = class(TThread)
  private
    FPipe:        THANDLE;
    FDataBuff:    Pointer;
    FDataBuffSize: integer;
    FDataBuffUsed: integer;
    FPipeName: string;
    FOnPipeData: TPipeDataNotify;
    FDataBuffLen: integer;
    FPipeOverlap:   TOverlapped;
    FConnected: boolean;
    FOnPipeDisconnected: TNotifyEvent;
    FOnPipeConnected: TNotifyEvent;
    function GetActive: boolean;
    procedure SetPipeName(const Value: string);
    procedure SetDataBuffSize(const Value: integer);
  protected
    procedure Execute; override;
    procedure DoOnPipeData;
    procedure DoOnPipeDisconnected;
    procedure DoOnPipeConnected;
    function SetupPipe : boolean;
    procedure CleanupPipe;
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

constructor TNamedPipeThread.Create(const APipeName: string);
begin
  inherited Create(true);
  FPipe := INVALID_HANDLE_VALUE;
  FPipeName := APipeName;
  SetDataBuffSize(16*1024);
end;

destructor TNamedPipeThread.Destroy;
begin
  if FDataBuffSize <> 0 then
    FreeMem(FDataBuff);
    
  inherited;
end;

procedure TNamedPipeThread.DoOnPipeConnected;
begin
  if Assigned(FOnPipeConnected) then
    FOnPipeConnected(Self);
end;

procedure TNamedPipeThread.DoOnPipeData;
begin
  if Assigned(FOnPipeData) then
    FOnPipeData(Self, FDataBuff, FDataBuffUsed);
  FDataBuffUsed := 0;
end;

procedure TNamedPipeThread.DoOnPipeDisconnected;
begin
  if Assigned(FOnPipeDisconnected) then
    FOnPipeDisconnected(Self);
end;

procedure TNamedPipeThread.Execute;
var
  dwBytes:  Cardinal;
begin
  if not SetupPipe then
    exit;

  while not Terminated do begin
    if not FConnected then
      ConnectNamedPipe(FPipe, @FPipeOverlap);

    WaitForSingleObject(FPipeOverlap.hEvent, INFINITE);

    if not Terminated then begin
      if FConnected then begin
        if GetOverlappedResult(FPipe, FPipeOverlap, dwBytes, true) then begin
          FDataBuffUsed := dwBytes;
          Synchronize(DoOnPipeData);
          ReadFile(FPipe, FDataBuff^, FDataBuffSize, dwBytes, @FPipeOverlap);
        end
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
          ReadFile(FPipe, FDataBuff^, FDataBuffSize, dwBytes, @FPipeOverlap);
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

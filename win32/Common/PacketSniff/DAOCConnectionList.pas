unit DAOCConnectionList;

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
  SysUtils, Classes, GameNetPackets, DAOCObjs, DAOCPlayerAttributes,
  DAOCConnection, DAOCInventory, ChatParse;

type
  TDAOCConnectionNotify = procedure(Sender: TObject; AConn: TDAOCConnection) of object;

  TDAOCConnectionList = class(TObject)
  private
    FOnDeleteConnection: TDAOCConnectionNotify;
    FOnNewConnection: TDAOCConnectionNotify;
    function GetCount: integer;
    function GetItems(I: integer): TDAOCConnection;
    function GetActiveCount: integer;
  protected
    FList:    TList;
    function NewDAOCConnectionNeeded : TDAOCConnection; virtual;
    procedure DoSetupConnectionEventsAndProperties(AConn: TDAOCConnection); virtual;
    procedure DoOnDeleteConnection(AConn: TDAOCConnection); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function IndexOf(AConn: TDAOCConnection) : integer;
    function IndexOfConnection(ASource: TObject; AConnectionID: Cardinal) : integer;
    function FindConnection(ASource: TObject; AConnectionID: Cardinal) : TDAOCConnection;

    function NewDAOCConnection(ASource: TObject; AConnectionID: Cardinal;
      AServerIP: Cardinal; AClientIP: Cardinal) : TDAOCConnection;
    procedure CloseDAOCConnection(ASource: TObject; AConnectionID: Cardinal);
    procedure ProcessDAOCPacket(ASource: TObject; APacket: TGameNetPacket);

      { properties }
    property ActiveCount: integer read GetActiveCount;
    property Count: integer read GetCount;
    property Items[I: integer]: TDAOCConnection read GetItems; default;
      { events }
    property OnNewConnection: TDAOCConnectionNotify read FOnNewConnection write FOnNewConnection;
    property OnDeleteConnection: TDAOCConnectionNotify read FOnDeleteConnection write FOnDeleteConnection; 
  end;

implementation

type
  TReintroduceDAOCConnection = class(TDAOCConnection);

{ TDAOCConnectionList }

procedure TDAOCConnectionList.Clear;
var
  pTmp: TReintroduceDAOCConnection;
begin
  while Count > 0 do begin
    pTmp := TReintroduceDAOCConnection(Items[0]);
    pTmp.DoOnDisconnect;
    FList.Delete(0);

    DoOnDeleteConnection(pTmp);
    pTmp.Free;
  end;
end;

procedure TDAOCConnectionList.CloseDAOCConnection(ASource: TObject;
  AConnectionID: Cardinal);
var
  iIdx:   integer;
  pTmp:   TReintroduceDAOCConnection;
begin
  iIdx := IndexOfConnection(ASource, AConnectionID);
  if iIdx <> -1 then begin
    pTmp := TReintroduceDAOCConnection(Items[iIdx]);
    pTmp.DoOnDisconnect;
    FList.Delete(iIdx);

    DoOnDeleteConnection(pTmp);
    pTmp.Free;
  end;
end;

constructor TDAOCConnectionList.Create;
begin
  FList := TList.Create;
end;

destructor TDAOCConnectionList.Destroy;
begin
  Clear;
  FList.Free;

  inherited;
end;

procedure TDAOCConnectionList.DoOnDeleteConnection(AConn: TDAOCConnection);
begin
  if Assigned(FOnDeleteConnection) then
    FOnDeleteConnection(Self, AConn);
end;

procedure TDAOCConnectionList.DoSetupConnectionEventsAndProperties(
  AConn: TDAOCConnection);
begin
  if Assigned(FOnNewConnection) then
    FOnNewConnection(Self, AConn);
end;

function TDAOCConnectionList.FindConnection(ASource: TObject;
  AConnectionID: Cardinal): TDAOCConnection;
var
  iIdx:   integer;
begin
  iIdx := IndexOfConnection(ASource, AConnectionID);
  if iIdx <> -1 then
    Result := Items[iIdx]
  else
    Result := nil;
end;

function TDAOCConnectionList.GetActiveCount: integer;
var
  I:  integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Active then
      inc(Result);
end;

function TDAOCConnectionList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TDAOCConnectionList.GetItems(I: integer): TDAOCConnection;
begin
  Result := TDAOCConnection(FList[I]);
end;

function TDAOCConnectionList.IndexOf(AConn: TDAOCConnection): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = AConn then
      exit;
      
  Result := -1;
end;

function TDAOCConnectionList.IndexOfConnection(ASource: TObject;
  AConnectionID: Cardinal): integer;
begin
  for Result := 0 to Count - 1 do
    with Items[Result] do
      if (Source = ASource) and (ConnectionID = AConnectionID) then
        exit;

  Result := -1;
end;

function TDAOCConnectionList.NewDAOCConnection(ASource: TObject;
  AConnectionID, AServerIP, AClientIP: Cardinal) : TDAOCConnection;
begin
    { make sure this connection is not a fake reconnect-- reuse
      an existing connection if we have a match }
  Result := FindConnection(ASource, AConnectionID);
  if not Assigned(Result) then begin
    Result := NewDAOCConnectionNeeded;
    FList.Add(Result);
    DoSetupConnectionEventsAndProperties(Result);
  end
  else
    Result.Clear;

  Result.Source := ASource;
  Result.ConnectionID := AConnectionID;
  Result.ServerAddr := AServerIP;
  Result.ClientAddr := AClientIP;
  Result.InitPacketHandlers;

  TReintroduceDAOCConnection(Result).DoOnConnect;
end;

function TDAOCConnectionList.NewDAOCConnectionNeeded: TDAOCConnection;
{ if this function is overridden in a decendant list, make sure you don't call
  the inherited, it it will leak a connection }
begin
  Result := TDAOCConnection.Create;
end;

procedure TDAOCConnectionList.ProcessDAOCPacket(ASource: TObject;
  APacket: TGameNetPacket);
var
  pConn:    TDAOCConnection;
begin
  pConn := FindConnection(ASource, APacket.ConnectionID);
  if not Assigned(pConn) then
    pConn := NewDAOCConnection(ASource, APacket.ConnectionID, 0, 0);
  pConn.ProcessDAOCPacket(APacket);
end;

end.

unit DAOCControlList;

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
  Classes, DAOCConnection, DAOCConnectionList, DAOCControl, MapNavigator;

type
  TDAOCControlList = class(TDAOCConnectionList)
  private
    FOnArriveAtGotoDest: TMapNodeNotify;
    FOnSelectNPCFailed: TNotifyEvent;
    FOnAttemptNPCRightClickSuccess: TNotifyEvent;
    FOnStopAllActions: TNotifyEvent;
    FOnPathChanged: TNotifyEvent;
    FOnAttemptNPCRightClickFailed: TNotifyEvent;
    FOnSelectNPCSuccess: TNotifyEvent;

    procedure CONNOnArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
    procedure CONNOnPathChanged(Sender: TObject);
    procedure CONNOnStopAllActions(Sender: TObject);
    procedure CONNOnSelectNPCFailed(Sender: TObject);
    procedure CONNOnSelectNPCSuccess(Sender: TObject);
    procedure CONNOnAttemptNPCRightClickFailed(Sender: TObject);
    procedure CONNOnAttemptNPCRightClickSuccess(Sender: TObject);
  protected
    procedure LinkEventHanders(AConn: TDAOCConnection); override;
    function NewDAOCConnectionNeeded : TDAOCConnection; override;
  public
    property OnArriveAtGotoDest: TMapNodeNotify read FOnArriveAtGotoDest write FOnArriveAtGotoDest;
    property OnPathChanged: TNotifyEvent read FOnPathChanged write FOnPathChanged;
    property OnStopAllActions: TNotifyEvent read FOnStopAllActions write FOnStopAllActions;
    property OnSelectNPCFailed: TNotifyEvent read FOnSelectNPCFailed write FOnSelectNPCFailed;
    property OnSelectNPCSuccess: TNotifyEvent read FOnSelectNPCSuccess write FOnSelectNPCSuccess;
    property OnAttemptNPCRightClickFailed: TNotifyEvent read FOnAttemptNPCRightClickFailed write FOnAttemptNPCRightClickFailed;
    property OnAttemptNPCRightClickSuccess: TNotifyEvent read FOnAttemptNPCRightClickSuccess write FOnAttemptNPCRightClickSuccess;
  end;
  
implementation

{ TDAOCControlList }

procedure TDAOCControlList.CONNOnArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
begin
  if Assigned(FOnArriveAtGotoDest) then
    FOnArriveAtGotoDest(Sender, ANode);
end;

procedure TDAOCControlList.CONNOnAttemptNPCRightClickFailed(Sender: TObject);
begin
  if Assigned(FOnAttemptNPCRightClickFailed) then
    FOnAttemptNPCRightClickFailed(Sender);
end;

procedure TDAOCControlList.CONNOnAttemptNPCRightClickSuccess(Sender: TObject);
begin
  if Assigned(FOnAttemptNPCRightClickSuccess) then
    FOnAttemptNPCRightClickSuccess(Sender);
end;

procedure TDAOCControlList.CONNOnPathChanged(Sender: TObject);
begin
  if Assigned(FOnPathChanged) then
    FOnPathChanged(Sender);
end;

procedure TDAOCControlList.CONNOnSelectNPCFailed(Sender: TObject);
begin
  if Assigned(FOnSelectNPCFailed) then
    FOnSelectNPCFailed(Sender);
end;

procedure TDAOCControlList.CONNOnSelectNPCSuccess(Sender: TObject);
begin
  if Assigned(FOnSelectNPCSuccess) then
    FOnSelectNPCSuccess(Sender);
end;

procedure TDAOCControlList.CONNOnStopAllActions(Sender: TObject);
begin
  if Assigned(FOnStopAllActions) then
    FOnStopAllActions(Sender);
end;

procedure TDAOCControlList.LinkEventHanders(AConn: TDAOCConnection);
begin
  inherited;

  TDAOCControl(AConn).OnArriveAtGotoDest := CONNOnArriveAtGotoDest;
  TDAOCControl(AConn).OnPathChanged := CONNOnPathChanged;
  TDAOCControl(AConn).OnStopAllActions := CONNOnStopAllActions;
  TDAOCControl(AConn).OnSelectNPCFailed := CONNOnSelectNPCFailed;
  TDAOCControl(AConn).OnSelectNPCSuccess := CONNOnSelectNPCSuccess;
  TDAOCControl(AConn).OnAttemptNPCRightClickFailed := CONNOnAttemptNPCRightClickFailed;
  TDAOCControl(AConn).OnAttemptNPCRightClickSuccess := CONNOnAttemptNPCRightClickSuccess;
end;

function TDAOCControlList.NewDAOCConnectionNeeded: TDAOCConnection;
begin
  Result := TDAOCControl.Create;
end;

end.

unit OLEDaocControl;

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
  ComObj, ActiveX, AxCtrls, DAOCControl, DAOCSkilla_TLB;

{$WARN SYMBOL_PLATFORM OFF}

type
  TOLEDaocControlWrapper = class(TAutoObject, IConnectionPointContainer,
    IDAOCControl)
  private
    { note: FEvents maintains a *single* event sink. For access to more
      than one event sink, use FConnectionPoint.SinkList, and iterate
      through the list of sinks. }
    FEvents:    IDAOCControlEvents;
    FConnectionPoints:  TConnectionPoints;
    FConnectionPoint:   TConnectionPoint;
    FDControl:  TDAOCControl;
    function GetClassTypeInfo: ITypeInfo;
  protected
    procedure QuitDAOC; safecall;
    procedure SetQuickbarPage(dwPage: Integer); safecall;
    procedure Jump; safecall;
    procedure SendKeys(const bsKeys: WideString); safecall;
    procedure LeftClick(X: Integer; Y: Integer); safecall;
    procedure RightClick(X: Integer; Y: Integer); safecall;
    procedure StopAllActions; safecall;
      { IConnectionPoints }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
  public
    constructor Create(ADAOCControl: TDAOCControl);

    procedure Initialize; override;

    property DAOCControl: TDAOCControl read FDControl write FDControl;
    property ClassTypeInfo: ITypeInfo read GetClassTypeInfo;
  end;

implementation

uses
  ComServ;

{ TOLEDaocControlWrapper }

constructor TOLEDaocControlWrapper.Create(ADAOCControl: TDAOCControl);
begin
  inherited Create;
  FDControl := ADAOCControl;
end;

procedure TOLEDaocControlWrapper.EventSinkChanged(
  const EventSink: IInterface);
begin
  FEvents := EventSink as IDAOCControlEvents;
end;

procedure TOLEDaocControlWrapper.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;
end;

procedure TOLEDaocControlWrapper.Jump;
begin
  if Assigned(FDControl) then
    FDControl.Jump;
end;

procedure TOLEDaocControlWrapper.LeftClick(X, Y: Integer);
begin
  if Assigned(FDControl) then
    FDControl.LeftClick(X, Y);
end;

function TOLEDaocControlWrapper.GetClassTypeInfo: ITypeInfo;
begin
  Result := AutoFactory.ClassInfo;
end;

procedure TOLEDaocControlWrapper.QuitDAOC;
begin
  if Assigned(FDControl) then
    FDControl.QuitDAOC;
end;

procedure TOLEDaocControlWrapper.RightClick(X, Y: Integer);
begin
  if Assigned(FDControl) then
    FDControl.RightClick(X, Y);
end;

procedure TOLEDaocControlWrapper.SendKeys(const bsKeys: WideString);
begin
  if Assigned(FDControl) then
    FDControl.DoSendKeys(bsKeys);
end;

procedure TOLEDaocControlWrapper.SetQuickbarPage(dwPage: Integer);
begin
  if Assigned(FDControl) then
    FDControl.SetQuickbarPage(dwPage);
end;

procedure TOLEDaocControlWrapper.StopAllActions;
begin
  if Assigned(FDControl) then
    FDControl.StopAllActions;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TOLEDaocControlWrapper, CLASS_CDAOCControl,
    ciInternal, tmFree);

end.

unit NamedPacketHandler;

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
  Classes, SysUtils, Contnrs, INIFiles, GenericNetPackets;

type
  TNamedPacketHandler = class(TObject)
  private
    FCommandID: integer;
    FName: string;
    FHandler: TGNPacketHandler;
  public
    property CommandID: integer read FCommandID write FCommandID;
    property Name: string read FName write FName;
    property Handler: TGNPacketHandler read FHandler write FHandler;
  end;

  TNamedPacketHandlerList = class(TObjectList)
  private
    function GetItems(I: integer): TNamedPacketHandler;
  public
    function HandlerByID(ACommandID: integer) : TNamedPacketHandler;
    procedure LoadFromFile(const AFName, ASection: string);

    property Items[I: integer]: TNamedPacketHandler read GetItems; default;
  end;

implementation

{ TNamedPacketHandlerList }

function TNamedPacketHandlerList.GetItems(I: integer): TNamedPacketHandler;
begin
  Result := TNamedPacketHandler(inherited Items[I]);
end;

function TNamedPacketHandlerList.HandlerByID(ACommandID: integer): TNamedPacketHandler;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].CommandID = ACommandID then begin
      Result := Items[I];
      exit;
    end;
    
  Result := nil;
end;

procedure TNamedPacketHandlerList.LoadFromFile(const AFName, ASection: string);
var
  slSection:  TStringList;
  pTmpItem:   TNamedPacketHandler;
  I:      integer;
  sCommandID: string;
begin
  Clear;

  slSection := TStringList.Create;
  with TINIFile.Create(AFName) do begin
    ReadSection(ASection, slSection);

    for I := 0 to slSection.Count - 1 do begin
      sCommandID := slSection[I];
      pTmpItem := TNamedPacketHandler.Create;
      pTmpItem.CommandID := StrToIntDef('$' + sCommandID, -1);
      pTmpItem.Name := ReadString(ASection, sCommandID, '');
      pTmpItem.Handler := nil;
      Add(pTmpItem);
    end;

    Free;
  end;
  slSection.Free;
end;

end.

unit DAOCAccountCharInfo;

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
  SysUtils, Contnrs, DAOCRegion, GameNetPackets;
  
type
  TDAOCAccountCharInfo = class(TObject)
  private
    FName:    string;
    FRegionID:integer;
    FLevel:   integer;
    FRealm:   TDAOCRealm;
  public
    function AsString : string;

    property Name: string read FName;
    property RegionID: integer read FRegionID write FRegionID;
    property Level: integer read FLevel write FLevel;
    property Realm: TDAOCRealm read FRealm write FRealm;
  end;

  TDAOCAccountCharInfoList = class(TObjectList)
  private
    FAccountName:   string;
    FServerName: string;
    FAccountPassword: string;
    FServerProtocol: byte;
    function GetItems(iIndex: integer): TDAOCAccountCharInfo;
  public
    constructor Create; 
    
    procedure Clear; override;
    function FindOrAddChar(const AName: string) : TDAOCAccountCharInfo;
    procedure AppendListFromPacketData(APacket: TGameNetPacket);
    procedure ResetFromPacketData(APacket: TGameNetPacket);

    property Items[iIndex: integer]: TDAOCAccountCharInfo read GetItems; default;
    property AccountName: string read FAccountName write FAccountName;
    property AccountPassword: string read FAccountPassword write FAccountPassword;
    property ServerName: string read FServerName write FServerName;
    property ServerProtocol: byte read FServerProtocol write FServerProtocol;
  end;

implementation

{ TDAOCAccountCharInfoList }

procedure TDAOCAccountCharInfoList.Clear;
begin
  inherited;
  FServerProtocol := $01;
  FAccountName := '';
  FAccountPassword := '';
  FServerName := '';    
end;

function TDAOCAccountCharInfoList.FindOrAddChar(const AName: string): TDAOCAccountCharInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].Name, AName) then begin
      Result := Items[I];
      exit;
    end;

  Result := TDAOCAccountCharInfo.Create;
  Result.FName := AName;
  Add(Result);
end;

function TDAOCAccountCharInfoList.GetItems(iIndex: integer): TDAOCAccountCharInfo;
begin
  Result := TDAOCAccountCharInfo(inherited Items[iIndex]);
end;

procedure TDAOCAccountCharInfoList.AppendListFromPacketData(APacket: TGameNetPacket);
var
  sName:  string;
  iCharsLeft: integer;
  iRegion:  integer;
  iRealm:   integer;
  iLevel:   integer;
  pAcctChar: TDAOCAccountCharInfo;
begin
  Clear;
  AccountName := APacket.getNullTermString(24);

    { parse up to 8 characters }
  iCharsLeft := 8;

  while (iCharsLeft > 0) and not APacket.EOF do begin
    sName := APacket.getNullTermString(48);
    APacket.Seek(72);
    iLevel := APacket.getByte;
    APacket.Seek(1);
    iRealm := APacket.getByte;
    APacket.Seek(3);
    iRegion := APacket.getByte;
    APacket.Seek(57);
    if (iRegion <> 0) and (sName <> '') then begin
      pAcctChar := FindOrAddChar(sName);
      pAcctChar.RegionID := iRegion;
      pAcctChar.Realm := TDAOCRealm(iRealm);
      pAcctChar.Level := iLevel;
    end;
    dec(iCharsLeft);
  end;    { for chars }
end;

procedure TDAOCAccountCharInfoList.ResetFromPacketData(APacket: TGameNetPacket);
begin
  Clear;
  FServerProtocol := APacket.GetByte;
  APacket.seek(3);
  FAccountName := APacket.getPascalString;
  FServerName := APacket.getPascalString;
end;

constructor TDAOCAccountCharInfoList.Create;
begin
  inherited Create(true);
  Clear;
end;

{ TDAOCAccountCharInfo }

function TDAOCAccountCharInfo.AsString: string;
begin
  Result := Format('%s level %d %s region %d', [
    FName, FLevel,  RealmToStr(FRealm), FRegionID]);
end;

end.

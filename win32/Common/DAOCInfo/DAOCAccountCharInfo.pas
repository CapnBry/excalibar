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
  SysUtils, Contnrs, DAOCRegion;
  
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
    function GetItems(iIndex: integer): TDAOCAccountCharInfo;
  public
    procedure Clear; override;
    function FindOrAddChar(const AName: string) : TDAOCAccountCharInfo;

    property Items[iIndex: integer]: TDAOCAccountCharInfo read GetItems; default;
    property AccountName: string read FAccountName write FAccountName;
    property AccountPassword: string read FAccountPassword write FAccountPassword;
    property ServerName: string read FServerName write FServerName;
  end;

implementation

{ TDAOCAccountCharInfoList }

procedure TDAOCAccountCharInfoList.Clear;
begin
  inherited;
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

{ TDAOCAccountCharInfo }

function TDAOCAccountCharInfo.AsString: string;
begin
  Result := Format('%s level %d %s region %d', [
    FName, FLevel,  RealmToStr(FRealm), FRegionID]);
end;

end.

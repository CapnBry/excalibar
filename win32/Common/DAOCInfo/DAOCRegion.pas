unit DAOCRegion;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

(****
  Note that a region is a large area and a zone is a subarea of that.
  For example the land of midgard is region 100, then that region is
  divided into zones such as Vale of Mularn (Zone 100), Svealand East
  (Zone 101) etc
****)

interface

uses
  Types, Classes, SysUtils, Contnrs, StringParseHlprs, Intersections, StreamINI,
  MPKFile;

type
  TDAOCRealm = (drNeutral, drAlbion, drMidgard, drHibernia);
  TDAOCZoneType = (dztUnknown, dztOverworld, dztCity, dztDungeon, dztHousing);
  TDAOCZoneInfoList = class;

	TDAOCZoneInfo = class(TObject)
  private
    FZoneType: TDAOCZoneType;
    FRotate:  integer;
    FZoneNum: integer;
    FRegion:  integer;
    FName:    string;
    FMapName: string;
    FBaseLoc: TPoint;
    FMaxLoc:  TPoint;
    FAdjacentZones:   TDAOCZoneInfoList;
    FProxyZone:       integer;

    procedure MapNameToName;
    procedure NameToMapName;
  public
    constructor Create;
    destructor Destroy; override;

  	procedure LoadFromString(const AZoneInfo: string);
    function AsString: string;

    function ContainsPoint(ARegion, AX, AY: integer) : boolean;
    function ZoneConvertHead(AHead: integer) : integer;
    function ZoneToWorldX(AX: Cardinal) : Cardinal;
    function ZoneToWorldY(AY: Cardinal) : Cardinal;
    function WorldToZoneX(AX: Cardinal) : Cardinal;
    function WorldToZoneY(AY: Cardinal) : Cardinal;

    property AdjacentZones: TDAOCZoneInfoList read FAdjacentZones;
  	property Region: integer read FRegion;
    property BaseLoc: TPoint read FBaseLoc;
    property MaxLoc: TPoint read FMaxLoc;
    property ZoneType: TDAOCZoneType read FZoneType;
    property ZoneNum: integer read FZoneNum;
    property Rotate: integer read FRotate;
    property Name: string read FName;
    property MapName: string read FMapName;
    property ProxyZone: integer read FProxyZone;
  end;

  TDAOCZoneInfoList = class(TObjectList)
  private
    function GetItems(I: Integer): TDAOCZoneInfo;
    procedure UpdateAdjacentZones;
  public
    procedure LoadFromFile(const AFName: string);
    procedure LoadFromMPKFile(const AMPKFName: string);

    function FindZoneForPoint(ARegion, AX, AY: integer) : TDAOCZoneInfo;
    function FindZone(AZoneNum: integer) : TDAOCZoneInfo;

    property Items[I: Integer]: TDAOCZoneInfo read GetItems; default;
  end;

function RealmToStr(ARealm: TDAOCRealm) : string;

implementation

function MPKZoneTypeToDAOCZoneType(AVal: integer) : TDAOCZoneType;
begin
  case AVal of
    0:  Result := dztOverworld;
    1:  Result := dztCity;
    2:  Result := dztDungeon;
    3:  Result := dztHousing;
    else
      Result := dztUnknown;
  end;
end;

function RealmToStr(ARealm: TDAOCRealm) : string;
begin
  case ARealm of
    drNeutral:  Result := 'Neutral';
    drAlbion:  Result := 'Albion';
    drMidgard:  Result := 'Midgard';
    drHibernia:  Result := 'Hibernia';
  end;    { case }
end;

{ TDAOCZoneInfo }

function TDAOCZoneInfo.AsString: string;
begin
  Result := Format('Zone Info for zone %d region %d [%s]:'#13#10 +
    '  BaseLoc (x,y):  %d,%d'#13#10 +
    '  MaxLoc (x,y):  %d,%d'#13#10 +
    '  Zone type: %d'#13#10 +
    '  Rotate: %d'#13#10,
    [FZoneNum, FRegion, FMapName, FBaseLoc.X, FBaseLoc.Y,
    MaxLoc.X, MaxLoc.Y, ord(FZoneType), FRotate]); 
end;

function TDAOCZoneInfo.ContainsPoint(ARegion, AX, AY: integer): boolean;
begin
  Result := (FRegion = ARegion) and
    (AX > FBaseLoc.x) and (AX < FMaxLoc.x) and
    (AY > FBaseLoc.y) and (AY < FMaxLoc.y);
end;

procedure TDAOCZoneInfo.MapNameToName;
var
  I:  integer;
begin
  FName := ChangeFileExt(FMapName, '');
  for I := 1 to Length(FName) do
    if not (FName[I] in ['0'..'9', 'A'..'Z', 'a'..'z']) then
      FName[I] := ' ';
end;

procedure TDAOCZoneInfo.LoadFromString(const AZoneInfo: string);
var
  iStartPos:  integer;
begin
  iStartPos := 1;
  FRegion := StrToIntDef(ParseWord(AZoneInfo, iStartPos), -1);
  FBaseLoc.X := StrToIntDef('$' + ParseWord(AZoneInfo, iStartPos), -1);
  FBaseLoc.Y := StrToIntDef('$' + ParseWord(AZoneInfo, iStartPos), -1);
  FMaxLoc.X := StrToIntDef('$' + ParseWord(AZoneInfo, iStartPos), -1);
  FMaxLoc.Y := StrToIntDef('$' + ParseWord(AZoneInfo, iStartPos), -1);
  case StrToIntDef(ParseWord(AZoneInfo, iStartPos), 0) of
    0:  FZoneType := dztOverworld;
    1:  FZoneType := dztCity;
    2:  FZoneType := dztDungeon;
    3:  FZoneType := dztHousing;
    else
      FZoneType := dztUnknown;
  end;
  FMapName := ParseWordEx(AZoneInfo, iStartPos, pcsFILENAME_CHARS);
  FZoneNum := StrToIntDef(ParseWord(AZoneInfo, iStartPos), -1);
  FRotate := StrToIntDef(ParseWord(AZoneInfo, iStartPos), -1);
  
  MapNameToName;
end;

function TDAOCZoneInfo.ZoneConvertHead(AHead: integer): integer;
begin
  Result := AHead + FRotate + 180;
  if Result > 360 then
    dec(Result, 360);
end;

function TDAOCZoneInfo.ZoneToWorldX(AX: Cardinal) : Cardinal;
begin
  Result := AX + Cardinal(FBaseLoc.X);
end;

function TDAOCZoneInfo.ZoneToWorldY(AY: Cardinal) : Cardinal;
begin
  Result := AY + Cardinal(FBaseLoc.Y);
end;

function TDAOCZoneInfo.WorldToZoneX(AX: Cardinal) : Cardinal;
begin
  Result := AX - Cardinal(FBaseLoc.X);
end;

function TDAOCZoneInfo.WorldToZoneY(AY: Cardinal) : Cardinal;
begin
  Result := AY - Cardinal(FBaseLoc.Y);
end;

constructor TDAOCZoneInfo.Create;
begin
  inherited;
  FAdjacentZones := TDAOCZoneInfoList.Create(false);
end;

destructor TDAOCZoneInfo.Destroy;
begin
  FAdjacentZones.Free;
  inherited;
end;

procedure TDAOCZoneInfo.NameToMapName;
begin
  if FProxyZone <> 0 then
    FMapName := Format('zone%3.3d.map', [FProxyZone])
  else
    FMapName := Format('zone%3.3d.map', [FZoneNum]);
end;

{ TDAOCZoneInfoList }

function TDAOCZoneInfoList.FindZone(AZoneNum: integer): TDAOCZoneInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ZoneNum = AZoneNum then begin
      Result := Items[I];
      exit;
    end;
  Result := nil;
end;

function TDAOCZoneInfoList.FindZoneForPoint(ARegion, AX, AY: integer): TDAOCZoneInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ContainsPoint(ARegion, AX, AY) then begin
      Result := Items[I];
      exit;
    end;
  Result := nil;
end;

function TDAOCZoneInfoList.GetItems(I: Integer): TDAOCZoneInfo;
begin
  Result := TDAOCZoneInfo(inherited Items[I]);
end;

procedure TDAOCZoneInfoList.LoadFromFile(const AFName: string);
var
  F:    TextFile;
  sLine:  string;
  pZI:  TDAOCZoneInfo;
begin
  Clear;
  if not FileExists(AFName) then
    exit;

  AssignFile(F, AFName);
  Reset(F);
  while not EOF(F) do begin
    ReadLn(f, sLine);
    pZI := TDAOCZoneInfo.Create;
    pZI.LoadFromString(sLine);
    Add(pZI);
  end;    { while }
  CloseFile(F);

  UpdateAdjacentZones;
end;

procedure TDAOCZoneInfoList.LoadFromMPKFile(const AMPKFName: string);
var
  pMPK:   TMPKFile;
  pStrm:  TStream;
  pINI:   TStreamINIFile;
  I:      integer;
  pZI:    TDAOCZoneInfo;
begin
  pMPK := TMPKFile.Create(AMPKFName);
  pStrm := pMPK.ExtractStream('zones.dat');
  pMPK.Free;

  pINI := TStreamINIFile.Create(pStrm);
  pStrm.Free;

  for I := 0 to pINI.SectionCount - 1 do
    with pINI.Sections[I] do begin
      if StrLIComp('zone', PChar(Name), 4) = 0 then begin
        if not ReadBool('enabled', false) then
          continue;

        pZI := TDAOCZoneInfo.Create;
        Self.Add(pZI);

        pZI.FRegion := ReadInteger('region', -1);
        pZI.FBaseLoc := Point(ReadInteger('region_offset_x', 0) * 8192,
          ReadInteger('region_offset_y', 0) * 8192);
        pZI.FMaxLoc := Point(pZI.BaseLoc.X + ReadInteger('width', 8) * 8192,
          pZI.BaseLoc.Y + ReadInteger('width', 8) * 8192);
        pZI.FZoneType := MPKZoneTypeToDAOCZoneType(ReadInteger('type', 0));
        pZI.FZoneNum := StrToInt(copy(Name, 5, Length(Name)));
        pZI.FProxyZone := ReadInteger('proxy_zone', 0);
        case pZI.ZoneNum of
          26:       pZI.FRotate := 90;
          120,209:  pZI.FRotate := 180;
          else      pZI.FRotate := 0;
        end;
        pZI.FName := ReadString('name', 'unk' + IntToStr(pZI.ZoneNum));
        pZI.NameToMapName;
      end;  { if is a zone }
    end;  { for I to section count / with }

  pINI.Free;
  UpdateAdjacentZones;
end;

procedure TDAOCZoneInfoList.UpdateAdjacentZones;
var
  pZone:    TDAOCZoneInfo;
  I:    integer;
  J:    integer;
begin
  for I := 0 to Count - 1 do begin
    pZone := Items[I];
    pZone.AdjacentZones.Clear;

    for J := 0 to Count - 1 do begin
      if I = J then
        continue;

      if (pZone.Region = Items[J].Region) and
        RectsIntersect(
        Rect(pZone.BaseLoc.X, pZone.BaseLoc.Y, pZone.MaxLoc.X, pZone.MaxLoc.Y),
        Rect(Items[J].BaseLoc.X, Items[J].BaseLoc.Y, Items[J].MaxLoc.X, Items[J].MaxLoc.Y)
        ) then
          { we could, in theory, cross-add the zones to each other's adjacency
            list, but I don't because it makes the loop logic cleaner }
        pZone.AdjacentZones.Add(Items[J]);
    end; { for J }
  end;  { for I }
end;

end.


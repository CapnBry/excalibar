unit DAOCRegion;

(****
  Note that a region is a large area and a zone is a subarea of that.
  For example the land of midgard is region 100, then that region is
  divided into zones such as Vale of Mularn (Zone 100), Svealand East
  (Zone 101) etc
****)

interface

uses
  Types, Windows, SysUtils, Contnrs, StringParseHlprs, Intersections;

type
  TDAOCRealm = (drNeutral, drAlbion, drMidgard, drHibernia);
  TDAOCZoneType = (dztUnknown, dztOverworld, dztCity, dztDungeon, dztHousing);
  TDAOCZoneInfoList = class;

	TDAOCZoneInfo = class(TObject)
  private
    FZoneType: TDAOCZoneType;
    FRotate: integer;
    FZoneNum: integer;
    FRegion: integer;
    FMapName: string;
    FBaseLoc: TPoint;
    FMaxLoc: TPoint;
    FAdjacentZones:   TDAOCZoneInfoList;
    function GetName: string;
  public
    constructor Create;
    destructor Destroy; override;
    
  	procedure LoadFromString(const AZoneInfo: string);
    function AsString: string;

    function ContainsPoint(ARegion, AX, AY: integer) : boolean;
    function ZoneConvertHead(AHead: integer) : integer;
    function ZoneToWorldX(AX: DWORD) : DWORD;
    function ZoneToWorldY(AY: DWORD) : DWORD;
    function WorldToZoneX(AX: DWORD) : DWORD;
    function WorldToZoneY(AY: DWORD) : DWORD;

    property AdjacentZones: TDAOCZoneInfoList read FAdjacentZones;
  	property Region: integer read FRegion;
    property BaseLoc: TPoint read FBaseLoc;
    property MaxLoc: TPoint read FMaxLoc;
    property ZoneType: TDAOCZoneType read FZoneType;
    property ZoneNum: integer read FZoneNum;
    property Rotate: integer read FRotate;
    property Name: string read GetName;
    property MapName: string read FMapName;
  end;

  TDAOCZoneInfoList = class(TObjectList)
  private
    function GetItems(I: Integer): TDAOCZoneInfo;
    procedure UpdateAdjacentZones;
  public
    procedure LoadFromFile(const AFName: string);
    function FindZoneForPoint(ARegion, AX, AY: integer) : TDAOCZoneInfo;
    function FindZone(AZoneNum: integer) : TDAOCZoneInfo;
    
    property Items[I: Integer]: TDAOCZoneInfo read GetItems; default;
  end;

function RealmToStr(ARealm: TDAOCRealm) : string;

implementation

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

function TDAOCZoneInfo.GetName: string;
begin
  Result := ChangeFileExt(FMapName, '');
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
end;

function TDAOCZoneInfo.ZoneConvertHead(AHead: integer): integer;
begin
  Result := AHead + FRotate + 180;
  if Result > 360 then
    dec(Result, 360);
end;

function TDAOCZoneInfo.ZoneToWorldX(AX: DWORD) : DWORD;
begin
  Result := AX + DWORD(FBaseLoc.X);
end;

function TDAOCZoneInfo.ZoneToWorldY(AY: DWORD) : DWORD;
begin
  Result := AY + DWORD(FBaseLoc.Y);
end;

function TDAOCZoneInfo.WorldToZoneX(AX: DWORD) : DWORD;
begin
  Result := AX - DWORD(FBaseLoc.X);
end;

function TDAOCZoneInfo.WorldToZoneY(AY: DWORD) : DWORD;
begin
  Result := AY - DWORD(FBaseLoc.Y);
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

unit DAOCWorldInfo;
(***
  Wrapper classes for reading zones.mpk.
***)

interface

uses
  Types, Windows, Graphics, SysUtils, Classes, Contnrs, MPKFile, StreamINI,
  DAOCWaterRegions;

type
  TDAOCStaticRegionInfo = class(TObject)
  private
    FPlayerHousing: boolean;
    FGrassDensity: integer;
    FEntryMusic: integer;
    FGrassCSV: string;
    FGrassMap: string;
    FDetailMap: string;
    FID:  integer;
  public
    property ID: integer read FID;
    property DetailMap: string read FDetailMap;
    property EntryMusic: integer read FEntryMusic;
    property GrassCSV: string read FGrassCSV;
    property GrassMap: string read FGrassMap;
    property GrassDensity: integer read FGrassDensity;
    property PlayerHousing: boolean read FPlayerHousing;
  end;

  TDAOCStaticRegionInfoList = class(TObjectList)
  private
    function GetItems(Index: integer): TDAOCStaticRegionInfo;
    function GetItemsByID(AID: integer): TDAOCStaticRegionInfo;
  public
    property Items[Index: integer]: TDAOCStaticRegionInfo read GetItems; default;
    property ByID[AID: integer]: TDAOCStaticRegionInfo read GetItemsByID;
  end;

  TDAOCFogInfo = record
    Amount:   integer;
    Clip:     integer;
    Color:    TColor;
  end;

  TDAOCStaticZoneInfo = class(TObject)
  private
    FEnabled: boolean;
    FZoneType: integer;
    FHeight: integer;
    FEntryMusic: integer;
    FWidth: integer;
    FRegionID: integer;
    FName: string;
    FRegionOffset: TPoint;
    FID: integer;
    FDetailLoaded: boolean;
    FSectorSizeX: integer;
    FSectorSizeY: integer;
    FTerrainOffset: integer;
    FTerrainScale: integer;
    FTerrainHorizon: string;
    FTerrainClouds: string;
    FSkyColor: TColor;
    FFog: TDAOCFogInfo;
    FWaterRegions: TWaterRegionList;
    FTerrainFlipY: boolean;
    FTerrainFlipX: boolean;
    FTerrainUseTextureZoneID: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDetails(ASectorDatINI: TStreamINIFile);

      { things from the global world file (zones.mpk/zones.dat) }
    property Enabled: boolean read FEnabled;
    property EntryMusic: integer read FEntryMusic;
    property Height: integer read FHeight;
    property ID: integer read FID;
    property Name: string read FName;
    property RegionID: integer read FRegionID;
    property RegionOffset: TPoint read FRegionOffset;
    property Width: integer read FWidth;
    property ZoneType: integer read FZoneType;

      { things from individual zone definition (zoneNNN/datNNN.mpk/sector.dat) }
    property DetailLoaded: boolean read FDetailLoaded;
    property SectorSizeX: integer read FSectorSizeX;
    property SectorSizeY: integer read FSectorSizeY;
    property Fog: TDAOCFogInfo read FFog;
    property SkyColor: TColor read FSkyColor;
    property TerrainHorizon: string read FTerrainHorizon;
    property TerrainClouds: string read FTerrainClouds;
    property TerrainScale: integer read FTerrainScale;
    property TerrainOffset: integer read FTerrainOffset;
    property TerrainUseTextureZoneID: integer read FTerrainUseTextureZoneID;
    property TerrainFlipX: boolean read FTerrainFlipX;
    property TerrainFlipY: boolean read FTerrainFlipY;
    property WaterRegions: TWaterRegionList read FWaterRegions;
  end;

  TDAOCStaticZoneInfoList = class(TObjectList)
  private
    function GetItems(Index: integer): TDAOCStaticZoneInfo;
    function GetItemsByID(AID: integer): TDAOCStaticZoneInfo;
  public
    property Items[Index: integer]: TDAOCStaticZoneInfo read GetItems; default;
    property ByID[AID: integer]: TDAOCStaticZoneInfo read GetItemsByID;
  end;

  TDAOCStaticWorldInfo = class(TObject)
  private
    FRegions: TDAOCStaticRegionInfoList;
    FZones: TDAOCStaticZoneInfoList;
  protected
    procedure AddRegion(AINISection: TStreamINISection);
    procedure AddZone(AINISection: TStreamINISection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(AStrm: TStream);
    procedure LoadFromMPK(AMPK: TMPKFile); overload;
    procedure LoadFromMPK(const AMPKFileName: string); overload;
    procedure LoadFromCamelotDir(const ADirName: string);

    property Regions: TDAOCStaticRegionInfoList read FRegions;
    property Zones: TDAOCStaticZoneInfoList read FZones;
  end;

const
  ZONE_TYPE_DUNGEON = 2;
  ZONE_TYPE_HOUSING = 3;

implementation

{ TDAOCStaticRegionInfoList }

function TDAOCStaticRegionInfoList.GetItems(Index: integer): TDAOCStaticRegionInfo;
begin
  Result := TDAOCStaticRegionInfo(inherited Items[Index]);
end;

function TDAOCStaticRegionInfoList.GetItemsByID(AID: integer): TDAOCStaticRegionInfo;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ID = AID then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

{ TDAOCStaticZoneInfoList }

function TDAOCStaticZoneInfoList.GetItems(Index: integer): TDAOCStaticZoneInfo;
begin
  Result := TDAOCStaticZoneInfo(inherited Items[Index]);
end;

function TDAOCStaticZoneInfoList.GetItemsByID(AID: integer): TDAOCStaticZoneInfo;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ID = AID then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

{ TDAOCStaticWorldInfo }

procedure TDAOCStaticWorldInfo.AddRegion(AINISection: TStreamINISection);
var
  pTempRegion:  TDAOCStaticRegionInfo;
begin
  pTempRegion := TDAOCStaticRegionInfo.Create;
  pTempRegion.FID := StrToIntDef(copy(AINISection.Name, 7, Length(AINISection.Name)), 0);
  FRegions.Add(pTempRegion);

  pTempRegion.FPlayerHousing := AINISection.ReadBool('phousing', false);
  pTempRegion.FGrassDensity := AINISection.ReadInteger('grassdensity', 0);
  pTempRegion.FEntryMusic := AINISection.ReadInteger('entry_music', 0);
  pTempRegion.FGrassCSV := AINISection.ReadString('grasscsv', '');
  pTempRegion.FGrassMap := AINISection.ReadString('grasssmap', '');
  pTempRegion.FDetailMap := AINISection.ReadString('detailmap', '');
end;

procedure TDAOCStaticWorldInfo.AddZone(AINISection: TStreamINISection);
var
  pTempZone:  TDAOCStaticZoneInfo;
begin
  pTempZone := TDAOCStaticZoneInfo.Create;
  pTempZone.FID := StrToIntDef(copy(AINISection.Name, 5, Length(AINISection.Name)), 0);
  FZones.Add(pTempZone);

  pTempZone.FEnabled := AINISection.ReadBool('enabled', true);
  pTempZone.FZoneType := AINISection.ReadInteger('type', 0);
  pTempZone.FHeight := AINISection.ReadInteger('height', 0);
  pTempZone.FEntryMusic := AINISection.ReadInteger('entry_music', 0);
  pTempZone.FWidth := AINISection.ReadInteger('width', 0);
  pTempZone.FRegionID := AINISection.ReadInteger('region', 0);
  pTempZone.FName := AINISection.ReadString('name', '');
  pTempZone.FRegionOffset.X := AINISection.ReadInteger('region_offset_x', 0);
  pTempZone.FRegionOffset.Y := AINISection.ReadInteger('region_offset_y', 0);
end;

procedure TDAOCStaticWorldInfo.Clear;
begin
  FRegions.Clear;
  FZones.Clear;
end;

constructor TDAOCStaticWorldInfo.Create;
begin
  FRegions := TDAOCStaticRegionInfoList.Create;
  FZones := TDAOCStaticZoneInfoList.Create;
end;

destructor TDAOCStaticWorldInfo.Destroy;
begin
  Clear;
  FZones.Free;
  FRegions.Free;
  inherited;
end;

procedure TDAOCStaticWorldInfo.LoadFromMPK(AMPK: TMPKFile);
var
  pStrm:  TStream;
begin
  pStrm := AMPK.ExtractStream('zones.dat');
  try
    LoadFromStream(pStrm);
  finally
    pStrm.Free;
  end;
end;

procedure TDAOCStaticWorldInfo.LoadFromCamelotDir(const ADirName: string);
{ Dir should point to the base of the camelot install and should end with
  a path delimiter, eg  c:\mythic\isles\ }
begin
  LoadFromMPK(ADirName + 'zones\zones.mpk');
end;

procedure TDAOCStaticWorldInfo.LoadFromMPK(const AMPKFileName: string);
var
  pMPK:   TMPKFile;
begin
  pMPK := TMPKFile.Create(AMPKFileName);
  try
    LoadFromMPK(pMPK);
  finally
    pMPK.Free;
  end;
end;

procedure TDAOCStaticWorldInfo.LoadFromStream(AStrm: TStream);
var
  I:    integer;
  pSection:   TStreamINISection;
  pINI:   TStreamINIFile;
begin
  pINI := TStreamINIFile.Create(AStrm);
  try
    for I := 0 to pINI.SectionCount - 1 do begin
      pSection := pINI.Sections[I];

      if StrLIComp('region', PChar(pSection.Name), 6) = 0 then
        AddRegion(pSection)
      else if StrLIComp('zone', PChar(pSection.Name), 4) = 0 then
        AddZone(pSection)
    end;  { for I to sectioncount }
  finally
    pINI.Free;  // INI
  end;
end;

{ TDAOCStaticZoneInfo }

constructor TDAOCStaticZoneInfo.Create;
begin
  FWaterRegions := TWaterRegionList.Create;
end;

destructor TDAOCStaticZoneInfo.Destroy;
begin
  FWaterRegions.Free;
  
  inherited;
end;

procedure TDAOCStaticZoneInfo.LoadDetails(ASectorDatINI: TStreamINIFile);
var
  R, G, B:  BYTE;
begin
  if FDetailLoaded then
    exit;

  FDetailLoaded := true;
  FSectorSizeX := ASectorDatINI.ReadInteger('sectorsize', 'sizex', 8);
  FSectorSizeY := ASectorDatINI.ReadInteger('sectorsize', 'sizey', 8);

  FTerrainOffset := ASectorDatINI.ReadInteger('terrain', 'offsetfactor', 48);
  FTerrainScale := ASectorDatINI.ReadInteger('terrain', 'scalefactor', 8);
  FTerrainHorizon := ASectorDatINI.ReadString('terrain', 'horizon', 'horizon.tga');
  FTerrainClouds := ASectorDatINI.ReadString('terrain', 'clouds', 'clouds.tga');
  FTerrainUseTextureZoneID := ASectorDatINI.ReadInteger('terrain', 'use_texture', FID);
  FTerrainFlipX := ASectorDatINI.ReadBool('terrain', 'flip_x', false);
  FTerrainFlipY := ASectorDatINI.ReadBool('terrain', 'flip_y', false);


  R := ASectorDatINI.ReadInteger('sky', 'red', 128);
  G := ASectorDatINI.ReadInteger('sky', 'green', 215);
  B := ASectorDatINI.ReadInteger('sky', 'blue', 255);
  FSkyColor := RGB(R, G, B);

  FFog.Amount := ASectorDatINI.ReadInteger('fog', 'amount', 0);
  FFog.Clip := ASectorDatINI.ReadInteger('fog', 'clip', 65000);
  R := ASectorDatINI.ReadInteger('fog', 'red', 85);
  G := ASectorDatINI.ReadInteger('fog', 'green', 140);
  B := ASectorDatINI.ReadInteger('fog', 'blue', 166);
  FFog.Color := RGB(R, G, B);

  FWaterRegions.LoadFromINI(ASectorDatINI);
end;

end.

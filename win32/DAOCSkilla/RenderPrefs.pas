unit RenderPrefs;

interface

uses
{$IFDEF LINUX}
{$ELSE}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ColorGrd, Spin,
  StdCtrls,
{$ENDIF !LINUX}
  SysUtils, Classes, INIFiles, DAOCObjs, DAOCRegion, DAOCConSystem,
  GLRenderObjects, ExtCtrls, ComCtrls, Buttons;

type
  TMobListSortOrder = (msoName, msoDistance);

  TRenderPreferences = class(TObject)
  private
    FObjectClassFilter: TDAOCObjectClasses;
    FOnObjectFilterChanged: TNotifyEvent;
    FHasOpenGL13: boolean;
    FHasGLUT: boolean;
    FDrawFriendlyPlayers:  boolean;
    FObjectConFilter: TDAOCConColors;
    FMobListSortOrder: TMobListSortOrder;
    FOnMobListOptionsChanged: TNotifyEvent;
    FGroupByRealm: boolean;
    FGroupByClass: boolean;
    FAlternateMobListText: boolean;
    FOnMobTriangleSizeChanged: TNotifyEvent;
    FScaleMobTriangle: boolean;
    FMobTriangleMax: integer;
    FMobTriangleNom: integer;
    FMobTriangleMin: integer;
    procedure SetObjectClassFilter(const Value: TDAOCObjectClasses);
    procedure DoOnObjectFilterChanged;
    procedure DoOnMobListOptionsChanged;
    procedure DoOnMobTriangleSizeChanged;
    procedure SetHasGLUT(const Value: boolean);
    procedure SetHasOpenGL13(const Value: boolean);
    procedure SetDrawFriendlyPlayers(const Value: boolean);
    procedure SetObjectConFilter(const Value: TDAOCConColors);
    procedure SetMobListSortOrder(const Value: TMobListSortOrder);
    procedure SetGroupByRealm(const Value: boolean);
    procedure SetGroupByClass(const Value: boolean);
    procedure SetAlternateMobListText(const Value: boolean);
    procedure SetMobTriangleMax(const Value: integer);
    procedure SetMobTriangleMin(const Value: integer);
    procedure SetMobTriangleNom(const Value: integer);
    procedure SetScaleMobTriangle(const Value: boolean);
  public
    Left:   integer;
    Top:    integer;
    Width:  integer;
    Height: integer;
    Range:  integer;
    DrawHUD:          boolean;
    DrawMapVector:    boolean;
    DrawMapTexture:   boolean;
    DrawRulers:       boolean;
    DrawRangeCircles: boolean;
    DrawAIDestination:boolean;
    TrackMapClick:    boolean;
    TrackInGameSelect:  boolean;
    DrawTypeTag:      boolean;
    DrawFrameStats:   boolean;
    RedrawOnAdd:      boolean;
    RedrawOnDelete:   boolean;
    RedrawOnUpdate:   boolean;
    RedrawOnTimer:    boolean;
    StayOnTop:        boolean;
    RotateMapWithPlayer:  boolean;
    AdjacentZones:    boolean;
    ViewFrustum:      boolean;
    AttemptMapDownload: boolean;
    MapBaseURL:       string;
    InvaderWarning:   boolean;
    InvaderWarnMinTicks:  DWORD;
    PlayerRealm:      TDAOCRealm;
    PlayerLevel:      integer;
    DrawGrid:         boolean;
    DrawPushPins:     boolean;
    AnonymousStealthers: boolean;
    SmoothLines:      boolean;
    SmoothPolygons:   boolean;
    SmoothPoints:     boolean;

    constructor Create;

    procedure LoadSettings(const AFileName: string);
    procedure SaveSettings(const AFileName: string);
    function Clone : TRenderPreferences;
    function IsObjectInFilter(AObj: TDAOCObject) : boolean;
    procedure XORObjectClassFilter(AObjectClass: TDAOCObjectClass);
    procedure XORObjectConFilter(AObjectCon: TDAOCConColor);

    property HasOpenGL13: boolean read FHasOpenGL13 write SetHasOpenGL13;
    property HasGLUT: boolean read FHasGLUT write SetHasGLUT;
    property DrawFriendlyPlayers: boolean read FDrawFriendlyPlayers write SetDrawFriendlyPlayers;
    property ObjectClassFilter: TDAOCObjectClasses read FObjectClassFilter write SetObjectClassFilter;
    property ObjectConFilter: TDAOCConColors read FObjectConFilter write SetObjectConFilter;
    property MobListSortOrder: TMobListSortOrder read FMobListSortOrder write SetMobListSortOrder;
    property GroupByRealm: boolean read FGroupByRealm write SetGroupByRealm;
    property GroupByClass: boolean read FGroupByClass write SetGroupByClass;
    property AlternateMobListText: boolean read FAlternateMobListText write SetAlternateMobListText;
    property MobTriangleMin: integer read FMobTriangleMin write SetMobTriangleMin;
    property MobTriangleMax: integer read FMobTriangleMax write SetMobTriangleMax;
    property MobTriangleNom: integer read FMobTriangleNom write SetMobTriangleNom;
    property ScaleMobTriangle: boolean read FScaleMobTriangle write SetScaleMobTriangle;
    property OnObjectFilterChanged: TNotifyEvent read FOnObjectFilterChanged write FOnObjectFilterChanged;
    property OnMobListOptionsChanged: TNotifyEvent read FOnMobListOptionsChanged write FOnMobListOptionsChanged;
    property OnMobTriangleSizeChanged: TNotifyEvent read FOnMobTriangleSizeChanged write FOnMobTriangleSizeChanged; 
  end;

  TfrmRenderPrefs = class(TForm)
    chkRenderPlayers: TCheckBox;
    chkRenderMobs: TCheckBox;
    chkRenderObjects: TCheckBox;
    chkRenderUnknown: TCheckBox;
    chkVectorMaps: TCheckBox;
    Label1: TLabel;
    chkTextureMaps: TCheckBox;
    Label2: TLabel;
    chkRangeCircles: TCheckBox;
    Label3: TLabel;
    chkRulers: TCheckBox;
    Label4: TLabel;
    chkHUD: TCheckBox;
    Label5: TLabel;
    chkDestination: TCheckBox;
    Label6: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkTrackMapClick: TCheckBox;
    chkTrackGameSelection: TCheckBox;
    chkTypeTag: TCheckBox;
    chkStayOnTop: TCheckBox;
    chkRotateMap: TCheckBox;
    chkAdjacentZones: TCheckBox;
    chkViewFrustum: TCheckBox;
    Label7: TLabel;
    chkRenderVehicles: TCheckBox;
    chkInvaderWarn: TCheckBox;
    Label8: TLabel;
    chkRenderFriendlies: TCheckBox;
    chkDrawGrid: TCheckBox;
    pagePrefs: TPageControl;
    tabFilter: TTabSheet;
    tabOptions: TTabSheet;
    tabExtras: TTabSheet;
    grpFilterByType: TGroupBox;
    grpFilterByCon: TGroupBox;
    chkShowGrays: TCheckBox;
    chkShowGreens: TCheckBox;
    chkShowBlues: TCheckBox;
    chkShowYellows: TCheckBox;
    chkShowOranges: TCheckBox;
    chkShowReds: TCheckBox;
    chkShowPurples: TCheckBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    tabRangeCircles: TTabSheet;
    Label16: TLabel;
    lstRangeCircles: TListBox;
    GroupBox1: TGroupBox;
    btnAddCircle: TBitBtn;
    btnDelCircle: TBitBtn;
    edtRangeDistance: TSpinEdit;
    colorRange: TColorGrid;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    edtRangeSmoothness: TSpinEdit;
    tabMobList: TTabSheet;
    chkGroupByRealm: TCheckBox;
    grpListSort: TRadioGroup;
    chkGroupByClass: TCheckBox;
    edtInvaderWarnTicks: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    chkPushpins: TCheckBox;
    Label23: TLabel;
    chkRenderUnkStealthers: TCheckBox;
    tabGraphics: TTabSheet;
    chkSmoothLines: TCheckBox;
    chkSmoothPolys: TCheckBox;
    chkSmoothPoints: TCheckBox;
    Label24: TLabel;
    chkScaleMobTriangle: TCheckBox;
    edtMobTriangleMin: TSpinEdit;
    edtMobTriangleNom: TSpinEdit;
    edtMobTriangleMax: TSpinEdit;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    procedure ObjectFilterClick(Sender: TObject);
    procedure chkVectorMapsClick(Sender: TObject);
    procedure chkTextureMapsClick(Sender: TObject);
    procedure chkRangeCirclesClick(Sender: TObject);
    procedure chkRulersClick(Sender: TObject);
    procedure chkHUDClick(Sender: TObject);
    procedure chkDestinationClick(Sender: TObject);
    procedure chkTrackMapClickClick(Sender: TObject);
    procedure chkTrackGameSelectionClick(Sender: TObject);
    procedure chkTypeTagClick(Sender: TObject);
    procedure chkStayOnTopClick(Sender: TObject);
    procedure chkRotateMapClick(Sender: TObject);
    procedure chkAdjacentZonesClick(Sender: TObject);
    procedure chkViewFrustumClick(Sender: TObject);
    procedure chkInvaderWarnClick(Sender: TObject);
    procedure chkRenderFriendliesClick(Sender: TObject);
    procedure chkRenderPlayersClick(Sender: TObject);
    procedure chkDrawGridClick(Sender: TObject);
    procedure ObjectConClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstRangeCirclesClick(Sender: TObject);
    procedure btnAddCircleClick(Sender: TObject);
    procedure btnDelCircleClick(Sender: TObject);
    procedure colorRangeChange(Sender: TObject);
    procedure edtRangeDistanceChange(Sender: TObject);
    procedure edtRangeSmoothnessChange(Sender: TObject);
    procedure grpListSortClick(Sender: TObject);
    procedure chkGroupByRealmClick(Sender: TObject);
    procedure chkGroupByClassClick(Sender: TObject);
    procedure edtInvaderWarnTicksExit(Sender: TObject);
    procedure edtInvaderWarnTicksKeyPress(Sender: TObject; var Key: Char);
    procedure chkPushpinsClick(Sender: TObject);
    procedure chkRenderUnkStealthersClick(Sender: TObject);
    procedure chkSmoothLinesClick(Sender: TObject);
    procedure chkSmoothPolysClick(Sender: TObject);
    procedure chkSmoothPointsClick(Sender: TObject);
    procedure chkScaleMobTriangleClick(Sender: TObject);
    procedure edtMobTriangleMinChange(Sender: TObject);
    procedure edtMobTriangleNomChange(Sender: TObject);
    procedure edtMobTriangleMaxChange(Sender: TObject);
  private
    FRenderPrefs:   TRenderPreferences;
    FRangeCircles:  TRangeCircleList;
    procedure SyncFormToPrefs;
    procedure RefreshRangeCircleList;
    procedure UpdateRangeCircleDetails;
    function CurrentRangeCircle : TRangeCircle;
    procedure SelectFirstRangeCircle;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    class function Execute(AOwner: TComponent; ARenderPrefs: TRenderPreferences;
      ARangeCircles: TRangeCircleList) : boolean;
  end;

implementation

{$R *.dfm}

{ TRenderPreferences }

function TRenderPreferences.Clone: TRenderPreferences;
begin
  Result := TRenderPreferences.Create;
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
  Result.Range := Range;
  Result.ObjectClassFilter := ObjectClassFilter;
  Result.DrawHUD := DrawHUD;
  Result.DrawMapVector := DrawMapVector;
  Result.DrawMapTexture := DrawMapTexture;
  Result.DrawRulers := DrawRulers;
  Result.DrawRangeCircles := DrawRangeCircles;
  Result.DrawAIDestination := DrawAIDestination;
  Result.TrackMapClick := TrackMapClick;
  Result.TrackInGameSelect := TrackInGameSelect;
  Result.DrawTypeTag := DrawTypeTag;
  Result.DrawFrameStats := DrawFrameStats;
  Result.RedrawOnAdd := RedrawOnAdd;
  Result.RedrawOnDelete := RedrawOnDelete;
  Result.RedrawOnUpdate := RedrawOnUpdate;
  Result.RedrawOnTimer := RedrawOnTimer;
  Result.StayOnTop := StayOnTop;
  Result.RotateMapWithPlayer := RotateMapWithPlayer;
  Result.AdjacentZones := AdjacentZones;
  Result.ViewFrustum := ViewFrustum;
  Result.AlternateMobListText := AlternateMobListText;
  Result.AttemptMapDownload := AttemptMapDownload;
  Result.MapBaseURL := MapBaseURL;
  Result.InvaderWarning := InvaderWarning;
  Result.InvaderWarnMinTicks := InvaderWarnMinTicks;
  Result.HasOpenGL13 := HasOpenGL13;
  Result.GroupByRealm := GroupByRealm;
  Result.HasGLUT := HasGLUT;
  Result.DrawFriendlyPlayers := DrawFriendlyPlayers;
  Result.DrawGrid := DrawGrid;
  Result.ObjectConFilter := ObjectConFilter;
  Result.MobListSortOrder := MobListSortOrder;
  Result.GroupByClass := GroupByClass;
  Result.DrawPushPins := DrawPushPins;
  Result.AnonymousStealthers := AnonymousStealthers;
  Result.SmoothLines := SmoothLines; 
  Result.SmoothPolygons := SmoothPolygons;
  Result.SmoothPoints := SmoothPoints;
  Result.MobTriangleMin := MobTriangleMin;
  Result.MobTriangleMax := MobTriangleMax;
  Result.MobTriangleNom := MobTriangleNom;
  Result.ScaleMobTriangle := ScaleMobTriangle;
end;

constructor TRenderPreferences.Create;
begin
  ObjectClassFilter := [ocUnknown, ocObject, ocMob, ocPlayer, ocVehicle];
  ObjectConFilter := [ccGray, ccGreen, ccBlue, ccYellow, ccOrange, ccRed, ccPurple];
end;

procedure TRenderPreferences.DoOnMobListOptionsChanged;
begin
  if Assigned(FOnMobListOptionsChanged) then
    FOnMobListOptionsChanged(Self);
end;

procedure TRenderPreferences.DoOnMobTriangleSizeChanged;
begin
  if Assigned(FOnMobTriangleSizeChanged) then
    FOnMobTriangleSizeChanged(Self);
end;

procedure TRenderPreferences.DoOnObjectFilterChanged;
begin
  if Assigned(FOnObjectFilterChanged) then
    FOnObjectFilterChanged(Self);
end;

function TRenderPreferences.IsObjectInFilter(AObj: TDAOCObject): boolean;
var
  ocl:  TDAOCObjectClass;
  ocn:  TDAOCConColor;
begin
  ocl := AObj.ObjectClass;
  ocn := GetConColor(PlayerLevel, AObj.Level);
   
  Result := (ocl in ObjectClassFilter) and (ocn in ObjectConFilter) and
    ((ocl <> ocPlayer) or FDrawFriendlyPlayers or (AObj.Realm <> PlayerRealm));
end;

procedure TRenderPreferences.LoadSettings(const AFileName: string);
begin
  with TINIFile.Create(AFileName) do begin
    Left := ReadInteger('RenderPrefs', 'Left', 0);
    Top := ReadInteger('RenderPrefs', 'Top', 0);
    Width := ReadInteger('RenderPrefs', 'Width', 640);
    Height := ReadInteger('RenderPrefs', 'Height', 480);
    Range := ReadInteger('RenderPrefs', 'Range', 6000);
    ObjectClassFilter := IntToObjectClasses(ReadInteger('RenderPrefs', 'ObjectClassFilter', $7fffffff));
    ObjectConFilter := IntToConColors(ReadInteger('RenderPrefs', 'ObjectConFilter', $7fffffff));
    DrawHUD := ReadBool('RenderPrefs', 'DrawHUD', true);
    DrawMapVector := ReadBool('RenderPrefs', 'DrawMapVector', true);
    DrawMapTexture := ReadBool('RenderPrefs', 'DrawMapTexture', true);
    DrawRulers := ReadBool('RenderPrefs', 'DrawRulers', true);
    DrawRangeCircles := ReadBool('RenderPrefs', 'DrawRangeCircles', true);
    DrawAIDestination := ReadBool('RenderPrefs', 'DrawAIDestination', true);
    TrackMapClick := ReadBool('RenderPrefs', 'TrackMapClick', true);
    TrackInGameSelect := ReadBool('RenderPrefs', 'TrackInGameSelect', true);
    DrawTypeTag := ReadBool('RenderPrefs', 'DrawTypeTag', false);
    DrawFrameStats := ReadBool('RenderPrefs', 'DrawFrameStats', false);
    RedrawOnAdd := ReadBool('RenderPrefs', 'RedrawOnAdd', true);
    RedrawOnDelete := ReadBool('RenderPrefs', 'RedrawOnDelete', true);
    RedrawOnUpdate := ReadBool('RenderPrefs', 'RedrawOnUpdate', true);
    RedrawOnTimer := ReadBool('RenderPrefs', 'RedrawOnTimer', true);
    StayOnTop := ReadBool('RenderPrefs', 'StayOnTop', false);
    RotateMapWithPlayer := ReadBool('RenderPrefs', 'RotateMapWithPlayer', false);
    AdjacentZones := ReadBool('RenderPrefs', 'AdjacentZones', false);
    ViewFrustum := ReadBool('RenderPrefs', 'ViewFrustum', true);
    AlternateMobListText := DrawTypeTag; //ReadBool('RenderPrefs', 'AlternateMobListText', false);
    AttemptMapDownload := ReadBool('RenderPrefs', 'AttemptMapDownload', true);
    MapBaseURL := ReadString('RenderPrefs', 'MapBaseURL', 'http://capnbry.net/daoc/map.php?');
    InvaderWarning := ReadBool('RenderPrefs', 'InvaderWarning', true);
    InvaderWarnMinTicks := ReadInteger('RenderPrefs', 'InvaderWarnMinTicks', 5000);
    GroupByRealm := ReadBool('RenderPrefs', 'GroupByRealm', true);
    DrawFriendlyPlayers := ReadBool('RenderPrefs', 'DrawFriendlyPlayers', true);
    DrawGrid := ReadBool('RenderPrefs', 'DrawGrid', false);
    MobListSortOrder := TMobListSortOrder(ReadInteger('RenderPrefs', 'MobListSortOrder', 0));
    GroupByClass := ReadBool('RenderPrefs', 'GroupByClass', true);
    DrawPushPins := ReadBool('RenderPrefs', 'DrawPushPins', true);
    AnonymousStealthers := ReadBool('RenderPrefs', 'AnonymousStealthers', true);
    SmoothLines := ReadBool('RenderPrefs', 'SmoothLines', false);
    SmoothPolygons := ReadBool('RenderPrefs', 'SmoothPolygons', false);
    SmoothPoints := ReadBool('RenderPrefs', 'SmoothPoints', false);
    MobTriangleMin := ReadInteger('RenderPrefs', 'MobTriangleMin', 25);
    MobTriangleMax := ReadInteger('RenderPrefs', 'MobTriangleMax', 300);
    MobTriangleNom := ReadInteger('RenderPrefs', 'MobTriangleNom', 150);
    ScaleMobTriangle := ReadBool('RenderPrefs', 'ScaleMobTriangle', true);
  end;
end;

procedure TRenderPreferences.SaveSettings(const AFileName: string);
begin
  with TINIFile.Create(AFileName) do begin
    WriteInteger('RenderPrefs', 'Left', Left);
    WriteInteger('RenderPrefs', 'Top', Top);
    WriteInteger('RenderPrefs', 'Width', Width);
    WriteInteger('RenderPrefs', 'Height', Height);
    WriteInteger('RenderPrefs', 'Range', Range);
    WriteInteger('RenderPrefs', 'ObjectClassFilter', ObjectClassesToInt(ObjectClassFilter));
    WriteInteger('RenderPrefs', 'ObjectConFilter', ConColorsToInt(ObjectConFilter));
    WriteBool('RenderPrefs', 'DrawHUD', DrawHUD);
    WriteBool('RenderPrefs', 'DrawMapVector', DrawMapVector);
    WriteBool('RenderPrefs', 'DrawMapTexture', DrawMapTexture);
    WriteBool('RenderPrefs', 'DrawRulers', DrawRulers);
    WriteBool('RenderPrefs', 'DrawRangeCircles', DrawRangeCircles);
    WriteBool('RenderPrefs', 'DrawAIDestination', DrawAIDestination);
    WriteBool('RenderPrefs', 'TrackMapClick', TrackMapClick);
    WriteBool('RenderPrefs', 'TrackInGameSelect', TrackInGameSelect);
    WriteBool('RenderPrefs', 'DrawTypeTag', DrawTypeTag);
    WriteBool('RenderPrefs', 'DrawFrameStats', DrawFrameStats);
    WriteBool('RenderPrefs', 'RedrawOnAdd', RedrawOnAdd);
    WriteBool('RenderPrefs', 'RedrawOnDelete', RedrawOnDelete);
    WriteBool('RenderPrefs', 'RedrawOnUpdate', RedrawOnUpdate);
    WriteBool('RenderPrefs', 'RedrawOnTimer', RedrawOnTimer);
    WriteBool('RenderPrefs', 'StayOnTop', StayOnTop);
    WriteBool('RenderPrefs', 'RotateMapWithPlayer', RotateMapWithPlayer);
    WriteBool('RenderPrefs', 'AdjacentZones', AdjacentZones);
    WriteBool('RenderPrefs', 'ViewFrustum', ViewFrustum);
    WriteBool('RenderPrefs', 'AlternateMobListText', AlternateMobListText);
    WriteBool('RenderPrefs', 'AttemptMapDownload', AttemptMapDownload);
    DeleteKey('RenderPrefs', 'MapBaseURL');  // WriteString('RenderPrefs', 'MapBaseURL', MapBaseURL);
    WriteBool('RenderPrefs', 'InvaderWarning', InvaderWarning);
    WriteInteger('RenderPrefs', 'InvaderWarnMinTicks', InvaderWarnMinTicks);
    WriteBool('RenderPrefs', 'GroupByRealm', GroupByRealm);
    WriteBool('RenderPrefs', 'DrawFriendlyPlayers', DrawFriendlyPlayers);
    WriteBool('RenderPrefs', 'DrawGrid', DrawGrid);
    WriteInteger('RenderPrefs', 'MobListSortOrder', ord(MobListSortOrder));
    WriteBool('RenderPrefs', 'GroupByClass', GroupByClass);
    WriteBool('RenderPrefs', 'DrawPushPins', DrawPushPins);
    WriteBool('RenderPrefs', 'AnonymousStealthers', AnonymousStealthers);
    WriteBool('RenderPrefs', 'SmoothLines', SmoothLines);
    WriteBool('RenderPrefs', 'SmoothPolygons', SmoothPolygons);
    WriteBool('RenderPrefs', 'SmoothPoints', SmoothPoints);
    WriteInteger('RenderPrefs', 'MobTriangleMin', MobTriangleMin);
    WriteInteger('RenderPrefs', 'MobTriangleMax', MobTriangleMax);
    WriteInteger('RenderPrefs', 'MobTriangleNom', MobTriangleNom);
    WriteBool('RenderPrefs', 'ScaleMobTriangle', ScaleMobTriangle);
  end;
end;

procedure TRenderPreferences.SetAlternateMobListText(const Value: boolean);
begin
  FAlternateMobListText := Value;
  DoOnMobListOptionsChanged;
end;

procedure TRenderPreferences.SetDrawFriendlyPlayers(const Value: boolean);
begin
  FDrawFriendlyPlayers := Value;
  DoOnObjectFilterChanged;
end;

procedure TRenderPreferences.SetGroupByClass(const Value: boolean);
begin
  FGroupByClass := Value;
  DoOnMobListOptionsChanged;
end;

procedure TRenderPreferences.SetGroupByRealm(const Value: boolean);
begin
  FGroupByRealm := Value;
  DoOnMobListOptionsChanged;
end;

procedure TRenderPreferences.SetHasGLUT(const Value: boolean);
begin
  FHasGLUT := Value;
  DrawHUD := FHasGLUT and DrawHUD;
end;

procedure TRenderPreferences.SetHasOpenGL13(const Value: boolean);
begin
  FHasOpenGL13 := Value;
  DrawMapTexture := FHasOpenGL13 and DrawMapTexture;
end;

procedure TRenderPreferences.SetMobListSortOrder(const Value: TMobListSortOrder);
begin
  FMobListSortOrder := Value;
  DoOnMobListOptionsChanged;
end;

procedure TRenderPreferences.SetMobTriangleMax(const Value: integer);
begin
  FMobTriangleMax := Value;
  DoOnMobTriangleSizeChanged;
end;

procedure TRenderPreferences.SetMobTriangleMin(const Value: integer);
begin
  FMobTriangleMin := Value;
  DoOnMobTriangleSizeChanged;
end;

procedure TRenderPreferences.SetMobTriangleNom(const Value: integer);
begin
  FMobTriangleNom := Value;
  DoOnMobTriangleSizeChanged;
end;

procedure TRenderPreferences.SetObjectClassFilter(const Value: TDAOCObjectClasses);
begin
  FObjectClassFilter := Value;
  DoOnObjectFilterChanged;
end;

procedure TRenderPreferences.SetObjectConFilter(const Value: TDAOCConColors);
begin
  FObjectConFilter := Value;
  DoOnObjectFilterChanged;
end;

procedure TRenderPreferences.SetScaleMobTriangle(const Value: boolean);
begin
  FScaleMobTriangle := Value;
  DoOnMobTriangleSizeChanged;
end;

procedure TRenderPreferences.XORObjectClassFilter(AObjectClass: TDAOCObjectClass);
begin
  if AObjectClass in FObjectClassFilter then
    Exclude(FObjectClassFilter, AObjectClass)
  else
    Include(FObjectClassFilter, AObjectClass);
  DoOnObjectFilterChanged;
end;

procedure TRenderPreferences.XORObjectConFilter(AObjectCon: TDAOCConColor);
begin
  if AObjectCon in FObjectConFilter then
    Exclude(FObjectConFilter, AObjectCon)
  else
    Include(FObjectConFilter, AObjectCon);
  DoOnObjectFilterChanged;
end;

{ TfrmRenderPrefs }

procedure TfrmRenderPrefs.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    ExStyle := ExStyle or WS_EX_TOPMOST;
    WndParent := GetDesktopWindow;
  end;
end;

class function TfrmRenderPrefs.Execute(AOwner: TComponent; ARenderPrefs: TRenderPreferences;
  ARangeCircles: TRangeCircleList): boolean;
begin
  with TfrmRenderPrefs.Create(AOwner) do
  try
    FRenderPrefs := ARenderPrefs;
    FRangeCircles := ARangeCircles;
    SyncFormToPrefs;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

procedure TfrmRenderPrefs.ObjectFilterClick(Sender: TObject);
var
  objFilter: TDAOCObjectClasses;
begin
  objFilter := FRenderPrefs.ObjectClassFilter;
  with TCheckbox(Sender) do
    if Checked then
      Include(objFilter, TDAOCObjectClass(Tag))
    else
      Exclude(objFilter, TDAOCObjectClass(Tag));

  FRenderPrefs.ObjectClassFilter := objFilter;
end;

procedure TfrmRenderPrefs.chkVectorMapsClick(Sender: TObject);
begin
  FRenderPrefs.DrawMapVector := chkVectorMaps.Checked;
end;

procedure TfrmRenderPrefs.chkTextureMapsClick(Sender: TObject);
begin
  FRenderPrefs.DrawMapTexture := chkTextureMaps.Checked;
end;

procedure TfrmRenderPrefs.chkRangeCirclesClick(Sender: TObject);
begin
  FRenderPrefs.DrawRangeCircles := chkRangeCircles.Checked;
end;

procedure TfrmRenderPrefs.chkRulersClick(Sender: TObject);
begin
  FRenderPrefs.DrawRulers := chkRulers.Checked;
end;

procedure TfrmRenderPrefs.chkHUDClick(Sender: TObject);
begin
  FRenderPrefs.DrawHUD := chkHUD.Checked;
end;

procedure TfrmRenderPrefs.chkDestinationClick(Sender: TObject);
begin
  FRenderPrefs.DrawAIDestination := chkDestination.Checked;
end;

procedure TfrmRenderPrefs.SyncFormToPrefs;
begin
  chkRenderPlayers.Checked := ocPlayer in FRenderPrefs.ObjectClassFilter;
  chkRenderFriendlies.Checked := FRenderPrefs.DrawFriendlyPlayers;
  chkRenderMobs.Checked := ocMob in FRenderPrefs.ObjectClassFilter;
  chkRenderObjects.Checked := ocObject in FRenderPrefs.ObjectClassFilter;
  chkRenderUnknown.Checked := ocUnknown in FRenderPrefs.ObjectClassFilter;
  chkRenderVehicles.Checked := ocVehicle in FRenderPrefs.ObjectClassFilter;
  chkRenderFriendlies.Enabled := chkRenderPlayers.Checked;
  chkRenderUnkStealthers.Checked := FRenderPrefs.AnonymousStealthers;

  chkShowGrays.Checked := ccGray in FRenderPrefs.ObjectConFilter;
  chkShowGreens.Checked := ccGreen in FRenderPrefs.ObjectConFilter;
  chkShowBlues.Checked := ccBlue in FRenderPrefs.ObjectConFilter;
  chkShowYellows.Checked := ccYellow in FRenderPrefs.ObjectConFilter;
  chkShowOranges.Checked := ccOrange in FRenderPrefs.ObjectConFilter;
  chkShowReds.Checked := ccRed in FRenderPrefs.ObjectConFilter;
  chkShowPurples.Checked := ccPurple in FRenderPrefs.ObjectConFilter;

  chkVectorMaps.Checked := FRenderPrefs.DrawMapVector;
  chkPushpins.Checked := FRenderPrefs.DrawPushPins;
  chkTextureMaps.Enabled := FRenderPrefs.HasOpenGL13;
  chkTextureMaps.Checked := FRenderPrefs.HasOpenGL13 and FRenderPrefs.DrawMapTexture;
  chkRangeCircles.Checked := FRenderPrefs.DrawRangeCircles;
  chkRulers.Checked := FRenderPrefs.DrawRulers;
  chkHUD.Checked := FRenderPrefs.DrawHUD;
  chkDestination.Checked := FRenderPrefs.DrawAIDestination;
  chkViewFrustum.Checked := FRenderPrefs.ViewFrustum;
  chkInvaderWarn.Checked := FRenderPrefs.InvaderWarning;
  chkDrawGrid.Checked := FRenderPrefs.DrawGrid;
  edtInvaderWarnTicks.Text := IntToStr(FRenderPrefs.InvaderWarnMinTicks div 1000);

  chkTrackMapClick.Checked := FRenderPrefs.TrackMapClick;
  chkTrackGameSelection.Checked := FRenderPrefs.TrackInGameSelect;
//  chkTypeTag.Enabled := FRenderPrefs.HasGLUT;
  chkTypeTag.Checked := FRenderPrefs.DrawTypeTag;
  chkStayOnTop.Checked := FRenderPrefs.StayOnTop;
  chkRotateMap.Checked := FRenderPrefs.RotateMapWithPlayer;
  chkAdjacentZones.Checked := FRenderPrefs.AdjacentZones;

  chkGroupByRealm.Checked := FRenderPrefs.GroupByRealm;
  chkGroupByClass.Checked := FRenderPrefs.GroupByClass;
  grpListSort.ItemIndex := ord(FRenderPrefs.MobListSortOrder);

  RefreshRangeCircleList;
  SelectFirstRangeCircle;

  chkSmoothLines.Checked := FRenderPrefs.SmoothLines;
  chkSmoothPolys.Checked := FRenderPrefs.SmoothPolygons;
  chkSmoothPoints.Checked := FRenderPrefs.SmoothPoints;
  edtMobTriangleMin.Value := FRenderPrefs.MobTriangleMin;
  edtMobTriangleMax.Value := FRenderPrefs.MobTriangleMax;
  edtMobTriangleNom.Value := FRenderPrefs.MobTriangleNom;
  chkScaleMobTriangle.Checked := FRenderPrefs.ScaleMobTriangle;
  chkScaleMobTriangleClick(nil); 
end;

procedure TfrmRenderPrefs.chkTrackMapClickClick(Sender: TObject);
begin
  FRenderPrefs.TrackMapClick := chkTrackMapClick.Checked;
end;

procedure TfrmRenderPrefs.chkTrackGameSelectionClick(Sender: TObject);
begin
  FRenderPrefs.TrackInGameSelect := chkTrackGameSelection.Checked;
end;

procedure TfrmRenderPrefs.chkTypeTagClick(Sender: TObject);
begin
  FRenderPrefs.DrawTypeTag := chkTypeTag.Checked;
  FRenderPrefs.AlternateMobListText := FRenderPrefs.DrawTypeTag;
end;

procedure TfrmRenderPrefs.chkStayOnTopClick(Sender: TObject);
begin
  FRenderPrefs.StayOnTop := chkStayOnTop.Checked;
end;

procedure TfrmRenderPrefs.chkRotateMapClick(Sender: TObject);
begin
  FRenderPrefs.RotateMapWithPlayer := chkRotateMap.Checked;
end;

procedure TfrmRenderPrefs.chkAdjacentZonesClick(Sender: TObject);
begin
  FRenderPrefs.AdjacentZones := chkAdjacentZones.Checked;
end;

procedure TfrmRenderPrefs.chkViewFrustumClick(Sender: TObject);
begin
  FRenderPrefs.ViewFrustum := chkViewFrustum.Checked;
end;

procedure TfrmRenderPrefs.chkInvaderWarnClick(Sender: TObject);
begin
  FRenderPrefs.InvaderWarning := chkInvaderWarn.Checked;
end;

procedure TfrmRenderPrefs.chkRenderFriendliesClick(Sender: TObject);
begin
  FRenderPrefs.DrawFriendlyPlayers := chkRenderFriendlies.Checked;
end;

procedure TfrmRenderPrefs.chkRenderPlayersClick(Sender: TObject);
begin
  chkRenderFriendlies.Enabled := chkRenderPlayers.Checked;
  ObjectFilterClick(Sender);
end;

procedure TfrmRenderPrefs.chkDrawGridClick(Sender: TObject);
begin
  FRenderPrefs.DrawGrid := chkDrawGrid.Checked;
end;

procedure TfrmRenderPrefs.ObjectConClick(Sender: TObject);
var
  objFilter: TDAOCConColors;
begin
  objFilter := FRenderPrefs.ObjectConFilter;
  with TCheckbox(Sender) do
    if Checked then
      Include(objFilter, TDAOCConColor(Tag))
    else
      Exclude(objFilter, TDAOCConColor(Tag));

  FRenderPrefs.ObjectConFilter := objFilter;
end;

procedure TfrmRenderPrefs.FormCreate(Sender: TObject);
begin
  pagePrefs.ActivePageIndex := 0;
end;

procedure TfrmRenderPrefs.RefreshRangeCircleList;
var
  I:    integer;
begin
  lstRangeCircles.Clear;
  for I := 0 to FRangeCircles.Count - 1 do
    lstRangeCircles.AddItem(IntToStr(FRangeCircles[I].Range), FRangeCircles[I]);
end;

procedure TfrmRenderPrefs.UpdateRangeCircleDetails;
var
  pCircle:  TRangeCircle;
begin
  pCircle := CurrentRangeCircle;
  if not Assigned(pCircle) then
    exit;

  edtRangeDistance.Value := pCircle.Range;
  edtRangeSmoothness.Value := pCircle.Smoothness;
  if colorRange.ColorToIndex(pCircle.Color) <> -1 then
    colorRange.ForegroundIndex := colorRange.ColorToIndex(pCircle.Color);
end;

function TfrmRenderPrefs.CurrentRangeCircle: TRangeCircle;
begin
  if (lstRangeCircles.ItemIndex >= FRangeCircles.Count) or (lstRangeCircles.ItemIndex = -1) then
    Result := nil
  else
    Result := FRangeCircles[lstRangeCircles.ItemIndex];
end;

procedure TfrmRenderPrefs.lstRangeCirclesClick(Sender: TObject);
begin
  UpdateRangeCircleDetails;
end;

procedure TfrmRenderPrefs.btnAddCircleClick(Sender: TObject);
begin
  FRangeCircles.Add(TRangeCircle.CreateRange(500, 24));
  RefreshRangeCircleList;
  lstRangeCircles.ItemIndex := lstRangeCircles.Items.Count - 1;
  UpdateRangeCircleDetails;
end;

procedure TfrmRenderPrefs.btnDelCircleClick(Sender: TObject);
begin
  if (lstRangeCircles.ItemIndex < FRangeCircles.Count) and (lstRangeCircles.ItemIndex <> -1) then begin
    FRangeCircles.Delete(lstRangeCircles.ItemIndex);
    RefreshRangeCircleList;
    SelectFirstRangeCircle;
  end;
end;

procedure TfrmRenderPrefs.SelectFirstRangeCircle;
begin
  if lstRangeCircles.Items.Count > 0 then begin
    lstRangeCircles.ItemIndex := 0;
    UpdateRangeCircleDetails;
  end;
end;

procedure TfrmRenderPrefs.colorRangeChange(Sender: TObject);
var
  pCircle:  TRangeCircle;
begin
  pCircle := CurrentRangeCircle;
  if not Assigned(pCircle) then
    exit;
    
  pCircle.Color := colorRange.ForegroundColor;
end;

procedure TfrmRenderPrefs.edtRangeDistanceChange(Sender: TObject);
var
  pCircle:  TRangeCircle;
begin
  pCircle := CurrentRangeCircle;
  if not Assigned(pCircle) then
    exit;
  pCircle.Range := edtRangeDistance.Value;
  pCircle.GLCleanup;
end;

procedure TfrmRenderPrefs.edtRangeSmoothnessChange(Sender: TObject);
var
  pCircle:  TRangeCircle;
begin
  pCircle := CurrentRangeCircle;
  if not Assigned(pCircle) then
    exit;
  pCircle.Smoothness := edtRangeSmoothness.Value;
  pCircle.GLCleanup;
end;

procedure TfrmRenderPrefs.grpListSortClick(Sender: TObject);
begin
  FRenderPrefs.MobListSortOrder := TMobListSortOrder(grpListSort.ItemIndex);
end;

procedure TfrmRenderPrefs.chkGroupByRealmClick(Sender: TObject);
begin
  FRenderPrefs.GroupByRealm := chkGroupByRealm.Checked;
end;

procedure TfrmRenderPrefs.chkGroupByClassClick(Sender: TObject);
begin
  FRenderPrefs.GroupByClass := chkGroupByClass.Checked;
end;

procedure TfrmRenderPrefs.edtInvaderWarnTicksExit(Sender: TObject);
begin
  FRenderPrefs.InvaderWarnMinTicks := StrToInt(edtInvaderWarnTicks.Text) * 1000;
end;

procedure TfrmRenderPrefs.edtInvaderWarnTicksKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TfrmRenderPrefs.chkPushpinsClick(Sender: TObject);
begin
  FRenderPrefs.DrawPushPins := chkPushpins.Checked;
end;

procedure TfrmRenderPrefs.chkRenderUnkStealthersClick(Sender: TObject);
begin
  FRenderPrefs.AnonymousStealthers := chkRenderUnkStealthers.Checked;
end;

procedure TfrmRenderPrefs.chkSmoothLinesClick(Sender: TObject);
begin
  FRenderPrefs.SmoothLines := chkSmoothLines.Checked;
end;

procedure TfrmRenderPrefs.chkSmoothPolysClick(Sender: TObject);
begin
  FRenderPrefs.SmoothPolygons := chkSmoothPolys.Checked;
end;

procedure TfrmRenderPrefs.chkSmoothPointsClick(Sender: TObject);
begin
  FRenderPrefs.SmoothPoints := chkSmoothPoints.Checked;
end;

procedure TfrmRenderPrefs.chkScaleMobTriangleClick(Sender: TObject);
begin
  FRenderPrefs.ScaleMobTriangle := chkScaleMobTriangle.Checked;

  edtMobTriangleMin.Enabled := FRenderPrefs.ScaleMobTriangle;
  edtMobTriangleMax.Enabled := FRenderPrefs.ScaleMobTriangle;
end;

procedure TfrmRenderPrefs.edtMobTriangleMinChange(Sender: TObject);
begin
  FRenderPrefs.MobTriangleMin := edtMobTriangleMin.Value;
end;

procedure TfrmRenderPrefs.edtMobTriangleNomChange(Sender: TObject);
begin
  FRenderPrefs.MobTriangleNom := edtMobTriangleNom.Value;
end;

procedure TfrmRenderPrefs.edtMobTriangleMaxChange(Sender: TObject);
begin
  FRenderPrefs.MobTriangleMax := edtMobTriangleMax.Value;
end;

end.

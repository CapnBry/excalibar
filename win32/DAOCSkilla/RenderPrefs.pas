unit RenderPrefs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, INIFiles, DAOCObjs, StdCtrls, Buttons;

type
  TRenderPreferences = class(TObject)
  private
    FObjectFilter: TDAOCObjectClasses;
    FOnObjectFilterChanged: TNotifyEvent;
    procedure SetObjectFilter(const Value: TDAOCObjectClasses);
    procedure DoOnObjectFilterChanged;
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
    AlternateMobListText: boolean;
    AttemptMapDownload: boolean;
    MapBaseURL:       string;
    InvaderWarning:   boolean;
    InvaderWarnMinTicks:  DWORD;
    HasOpenGL13:      boolean;

    constructor Create;

    procedure LoadSettings(const AFileName: string);
    procedure SaveSettings(const AFileName: string);
    function Clone : TRenderPreferences;
    function IsObjectInFilter(AObj: TDAOCObject) : boolean;
    procedure XORObjectFilter(AObjectClass: TDAOCObjectClass);

    property ObjectFilter: TDAOCObjectClasses read FObjectFilter write SetObjectFilter;
    property OnObjectFilterChanged: TNotifyEvent read FOnObjectFilterChanged write FOnObjectFilterChanged;
  end;

  TfrmRenderPrefs = class(TForm)
    grpObjects: TGroupBox;
    chkRenderPlayers: TCheckBox;
    chkRenderMobs: TCheckBox;
    chkRenderObjects: TCheckBox;
    chkRenderUnknown: TCheckBox;
    grpExtras: TGroupBox;
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
    grpUIOptions: TGroupBox;
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
  private
    FRenderPrefs:   TRenderPreferences;
    procedure SyncFormToPrefs;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    class function Execute(AOwner: TComponent; ARenderPrefs: TRenderPreferences) : boolean;
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
  Result.ObjectFilter := ObjectFilter;
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
end;

constructor TRenderPreferences.Create;
begin
  ObjectFilter := [ocUnknown, ocObject, ocMob, ocPlayer, ocVehicle];
end;

procedure TRenderPreferences.DoOnObjectFilterChanged;
begin
  if Assigned(FOnObjectFilterChanged) then
    FOnObjectFilterChanged(Self);
end;

function TRenderPreferences.IsObjectInFilter(AObj: TDAOCObject): boolean;
begin
  Result := AObj.ObjectClass in ObjectFilter;
end;

procedure TRenderPreferences.LoadSettings(const AFileName: string);
begin
  with TINIFile.Create(AFileName) do begin
    Left := ReadInteger('RenderPrefs', 'Left', 0);
    Top := ReadInteger('RenderPrefs', 'Top', 0);
    Width := ReadInteger('RenderPrefs', 'Width', 640);
    Height := ReadInteger('RenderPrefs', 'Height', 480);
    Range := ReadInteger('RenderPrefs', 'Range', 6000);
    // ObjectFilter := TDAOCObjectClasses(ReadInteger('RenderPrefs', 'Scale', 0));
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
    MapBaseURL := ReadString('RenderPrefs', 'MapBaseURL', 'http://capnbry.net/daoc/map.php?z=%d');
    InvaderWarning := ReadBool('RenderPrefs', 'InvaderWarning', true);
    InvaderWarnMinTicks := ReadInteger('RenderPrefs', 'InvaderWarnMinTicks', 5000);
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
    // WriteInteger('RenderPrefs', 'Scale', ord(ObjectFilter));
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
  end;
end;

procedure TRenderPreferences.SetObjectFilter(const Value: TDAOCObjectClasses);
begin
  FObjectFilter := Value;
  DoOnObjectFilterChanged;
end;

procedure TRenderPreferences.XORObjectFilter(AObjectClass: TDAOCObjectClass);
begin
  if AObjectClass in FObjectFilter then
    Exclude(FObjectFilter, AObjectClass)
  else
    Include(FObjectFilter, AObjectClass);

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

class function TfrmRenderPrefs.Execute(AOwner: TComponent; ARenderPrefs: TRenderPreferences): boolean;
begin
  with TfrmRenderPrefs.Create(AOwner) do
  try
    FRenderPrefs := ARenderPrefs;
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
  objFilter := FRenderPrefs.ObjectFilter;
  with TCheckbox(Sender) do
    if Checked then
      Include(objFilter, TDAOCObjectClass(Tag))
    else
      Exclude(objFilter, TDAOCObjectClass(Tag));
      
  FRenderPrefs.ObjectFilter := objFilter;
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
  chkRenderPlayers.Checked := ocPlayer in FRenderPrefs.ObjectFilter;
  chkRenderMobs.Checked := ocMob in FRenderPrefs.ObjectFilter;
  chkRenderObjects.Checked := ocObject in FRenderPrefs.ObjectFilter;
  chkRenderUnknown.Checked := ocUnknown in FRenderPrefs.ObjectFilter;
  chkRenderVehicles.Checked := ocVehicle in FRenderPrefs.ObjectFilter;

  chkVectorMaps.Checked := FRenderPrefs.DrawMapVector;
  chkTextureMaps.Enabled := FRenderPrefs.HasOpenGL13;
  chkTextureMaps.Checked := FRenderPrefs.HasOpenGL13 and FRenderPrefs.DrawMapTexture;
  chkRangeCircles.Checked := FRenderPrefs.DrawRangeCircles;
  chkRulers.Checked := FRenderPrefs.DrawRulers;
  chkHUD.Checked := FRenderPrefs.DrawHUD;
  chkDestination.Checked := FRenderPrefs.DrawAIDestination;
  chkViewFrustum.Checked := FRenderPrefs.ViewFrustum;
  chkInvaderWarn.Checked := FRenderPrefs.InvaderWarning;

  chkTrackMapClick.Checked := FRenderPrefs.TrackMapClick;
  chkTrackGameSelection.Checked := FRenderPrefs.TrackInGameSelect;
  chkTypeTag.Checked := FRenderPrefs.DrawTypeTag;
  chkStayOnTop.Checked := FRenderPrefs.StayOnTop;
  chkRotateMap.Checked := FRenderPrefs.RotateMapWithPlayer;
  chkAdjacentZones.Checked := FRenderPrefs.AdjacentZones;
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

end.

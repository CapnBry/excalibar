unit GLRender;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, glWindow, GL, GLU, GLext, DAOCConnection, ComCtrls, DAOCObjs,
  StdCtrls, GLRenderObjects, MapElementList, DAOCRegion, GLUT, RenderPrefs,
  DAOCClasses, QuickSinCos;

type
  TfrmGLRender = class(TForm)
    glMap: TglWindow;
    slideZoom: TTrackBar;
    tmrMinFPS: TTimer;
    pnlMap: TPanel;
    pnlLeft: TPanel;
    lblObjCounts: TLabel;
    lstObjects: TListBox;
    pnlGridHeader: TPanel;
    procedure glMapDraw(Sender: TObject);
    procedure glMapInit(Sender: TObject);
    procedure glMapResize(Sender: TObject);
    procedure slideZoomChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure glMapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrMinFPSTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure glMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstObjectsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstObjectsClick(Sender: TObject);
  private
    FDControl: TDAOCConnection;
    FRange:         DWORD;
    FRenderBounds:  TRect;
    FGLInitsCalled:       boolean;
    FDirty:   boolean;
    FInvaderHighlight:    boolean;
    FInvaderHighlightLastSwap:  DWORD;
    FMapElementsListList:   TVectorMapElementListList;
    FMapTexturesListList:   TTextureMapElementListList;
    FRangeCircles:  TRangeCircleList;
    FMobTriangle:   T3DArrowHead;
    FObjectTriangle:    T3DPyramid;
    FGroundTarget:      TGLBullsEye;
    FVisibleRangeRep:   TGLFlatViewFrustum;
    FMapToPlayerOffset: TPoint;
    FFilteredObjects:   TDAOCObjectList;
    FRenderPrefs:   TRenderPreferences;
    FPrefsFile:     string;
    FFrameStats:    string;
    FFrameCount:    integer;
    FDirtyCount:    integer;
    FInvalidateCount: integer;

    procedure GLInits;
    procedure GLCleanups;
    procedure SetupRadarProjectionMatrix;
    procedure SetupScreenProjectionMatrix;
    procedure CheckGLError;

    procedure DrawMapRulers;
    procedure DrawRangeCircles;
    procedure DrawPlayerTriangle;
    procedure DrawLineToSelected;
    procedure DrawMobsAndPlayers;
    procedure DrawMapElements;
    procedure DrawPlayerHighlightRing(ADAOCObject: TDAOCPlayer);
    procedure DrawHUD;
    procedure DrawMobTypeTag(AMob: TDAOCMob);
    procedure DrawGroundTarget;
    procedure DrawFrameStats;
    procedure DrawLineToMobTarget(AMob: TDAOCMob);

    procedure SetDControl(const Value: TDAOCConnection);
    procedure Log(const s: string);
    procedure RefreshFilteredList;
    procedure GridSelectObject(ADAOCObject: TDAOCObject);
    function FilteredObjectInsert(ADAOCObject: TDAOCObject) : integer;
    procedure UpdateObjectCounts;
    procedure SetObjectListRowCount(ACount: integer);
    procedure RENDERPrefsObjectFilterChanged(Sender: TObject);
    procedure UpdateFrameStats(ATime: integer);
    procedure InvalidateListObject(AObj: TDAOCObject);
    procedure UpdateStayOnTop;
    procedure ReloadMapElementsAndTextures;
    procedure DoPrefsDialog;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure DAOCAddObject(AObj: TDAOCObject);
    procedure DAOCDeleteObject(AObj: TDAOCObject);
    procedure DAOCUpdateObject(AObj: TDAOCObject);
    procedure DAOCSelectedObjectChanged(AObj: TDAOCObject);
    procedure DAOCRegionChanged;
    procedure DAOCZoneChanged;
    procedure DAOCSetGroundTarget;

    procedure Dirty;
    property DAOCControl: TDAOCConnection read FDControl write SetDControl;
    property PrefsFile: string read FPrefsFile write FPrefsFile;
    property RangeCircles: TRangeCircleList read FRangeCircles;
  end;

var
  frmGLRender: TfrmGLRender;

implementation

uses Unit1;

const
  COL_NAME = 0;
  COL_LEVEL = 1;
  COL_HEALTH = 2;

resourcestring
  S_NO_OGL13 = 'OpenGL 1.3 support required for terrain textures';
  S_CAPTION_SUFFIX = ' -- Press F1 for options';
  
{$R *.dfm}

function RealmColor(ARealm: TDAOCRealm) : TColor;
begin
  case ARealm of
    drNeutral:  Result := clWhite;
    drAlbion:   Result := clRed;
    drMidgard:  Result := $efae00;
    drHibernia: Result := $33cc33;
    else
      Result := clFuchsia;
  end;
end;

function ListDeadRealmColor(ARealm: TDAOCRealm) : TColor;
begin
  case ARealm of
    drNeutral:  Result := clWhite;
    drAlbion:   Result := $ccccff;
    drMidgard:  Result := $ffcccc;
    drHibernia: Result := $ccffcc;
    else
      Result := clFuchsia;
  end;
end;

{ TfrmGLRender }

procedure TfrmGLRender.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
//    ExStyle := ExStyle or WS_EX_TOPMOST;
    WndParent := GetDesktopWindow;
  end;
end;

procedure TfrmGLRender.SetDControl(const Value: TDAOCConnection);
begin
  FDControl := Value;
end;

procedure TfrmGLRender.glMapDraw(Sender: TObject);
var
  dwStartTickCount:  DWORD;
begin
  if not Assigned(FDControl) then
    exit;

  if not FGLInitsCalled then
    GLInits;

  dwStartTickCount := GetTickCount;
  if FInvaderHighlightLastSwap + 1000 < dwStartTickCount then begin
    UpdateFrameStats(dwStartTickCount - FInvaderHighlightLastSwap);
    FDControl.CheckForStaleObjects;
    FInvaderHighlightLastSwap := dwStartTickCount;
    FInvaderHighlight := not FInvaderHighlight;
  end;

  glClear(GL_COLOR_BUFFER_BIT);

  SetupRadarProjectionMatrix;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_LIGHTING);

    { at origin }
  DrawMapElements;
  DrawMobsAndPlayers;
  DrawLineToSelected;
  DrawGroundTarget;

  glTranslatef(FDControl.LocalPlayer.XProjected, FDControl.LocalPlayer.YProjected, 0);

    { at player pos }
  DrawMapRulers;
  DrawRangeCircles;
  DrawPlayerTriangle;

    { in a screen-size ortho }
  SetupScreenProjectionMatrix;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  DrawHUD;
  DrawFrameStats;

  CheckGLError();

  inc(FFrameCount);
  FDirty := false;
end;

procedure TfrmGLRender.glMapInit(Sender: TObject);
const
  lightpos: array[0..3] of GLfloat = (-0.5, 0.0, 1.0, 0.0);
  diffuse: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  ambient: array[0..3] of GLfloat = (0.0, 0.0, 0.0, 1.0);
  material: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
begin
  { BRY: Handle this gracefully at some point }
  if not Load_GL_version_1_3 then
    raise Exception.Create(S_NO_OGL13);

  glClearColor(0, 0, 0, 0);
  glPointSize(3);

  glEnable(GL_CULL_FACE);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);

  glShadeModel(GL_FLAT);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glDisable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);

  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glLightfv(GL_LIGHT0, GL_POSITION, @lightpos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @diffuse);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @ambient);
  glMaterialfv(GL_FRONT, GL_AMBIENT, @material);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @material);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  FGLInitsCalled := false;
  CheckGLError;
end;

procedure TfrmGLRender.glMapResize(Sender: TObject);
begin
  glViewport(0, 0, glMap.Width, glMap.Height);
  CheckGLError;
end;

procedure TfrmGLRender.slideZoomChange(Sender: TObject);
var
  iSize:  integer;
begin
  FRange := slideZoom.Position;

  iSize := FRange div 40;
  if iSize > 300 then
    iSize := 300
  else if iSize < 25 then
    iSize := 25;
  FMobTriangle.Size := iSize;
  
  if Visible then begin
    FMobTriangle.GLCleanup;
    FMobTriangle.GLInitialize;
  end;

  Dirty;
end;

procedure TfrmGLRender.FormShow(Sender: TObject);
begin
  if FPrefsFile <> '' then begin
    FRenderPrefs.LoadSettings(FPrefsFile);
    Left := FRenderPrefs.Left;
    Top := FRenderPrefs.Top;
    Width := FRenderPrefs.Width;
    Height := FRenderPrefs.Height;
    FRange := FRenderPrefs.Range;
    slideZoom.Position := FRange;
    UpdateStayOnTop;
  end
  else
    slideZoomChange(Self);
end;

procedure TfrmGLRender.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmGLRender.CheckGLError;
var
  err:  GLenum;
begin
  err := glGetError();
  if err <> GL_NO_ERROR then
    Log(string(gluErrorString(err)));
end;

procedure TfrmGLRender.DrawMapRulers;
var
  r:      GLfloat;
  s, c:   single;
begin
  if not FRenderPrefs.DrawRulers then
    exit;

  glDisable(GL_LIGHTING);

  r := FRange * 1.5;
  sincos_quick(FDControl.LocalPlayer.Head, s, c);

    { Draw map rulers }
  glColor3f(0.45, 0.45, 0.45);
  glLineWidth(1.0);
  glBegin(GL_LINES);
    glVertex3f(-r, 0, 0);
    glVertex3f(r, 0, 0);
    glVertex3f(0, -r, 0);
    glVertex3f(0, r, 0);

    glVertex3f(0, 0, 0);
    glVertex3f(-s * r, c * r, 0);
  glEnd();
end;

procedure TfrmGLRender.DrawPlayerTriangle;
begin
  glEnable(GL_LIGHTING);
  glRotatef(FDControl.LocalPlayer.Head, 0, 0, 1);

  if FRenderPrefs.ViewFrustum then
    FVisibleRangeRep.GLRender(FRenderBounds);

  glColor3f(1, 1, 0);
  FMobTriangle.GLRender(FRenderBounds);
end;

procedure TfrmGLRender.DrawRangeCircles;
begin
  if not FRenderPrefs.DrawRangeCircles then
    exit;

  glDisable(GL_LIGHTING);
  FRangeCircles.GLRender(FRenderBounds);
end;

procedure TfrmGLRender.Dirty;
begin
  inc(FDirtyCount);
  if not FDirty then begin
    inc(FInvalidateCount);

    FDirty := true;
    glMap.Invalidate;
//    lstObjects.Invalidate;
  end;
end;

procedure TfrmGLRender.DAOCAddObject(AObj: TDAOCObject);
var
  iOldIndex:  integer;
  iPos:       integer;
begin
  if FRenderPrefs.IsObjectInFilter(AObj) then begin
    iPos := FilteredObjectInsert(AObj);
    iOldIndex := lstObjects.ItemIndex;

    SetObjectListRowCount(FFilteredObjects.Count);

      { if we inserted an object before the selected index, update the
        selected index to keep it the same }
    if iPos <= iOldIndex then
      lstObjects.ItemIndex := iOldIndex + 1
    else if iOldIndex <> -1 then
      lstObjects.ItemIndex := iOldIndex;
      
    UpdateObjectCounts;

    if FRenderPrefs.RedrawOnAdd then
      Dirty;
  end;
end;

procedure TfrmGLRender.DAOCDeleteObject(AObj: TDAOCObject);
var
  iOldPos:  integer;
  iPos:     integer;
begin
  if FRenderPrefs.IsObjectInFilter(AObj) then begin
    iPos := FFilteredObjects.Remove(AObj);
    if iPos = -1 then begin
      Log('Removing: ' + AObj.Name + ' not in list');
      exit;
    end;

    UpdateObjectCounts;

    iOldPos := lstObjects.ItemIndex;
    SetObjectListRowCount(FFilteredObjects.Count);
    if iPos <= iOldPos then
      lstObjects.ItemIndex := iOldPos - 1
    else
      lstObjects.ItemIndex := iOldPos;

    if FRenderPrefs.RedrawOnDelete then
      Dirty;
  end;
end;

procedure TfrmGLRender.DAOCUpdateObject(AObj: TDAOCObject);
begin
  InvalidateListObject(AObj);
  if FRenderPrefs.RedrawOnUpdate then
    Dirty;
end;

procedure TfrmGLRender.glMapClick(Sender: TObject);
var
  projmatrix: T16dArray;
  modmatrix:  T16dArray;
  viewport:   TViewPortArray;
  x, y, z:    GLdouble;
  pt:         TPoint;
  pNearest:   TDAOCObject;
begin
  if not FRenderPrefs.TrackMapClick then
    exit;

  SetupRadarProjectionMatrix;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, @modmatrix);
  glGetIntegerv(GL_VIEWPORT, @viewport);

  pt := glMap.ScreenToClient(Mouse.CursorPos);
  gluUnProject(pt.X, glMap.Height - pt.Y, 0, modmatrix, projmatrix, viewport,
    @x, @y, @z);

    { if this is a dungeon, use 3d distance else use 2d }
  if Assigned(FDControl.Zone) and (FDControl.Zone.ZoneType = 2) then
    pNearest := FFilteredObjects.FindNearest3D(round(x), round(y), FDControl.LocalPlayer.Z)
  else
    pNearest := FFilteredObjects.FindNearest2D(round(x), round(y));

  if Assigned(pNearest) then begin
    if FRenderPrefs.TrackInGameSelect then
        { callback will update screen }
      FDControl.SelectedObject := pNearest
    else
      GridSelectObject(pNearest);
    Dirty;
  end;  { if pNearest }
end;

procedure TfrmGLRender.DrawLineToSelected;
var
  pSelected:  TDAOCObject;
begin
  pSelected := FDControl.SelectedObject;
  if Assigned(pSelected) then begin
    glDisable(GL_LIGHTING);

    glColor3f(1.0, 1.0, 1.0);
    glLineWidth(2.0);
    glBegin(GL_LINES);
    glVertex3i(FDControl.LocalPlayer.XProjected, FDControl.LocalPlayer.YProjected, 0);
    if pSelected is TDAOCMovingObject then
      glVertex3i(TDAOCMovingObject(pSelected).XProjected, TDAOCMovingObject(pSelected).YProjected, 0)
    else
      glVertex3i(pSelected.X, pSelected.Y, 0);
    glEnd();
  end;
end;

procedure TfrmGLRender.DrawMobsAndPlayers;
var
  I:    integer;
  clMob:  TColor;
  pObj:   TDAOCObject;
  pMovingObj:  TDAOCMovingObject;
begin
  glEnable(GL_BLEND);

  for I := 0 to FFilteredObjects.Count - 1 do begin
    pObj := FFilteredObjects[I];
    if not pObj.IsInUpdateRange then
      continue;

    if pObj.ObjectClass in [ocUnknown, ocMob, ocPlayer] then begin
      pMovingObj := TDAOCMovingObject(pObj);
      clMob := pObj.GetConColor(FDControl.LocalPlayer.Level);

        { if the mob is on the move, draw a line to its destination }
      if FRenderPrefs.DrawAIDestination then begin
        if (pObj.ObjectClass = ocMob) and Assigned(TDAOCMob(pObj).Target) then
          DrawLineToMobTarget(TDAOCMob(pObj))
        else if (pObj.DestinationX <> 0) and (pObj.DestinationY <> 0) then begin
          glLineWidth(3.0);

          SetGLColorFromTColor(clMob, 0.33);

          glBegin(GL_LINES);
            glVertex3f(pMovingObj.XProjected, pMovingObj.YProjected, 0);
            glVertex3f(pMovingObj.DestinationX, pMovingObj.DestinationY, 0);
          glEnd();
        end;  { if destinaton set }
      end;  { if draw AIDest }

      glPushMatrix();
      glTranslatef(pMovingObj.XProjected, pMovingObj.YProjected, 0);
      glRotatef(pObj.Head, 0, 0, 1);

      if pObj.ObjectClass = ocPlayer then 
        DrawPlayerHighlightRing(TDAOCPlayer(pObj))
      else if FRenderPrefs.DrawTypeTag and (pObj.ObjectClass = ocMob) then
        DrawMobTypeTag(TDAOCMob(pObj));

      if pObj.Stealthed then
        SetGLColorFromTColor(clBlack, 1.0)
      else if pObj.Stale then
        SetGLColorFromTColor(clMob, 0.75)
      else
        glColor3ubv(PGLubyte(@clMob));
      FMobTriangle.GLRender(FRenderBounds);

      glPopMatrix();
    end;  { if a class to draw }

    if pObj.ObjectClass = ocObject then begin
      glPushMatrix();
      glTranslatef(pObj.X, pObj.Y, 0);
      FObjectTriangle.GLRender(FRenderBounds);
      glPopMatrix();
    end;
  end;  { for each object }
end;

procedure TfrmGLRender.FormCreate(Sender: TObject);
var
  rngCircle:  TRangeCircle;
begin
  FRangeCircles := TRangeCircleList.Create;
  rngCircle := TRangeCircle.CreateRange(500, 24);
  rngCircle.Color := clRed;
  FRangeCircles.Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(1500, 24);
  rngCircle.Color := clLime;
  FRangeCircles.Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(6000, 40);
  rngCircle.Color := clSilver;
  FRangeCircles.Add(rngCircle);

  FMapElementsListList := TVectorMapElementListList.Create;
  FMapElementsListList.VectorMapDir := ExtractFilePath(ParamStr(0)) + 'maps\';
  FMapTexturesListList := TTextureMapElementListList.Create;
  FMapTexturesListList.TextureMapDir := ExtractFilePath(ParamStr(0)) + 'maps\dds\';
  FMobTriangle := T3DArrowHead.Create;
  FObjectTriangle := T3DPyramid.Create;
  FGroundTarget := TGLBullsEye.Create;
  FVisibleRangeRep := TGLFlatViewFrustum.Create;
  FFilteredObjects := TDAOCObjectList.Create(false);
  FRenderPrefs := TRenderPreferences.Create;
  FRenderPrefs.OnObjectFilterChanged := RENDERPrefsObjectFilterChanged;
  UpdateObjectCounts;
end;

procedure TfrmGLRender.FormDestroy(Sender: TObject);
begin
  GLCleanups;
  FFilteredObjects.Free;
  FMobTriangle.Free;
  FGroundTarget.Free;
  FVisibleRangeRep.Free;
  FObjectTriangle.Free;
  FMapElementsListList.Free;
  FMapTexturesListList.Free;
  FRangeCircles.Free;
  FRenderPrefs.Free;
end;

procedure TfrmGLRender.GLCleanups;
begin
  FMobTriangle.GLCleanup;
  FObjectTriangle.GLCleanup;
  FRangeCircles.GLCleanup;
  FMapElementsListList.GLCleanup;
  FMapTexturesListList.GLCleanup;
  FGroundTarget.GLCleanup;
  FVisibleRangeRep.GLCleanup;

  FGLInitsCalled := false;
end;

procedure TfrmGLRender.GLInits;
begin
  FGLInitsCalled := true;
  FMobTriangle.GLInitialize;
  FObjectTriangle.GLInitialize;
  FRangeCircles.GLInitialize;
  FMapElementsListList.GLInitialize;
  FMapTexturesListList.GLInitialize;
  FGroundTarget.GLInitialize;
  FVisibleRangeRep.GLInitialize;
  
  CheckGLError;
end;

procedure TfrmGLRender.DrawMapElements;
begin
  glPushAttrib(GL_ENABLE_BIT);

  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDisable(GL_BLEND);
  glLineWidth(1.0);

  if FRenderPrefs.DrawMapTexture then
    FMapTexturesListList.GLRender(FRenderBounds);
  if FRenderPrefs.DrawMapVector then
    FMapElementsListList.GLRender(FRenderBounds);

  glPopAttrib();
end;

procedure TfrmGLRender.DAOCZoneChanged;
begin
  if not Assigned(FDControl.Zone) then
    exit;

  { BRY:  We really need to select the GL context here, but I don't because
    glWindow doesn't have a function to activate its context and we'll just
    see if this works }
  ReloadMapElementsAndTextures;
end;

procedure TfrmGLRender.DAOCRegionChanged;
begin
  Caption := FDControl.LocalPlayer.Name + S_CAPTION_SUFFIX;
  DAOCSetGroundTarget;
end;

procedure TfrmGLRender.GridSelectObject(ADAOCObject: TDAOCObject);
var
  I:    integer;
begin
  I := FFilteredObjects.IndexOf(ADAOCObject);

  if I <> lstObjects.ItemIndex then
    lstObjects.ItemIndex := I;
end;

procedure TfrmGLRender.DAOCSelectedObjectChanged(AObj: TDAOCObject);
begin
  if FRenderPrefs.TrackInGameSelect then begin
    GridSelectObject(AObj);
    Dirty;
  end;
end;

procedure TfrmGLRender.DrawPlayerHighlightRing(ADAOCObject: TDAOCPlayer);
var
  cl:     TColor;
  fSize:  GLfloat;
begin
  cl := RealmColor(ADAOCObject.Realm);

  if ADAOCObject.IsInGroup or ADAOCObject.IsInGuild then
    fSize :=  FMobTriangle.Size * 1.75
  else
    fSize :=  FMobTriangle.Size * 1.33;
    
  if ADAOCObject.IsDead then
    SetGLColorFromTColorDarkened(cl, 1, 0.25)
  else if ADAOCObject.Realm <> FDControl.LocalPlayer.Realm then
    if FInvaderHighlight then
      SetGLColorFromTColor(cl, 1)
    else
      SetGLColorFromTColor(cl, 0.6)
  else
    SetGLColorFromTColor(cl, 1);

  glShadeModel(GL_SMOOTH);
  
  glBegin(GL_TRIANGLES);
    glNormal3f(0, 0, 1);
    glVertex3F(0, 2 * fSize, 0);
    if ADAOCObject.IsInGroup then
      SetGLColorFromTColor($3399ff, 1)
    else if ADAOCObject.IsInGuild then
      SetGLColorFromTColor($666600, 1);
    glVertex3f(fSize, -fSize, 0);
    glVertex3f(-fSize, -fSize, 0);
  glEnd();

  glShadeModel(GL_FLAT);
end;

procedure TfrmGLRender.tmrMinFPSTimer(Sender: TObject);
begin
  if FRenderPrefs.RedrawOnTimer then
    Dirty;
end;

procedure TfrmGLRender.DrawHUD;
const
  TEXT_COLOR: array[0..3] of GLfloat = (0.35, 0.80, 1, 0.75);
var
  rastery:  integer;
  pMob: TDAOCObject;
  s:    string;

  procedure WriteMobNameCon(const AName: string);
  begin
    with TDAOCMovingObject(pMob) do begin
        { white background for the name }
      glColor3f(0.4, 0.4, 0.4);
      WriteGLUTTextH12(4+1, rastery-1, AName);
        { con color for name }
      SetGLColorFromTColor(GetConColor(FDControl.LocalPlayer.Level), 1);
      rastery := WriteGLUTTextH12(4, rastery, AName);
    end;
  end;

  procedure WriteMobLevelHealth;
  var
    s:  string;
  begin
    with TDAOCMovingObject(pMob) do begin
      s := 'Level ' + IntToStr(Level);
      if HitPoints <> 100 then
        if IsDead then
          s := s + ' (dead)'
        else
          s := s + ' (' + IntToStr(HitPoints) + '%)';
      rastery := WriteGLUTTextH12(4, rastery, s);
    end;
  end;

begin
  if not FRenderPrefs.DrawHUD then
    exit;

  pMob := FDControl.SelectedObject;
  if not (Assigned(pMob) and Assigned(glutBitmapCharacter)) then
    exit;

  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  rastery := glMap.ClientHeight;

  glColor4f(0, 0, 0, 0.5);
  glBegin(GL_QUADS);
    glVertex2i(0, rastery);
    glVertex2i(0, rastery - 58);
    glVertex2i(145, rastery - 58);
    glVertex2i(145, rastery);
  glEnd;

  case pMob.ObjectClass of
    ocObject:
      begin
        glColor3f(0.9, 0.9, 0.9);
        rastery := WriteGLUTTextH12(4, rastery, pMob.Name);
        glColor4fv(@TEXT_COLOR);
      end;

    ocMob:
      with TDAOCMob(pMob) do begin
        WriteMobNameCon(Name);
        glColor4fv(@TEXT_COLOR);
        if TypeTag <> '' then
          rastery := WriteGLUTTextH12(4, rastery, TypeTag);
        WriteMobLevelHealth;
      end;  { ocMob }

    ocPlayer:
      with TDAOCPlayer(pMob) do begin
        WriteMobNameCon(FullName);
        glColor4fv(@TEXT_COLOR);
        if Guild <> '' then
          rastery := WriteGLUTTextH12(4, rastery, '<' + Guild + '>');
        WriteMobLevelHealth;
      end;  { ocPlayer }

    else
        glColor4fv(@TEXT_COLOR);
  end;    { case class }

  s := 'Dist: ' + FormatFloat('0', pMob.Distance3D(FDControl.LocalPlayer));
  if (pMob is TDAOCMovingObject) and (TDAOCMovingObject(pMob).Speed <> 0) then
    s := s + '  Speed: ' + TDAOCMovingObject(pMob).SpeedString;
  rastery := WriteGLUTTextH12(4, rastery, s);
end;

procedure TfrmGLRender.SetupRadarProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if FRenderPrefs.RotateMapWithPlayer then
    glRotatef(FDControl.LocalPlayer.Head - 180, 0, 0, 1)
  else if Assigned(FDControl.Zone) and (FDControl.Zone.Rotate > 0) then
    glRotatef(FDControl.Zone.Rotate, 0, 0, 1);
  glRotatef(180, 1, 0, 0);

  FRenderBounds.Left := FDControl.LocalPlayer.XProjected - FRange;
  inc(FRenderBounds.Left, FMapToPlayerOffset.X);
  FRenderBounds.Right := FRenderBounds.Left + (integer(FRange) * 2);

  FRenderBounds.Top := FDControl.LocalPlayer.YProjected - FRange;
  inc(FRenderBounds.Top, FMapToPlayerOffset.Y);
  FRenderBounds.Bottom := FRenderBounds.Top + (integer(FRange) * 2);

  with FRenderBounds do
      { render bounds are in DAOC (top down) coordinates, so flip the top
        and bottom when we create our ortho }
    glOrtho(Left, Right, Top, Bottom, 1, -300);
end;

procedure TfrmGLRender.SetupScreenProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, glMap.ClientWidth, 0, glMap.ClientHeight, 1, -1);
end;

procedure TfrmGLRender.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_HOME:
      begin
        FMapToPlayerOffset.X := 0;
        FMapToPlayerOffset.Y := 0;
        Key := 0;
      end;
    VK_LEFT:
      begin
        dec(FMapToPlayerOffset.X, FRange div 10);
        Key := 0;
      end;
    VK_UP:
      begin
        dec(FMapToPlayerOffset.Y, FRange div 10);
        Key := 0;
      end;
    VK_RIGHT:
      begin
        inc(FMapToPlayerOffset.X, FRange div 10);
        Key := 0;
      end;
    VK_DOWN:
      begin
        inc(FMapToPlayerOffset.Y, FRange div 10);
        Key := 0;
      end;
    VK_F1:
      begin
        DoPrefsDialog;
        Key := 0;
      end;
  end;
end;

procedure TfrmGLRender.glMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  glMap.SetFocus;
end;

procedure TfrmGLRender.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  slideZoom.Position := slideZoom.Position + slideZoom.Frequency;
  Handled := true;
end;

procedure TfrmGLRender.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  slideZoom.Position := slideZoom.Position - slideZoom.Frequency;
  Handled := true;
end;

procedure TfrmGLRender.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    'b', 'B':
      begin
        FRenderPrefs.DrawMapTexture := not FRenderPrefs.DrawMapTexture;
        Dirty;
        Key := #0;
      end;
    'c', 'C':
      begin
        FRenderPrefs.DrawRangeCircles := not FRenderPrefs.DrawRangeCircles;
        Dirty;
        Key := #0;
      end;
    'd', 'D':
      begin
        FRenderPrefs.DrawAIDestination := not FRenderPrefs.DrawAIDestination;
        Dirty;
        Key := #0;
      end;
    'h', 'H':
      begin
        FRenderPrefs.DrawHUD := not FRenderPrefs.DrawHUD;
        Dirty;
        Key := #0;
      end;
    'm', 'M':
      begin
        FRenderPrefs.XORObjectFilter(ocMob);
        Key := #0;
      end;
    'o', 'O':
      begin
        FRenderPrefs.XORObjectFilter(ocObject);
        Key := #0;
      end;
    'p', 'P':
      begin
        FRenderPrefs.XORObjectFilter(ocPlayer);
        Key := #0;
      end;
    'r', 'R':
      begin
        FRenderPrefs.DrawRulers := not FRenderPrefs.DrawRulers;
        Dirty;
        Key := #0;
      end;
    't', 'T':
      begin
        FRenderPrefs.StayOnTop := not FRenderPrefs.StayOnTop;
        UpdateStayOnTop;
        Key := #0;
      end;
    'u', 'U':
      begin
        FRenderPrefs.XORObjectFilter(ocUnknown);
        RefreshFilteredList;
        Key := #0;
      end;
    'v', 'V':
      begin
        FRenderPrefs.DrawMapVector := not FRenderPrefs.DrawMapVector;
        Dirty;
        Key := #0;
      end;
    'y', 'Y':
      begin
        FRenderPrefs.DrawTypeTag := not FRenderPrefs.DrawTypeTag;
        Dirty;
        Key := #0;
      end;
  end;
end;

procedure TfrmGLRender.RefreshFilteredList;
var
  I:    integer;
begin
  FFilteredObjects.Clear;
  for I := 0 to FDControl.DAOCObjects.Count - 1 do
    if FRenderPrefs.IsObjectInFilter(FDControl.DAOCObjects[I]) then
      FilteredObjectInsert(FDControl.DAOCObjects[I]);

  UpdateObjectCounts;
  SetObjectListRowCount(FFilteredObjects.Count);
  Dirty;
end;

procedure TfrmGLRender.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FPrefsFile <> '' then begin
    FRenderPrefs.Left := Left;
    FRenderPrefs.Top := Top;
    FRenderPrefs.Width := Width;
    FRenderPrefs.Height := Height;
    FRenderPrefs.Range := FRange;
    FRenderPrefs.SaveSettings(FPrefsFile);
  end;
end;

function TfrmGLRender.FilteredObjectInsert(ADAOCObject: TDAOCObject) : integer;
const
  OBJECT_ORDER: array[TDAOCObjectClass] of integer = (4, 3, 2, 1, 0);
var
  I:   integer;
  function CompareObjectClasses(A, B: TDAOCObjectClass) : integer;
  begin
    if OBJECT_ORDER[A] < OBJECT_ORDER[B] then
      Result := -1
    else if OBJECT_ORDER[A] > OBJECT_ORDER[B] then
      Result := 1
    else
      Result := 0;
  end;
begin
  for I := 0 to FFilteredObjects.Count - 1 do
    if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) < 0 then begin
      FFilteredObjects.Insert(I, ADAOCObject);
      Result := I;
      exit;
    end
    else if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) = 0 then begin
      if AnsiCompareText(ADAOCObject.Name, FFilteredObjects[I].Name) < 0 then begin
        FFilteredObjects.Insert(I, ADAOCObject);
        Result := I;
        exit;
      end
    end;

  Result := FFilteredObjects.Add(ADAOCObject);
end;

procedure TfrmGLRender.UpdateObjectCounts;
var
  I:    integer;
  counts:  array[0..1, TDAOCRealm] of integer;
  pMob: TDAOCObject;
begin
  FillChar(counts, sizeof(counts), 0);
  
  for I := 0 to FFilteredObjects.Count - 1 do begin
    pMob := FFilteredObjects[I];
    if pMob is TDAOCMovingObject then begin
      inc(counts[0, pMob.Realm]);
      if TDAOCMovingObject(pMob).IsAlive then
        inc(counts[1, pMob.Realm]);
    end;
  end;

  lblObjCounts.Caption := Format(
    'Albs: %d (%d)        Mids: %d (%d)'#13'Hibs: %d (%d)        Mobs: %d', [
    counts[1, drAlbion], counts[0, drAlbion],
    counts[1, drMidgard], counts[0, drMidgard],
    counts[1, drHibernia], counts[0, drHibernia],
    counts[0, drNeutral]
  ]);
end;

procedure TfrmGLRender.DrawMobTypeTag(AMob: TDAOCMob);
begin
  if AMob.TypeTag <> '' then begin
    glDisable(GL_LIGHTING);
    WriteGLUTTextH10(30, 30, AMob.TypeTag);
    glEnable(GL_LIGHTING);
  end;
end;

procedure TfrmGLRender.SetObjectListRowCount(ACount: integer);
begin
  lstObjects.Count := FFilteredObjects.Count;
end;

procedure TfrmGLRender.lstObjectsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  pMob:   TDAOCObject;
  R,G,B:  byte;
  sText:  string;
  cl:     TColor;
begin
  if Index < FFilteredObjects.Count then
    with lstObjects.Canvas do begin
      pMob := FFilteredObjects[Index];

        { objects are gray on white }
      if pMob.ObjectClass = ocObject then begin
        Font.Color := clGray;
        Brush.Color := clWhite;
      end  { object }

        { mobs are con color on white }
      else if pMob.Realm = drNeutral then begin
        cl := pMob.GetConColor(FDControl.LocalPlayer.Level);
        R := GetRValue(cl) shr 1;
        G := GetGValue(cl) shr 1;
        B := GetBValue(cl) shr 1;

        Font.Color := RGB(R, G, B);
        Brush.Color := clWhite;
      end

        { else (should be just players) black on realm color }
      else begin
        Font.Color := clBlack;
        if TDAOCPlayer(pMob).IsAlive then
          cl := RealmColor(pMob.Realm)
        else
          cl := ListDeadRealmColor(pMob.Realm);
        Brush.Color := cl;
      end;

      FillRect(Rect);

        { NAME }
      if (pMob is TDAOCPlayer) and (TDAOCPlayer(pMob).CharacterClass <> ccUnknown) then
        if pMob.Realm <> FDControl.LocalPlayer.Realm then
          sText := pMob.Name + ' ' + DAOCCharacterClassToStr(TDAOCPlayer(pMob).CharacterClass)
        else
          sText := pMob.Name + ' (' + DAOCCharacterClassToStr(TDAOCPlayer(pMob).CharacterClass) + ')'
      else
        sText := pMob.Name;
      TextOut(Rect.Left + 3, Rect.Top + 2, sText);

        { LEVEL }
      sText := IntToStr(pMob.Level);
      TextOut(Rect.Left + 150, Rect.Top + 2, sText);

        { HEALTH }
      if pMob is TDAOCMovingObject then
        sText := IntToStr(TDAOCMovingObject(pMob).HitPoints)
      else
        sText := '';
      TextOut(Rect.Left + 175, Rect.Top + 2, sText);

        { If selected, draw the focus rect even if we're not focused }
      if (odSelected in State) and not (odFocused in State) then
        DrawFocusRect(Rect);
  end  { data row / with }

    { blank row }
  else begin
    lstObjects.Canvas.Brush.Color := clWhite;
    lstObjects.Canvas.FillRect(Rect);
  end;
end;

procedure TfrmGLRender.lstObjectsClick(Sender: TObject);
begin
  if lstObjects.ItemIndex < FFilteredObjects.Count then
    FDControl.SelectedObject := FFilteredObjects[lstObjects.ItemIndex];
end;

procedure TfrmGLRender.RENDERPrefsObjectFilterChanged(Sender: TObject);
begin
  RefreshFilteredList;
end;

procedure TfrmGLRender.DrawGroundTarget;
begin
  FGroundTarget.GLRender(FRenderBounds);
end;

procedure TfrmGLRender.DrawFrameStats;
begin
  if not FRenderPrefs.DrawFrameStats then
    exit;

  if FFrameStats <> '' then begin
    glDisable(GL_LIGHTING);
    glColor3f(1, 1, 0);
    WriteGLUTTextH10(3, 15, FFrameStats);
  end;
end;

procedure TfrmGLRender.UpdateFrameStats(ATime: integer);
begin
  FFrameStats := Format('%d frames for %d dirty / %d invalidate in %d msec', [
    FFrameCount, FDirtyCount, FInvalidateCount, ATime]);

  FFrameCount := 0;
  FDirtyCount := 0;
  FInvalidateCount := 0;
end;

procedure TfrmGLRender.DAOCSetGroundTarget;
begin
  with FDControl.GroundTarget do
    FGroundTarget.Assign(X, Y);
end;

procedure TfrmGLRender.InvalidateListObject(AObj: TDAOCObject);
var
  I:    integer;
  R:    TRect;
begin
  I := FFilteredObjects.IndexOf(AObj);
  if I <> -1 then begin
    R := lstObjects.ItemRect(I);
    InvalidateRect(lstObjects.Handle, @R, false);
  end;
end;

procedure TfrmGLRender.UpdateStayOnTop;
var
  dwStyle:  DWORD;
begin
  dwStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if FRenderPrefs.StayOnTop then begin
    dwStyle := dwStyle or WS_EX_TOPMOST;
    SetWindowLong(Handle, GWL_EXSTYLE, dwStyle);
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end
  else begin
    dwStyle := dwStyle and not WS_EX_TOPMOST;
    SetWindowLong(Handle, GWL_EXSTYLE, dwStyle);
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TfrmGLRender.ReloadMapElementsAndTextures;
begin
  FMapElementsListList.LoadForZone(FDControl.Zone, FRenderPrefs.AdjacentZones);
  FMapTexturesListList.LoadForZone(FDControl.Zone, FRenderPrefs.AdjacentZones);
end;

procedure TfrmGLRender.DoPrefsDialog;
var
  tmpPrefs:   TRenderPreferences;
begin
  tmpPrefs := FRenderPrefs.Clone;
  if TfrmRenderPrefs.Execute(Self, FRenderPrefs) then begin
    if tmpPrefs.AdjacentZones <> FRenderPrefs.AdjacentZones then
      ReloadMapElementsAndTextures;
    UpdateStayOnTop;
  end
  else begin
    FRenderPrefs.Free;
    FRenderPrefs := tmpPrefs;
    FRenderPrefs.OnObjectFilterChanged := RENDERPrefsObjectFilterChanged;
  end;
end;

procedure TfrmGLRender.DrawLineToMobTarget(AMob: TDAOCMob);
begin
  glLineWidth(3.0);
  glColor3f(1, 0, 0);
  glBegin(GL_LINES);
    glVertex3f(AMob.XProjected, AMob.YProjected, 0);
    glVertex3f(AMob.Target.XProjected, AMob.Target.YProjected, 0);
  glEnd();
end;

end.


unit GLRender;

(****************************************************************************
**
** Copyright (C) 2004 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
{$IFDEF LINUX}
  QForms, QControls, QGraphics,
{$ELSE}
  Windows, Messages, Controls, Forms, Graphics, MMSystem,
  StdCtrls, ExtCtrls, ComCtrls, Dialogs,
{$ENDIF !LINUX}
  SysUtils, Classes, glWindow, GL, GLU, GLext, DAOCConnection, DAOCConnectionList,
  DAOCObjs, GLRenderObjects, MapElementList, DAOCRegion, RenderPrefs, DAOCClasses,
  QuickSinCos, BackgroundHTTP, Menus, ActnList, GlutFonts, jpeg;

type
  { A simple list box that you can prevent drawing }
  TLockableListBox = class(TListBox)
  private
    FLocked: boolean;
    procedure SetLocked(const Value: boolean);
  protected
{$IFDEF MSWINDOWS}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{$ENDIF}
{$IFDEF LINUX}
    function WidgetFlags: integer; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

    property Locked: boolean read FLocked write SetLocked;
  end;

  TfrmGLRender = class(TForm)
    slideZoom: TTrackBar;
    tmrMinFPS: TTimer;
    pnlMap: TPanel;
    pnlLeft: TPanel;
    lblObjCounts: TLabel;
    pnlGridHeader: TPanel;
    pumRadar: TPopupMenu;
    mniShowZoneInfo: TMenuItem;
    pnlInventory: TPanel;
    lblInventory: TLabel;
    lblInventoryHeader: TLabel;
    mniForceContextCurrent: TMenuItem;
    atlRadarKeys: TActionList;
    atnShowInventory: TAction;
    atnInvaderWarningToggle: TAction;
    atnDrawTextureToggle: TAction;
    atnDrawRangeCirclesToggle: TAction;
    atnDrawAIDestinationToggle: TAction;
    atnDrawFriendlyPlayersToggle: TAction;
    atnDrawGridToggle: TAction;
    atnDrawHUDToggle: TAction;
    atnAddPushPin: TAction;
    atnDrawMobsToggle: TAction;
    atnDrawObjectsToggle: TAction;
    atnDrawPlayersToggle: TAction;
    atnDrawRulersToggle: TAction;
    atnStayOnTopToggle: TAction;
    atnDrawUnknownToggle: TAction;
    atnDrawTypeTagToggle: TAction;
    atnNextConnection: TAction;
    atnPreviousConnection: TAction;
    atnZoomIn: TAction;
    atnZoomOut: TAction;
    atnRecenterMap: TAction;
    atnScrollLeft: TAction;
    atnScrollRight: TAction;
    atnScrollUp: TAction;
    atnScrollDown: TAction;
    atnShowPrefsDialog: TAction;
    atnShowHideMobList: TAction;
    atnDrawVectorToggle: TAction;
    atnScreenShot: TAction;
    procedure glMapDraw(Sender: TObject);
    procedure glMapInit(Sender: TObject);
    procedure glMapResize(Sender: TObject);
    procedure slideZoomChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure glMapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrMinFPSTimer(Sender: TObject);
    procedure glMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstObjectsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure glMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MobListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlLeftEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure mniShowZoneInfoClick(Sender: TObject);
    procedure pumRadarPopup(Sender: TObject);
    procedure mniForceContextCurrentClick(Sender: TObject);
    procedure atnShowInventoryExecute(Sender: TObject);
    procedure atnInvaderWarningToggleExecute(Sender: TObject);
    procedure atnDrawTextureToggleExecute(Sender: TObject);
    procedure atnDrawRangeCirclesToggleExecute(Sender: TObject);
    procedure atnDrawAIDestinationToggleExecute(Sender: TObject);
    procedure atnDrawFriendlyPlayersToggleExecute(Sender: TObject);
    procedure atnDrawGridToggleExecute(Sender: TObject);
    procedure atnDrawHUDToggleExecute(Sender: TObject);
    procedure atnAddPushPinExecute(Sender: TObject);
    procedure atnDrawMobsToggleExecute(Sender: TObject);
    procedure atnDrawObjectsToggleExecute(Sender: TObject);
    procedure atnDrawPlayersToggleExecute(Sender: TObject);
    procedure atnDrawRulersToggleExecute(Sender: TObject);
    procedure atnStayOnTopToggleExecute(Sender: TObject);
    procedure atnDrawUnknownToggleExecute(Sender: TObject);
    procedure atnDrawTypeTagToggleExecute(Sender: TObject);
    procedure atnDrawVectorToggleExecute(Sender: TObject);
    procedure atnNextConnectionExecute(Sender: TObject);
    procedure atnPreviousConnectionExecute(Sender: TObject);
    procedure atnZoomInExecute(Sender: TObject);
    procedure atnZoomOutExecute(Sender: TObject);
    procedure atnRecenterMapExecute(Sender: TObject);
    procedure atnScrollLeftExecute(Sender: TObject);
    procedure atnScrollRightExecute(Sender: TObject);
    procedure atnScrollUpExecute(Sender: TObject);
    procedure atnScrollDownExecute(Sender: TObject);
    procedure atnShowPrefsDialogExecute(Sender: TObject);
    procedure atnShowHideMobListExecute(Sender: TObject);
    procedure atnScreenShotExecute(Sender: TObject);
  private
    glMap:     TglWindow;
    FCurrConn: TDAOCConnection;
    FRange:         Cardinal;
    FRenderBounds:  TRect;
    FGLInitsCalled:       boolean;
    FDirty:   boolean;
    FInvaderHighlight:    boolean;
    FInvaderHighlightLastSwap:  Cardinal;
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
    FZoneName:      string;
    FLastInvaderWarningTicks:   Cardinal;
    FLastAlertIntervalTicks:    Cardinal;
    FBoat:    TGLBoat;
    FMouseLocX:   Cardinal;
    FMouseLocY:   Cardinal;
    FHTTPFetch:   TBackgroundHTTPManager;
    lstObjects:   TLockableListBox;
    FPushPins:    TVectorMapElementList;
    FUnknownStealther:  TGLUnknownStealther;
    FObjectHighlight:   TGLObjectHighlight;
    FPrescienceNode:    TGLPrescienceNode;
    FBasePath:    string;
    //FHudConFlag:  TGLHudConFlag;
    FDAOCConnectionList: TDAOCConnectionList;
    FPrefsLoaded: boolean;

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
    procedure DrawTargetHUD;
    procedure DrawLocalPlayerHUD;
    procedure DrawMobTypeTag(AMob: TDAOCMob);
    procedure DrawGroundTarget;
    procedure DrawFrameStats;
    procedure DrawStealtherAlert;
    procedure DrawLineToMobTarget(AMob: TDAOCMob);
    procedure DrawAIDestination(AObj: TDAOCObject; AColor: TColor);
    procedure DrawGrid;
    procedure DrawMouseTooltip;
    procedure DrawUnknownStealthers;
    procedure DrawNoConnection;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure Log(const s: string);
    procedure RefreshFilteredList;
    procedure GridSelectObject(ADAOCObject: TDAOCObject);
    function FilteredObjectInsert(ADAOCObject: TDAOCObject) : integer;
    function FilteredObjectInsertByName(ADAOCObject: TDAOCObject) : integer;
    function FilteredObjectInsertByDistance(ADAOCObject: TDAOCObject) : integer;
    procedure UpdateObjectCounts;
    function SetObjectListRowCount(ACount: integer) : boolean;
    procedure RENDERPrefsObjectFilterChanged(Sender: TObject);
    procedure RENDERPrefsMobListOptionChanged(Sender: TObject);
    procedure RENDERPrefsMobTriangleSizeChanged(Sender: TObject);
    procedure RENDERPrefsMinFPSChanged(Sender: TObject);
    procedure UpdateFrameStats(ATime: integer);
    procedure InvalidateListObject(AObj: TDAOCObject);
    procedure UpdateStayOnTop;
    procedure ReloadMapElementsAndTextures;
    procedure DoPrefsDialog;
    function PlayerMobListText(AMob: TDAOCPlayer) : string;
    procedure MapUnproject(var X, Y: Cardinal; ANeedMVPSetup: boolean);
    function CompareObjectClasses(A, B: TDAOCObjectClass): integer;
    procedure UpdateMapURLs;
    procedure LoadRegionPushpins;
    procedure AddPushPin;
    procedure SetSmoothingOpts;
    procedure CreateGLWindow;
    procedure CreateObjectListBox;
    procedure AdjustMobTriangleSize;
    procedure AdjustMinFPSTimer;
    function ZDeltaStr(AObj: TDAOCObject; AVerbose: boolean) : string;
    procedure CheckMouseOverUnproject;
    procedure SetDAOCConnectionList(const Value: TDAOCConnectionList);
    procedure PreviousConnection;
    procedure NextConnection;
    procedure SetCurrentConnection(AConn: TDAOCConnection);
    procedure AutoSelectConnection;
    procedure UpdateCaption;
    procedure UpdateInventoryPanel(AForce: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DumpMVM(const where: string);
  public
    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCAddObject(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCDeleteObject(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCUpdateObject(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCSelectedObjectChanged(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCRegionChanged(Sender: TObject);
    procedure DAOCZoneChanged(Sender: TObject);
    procedure DAOCSetGroundTarget(Sender: TObject);
    procedure DAOCCharacterLogin(Sender: TObject);
    procedure DAOCPlayerPosUpdate(Sender: TObject);
    procedure DAOCUnknownStealther(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCDoorPositionUpdate(Sender: TObject; ADoor: TDAOCObject);
    procedure DAOCMobInventoryChanged(Sender: TObject; AObj: TDAOCMovingObject);

    procedure Dirty;
    property DAOCConnectionList: TDAOCConnectionList read FDAOCConnectionList write SetDAOCConnectionList;
    property PrefsFile: string read FPrefsFile write FPrefsFile;
    property RangeCircles: TRangeCircleList read FRangeCircles;
  end;

var
  frmGLRender: TfrmGLRender;

implementation

uses Unit1, GlobalTickCounter, AddPushPin, Intersections;

const
  COL_NAME = 0;
  COL_LEVEL = 1;
  COL_HEALTH = 2;

resourcestring
  S_CAPTION_SUFFIX = ' -- Press F1 for options';

{$R *.dfm}

function RealmColor(ARealm: TDAOCRealm) : TColor;
begin
  case ARealm of
    drNeutral:  Result := clSilver;
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

function min(a, b: integer) : integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function GetGLVersion : string;
begin
    { must have a current GL context active this function is called }
  Result := string(PChar(glGetString(GL_VERSION)));
end;

function GetGLVendor : string;
begin
    { must have a current GL context active this function is called }
  Result := string(PChar(glGetString(GL_VENDOR)));
end;

{ TLockableListBox }

constructor TLockableListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
end;

procedure TLockableListBox.SetLocked(const Value: boolean);
begin
    { if we're unlocking, invalidate us }
  if FLocked and not Value then
    Invalidate;
  FLocked := Value;
end;

{$IFDEF LINUX}
function TLockableListBox.WidgetFlags: integer;
begin
  Result := inherited WidgetFlags or integer(WidgetFlags_WRepaintNoErase) or
    integer(WidgetFlags_WResizeNoErase);
end;
{$ENDIF LINUX}

{$IFDEF MSWINDOWS}
procedure TLockableListBox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
    { redraw the background only if our list doesn't fill the whole area }
  if not FLocked and ((Count * ItemHeight) < Height) then
    inherited
  else
    Message.Result := 1;
end;

procedure TLockableListBox.WMPaint(var Message: TWMPaint);
begin
  if not FLocked then
    inherited;
end;
{$ENDIF MSWINDOWS}

{ TfrmGLRender }

procedure TfrmGLRender.DumpMVM(const where: string);
var
  modmatrix:  T16dArray;
begin
exit;
  glGetDoublev(GL_MODELVIEW_MATRIX, @modmatrix);
  Log(Format('%s: %f %f %f %f, %f %f %f %f, %f %f %f %f, %f %f %f %f',
    [where, modmatrix[0], modmatrix[1], modmatrix[2], modmatrix[3],
    modmatrix[4], modmatrix[5], modmatrix[6], modmatrix[7],
    modmatrix[8], modmatrix[9], modmatrix[10], modmatrix[11],
    modmatrix[12], modmatrix[13], modmatrix[14], modmatrix[15]]));
end;

procedure TfrmGLRender.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
//    ExStyle := ExStyle or WS_EX_TOPMOST;
    WndParent := GetDesktopWindow;
  end;
end;

procedure TfrmGLRender.glMapDraw(Sender: TObject);
const
  lightpos: array[0..3] of GLfloat = (-0.5, 0.0, 1.0, 0.0);
var
  dwStartTickCount:  Cardinal;
begin
    { this is a shortcut, since it will not take effect until the next frame }
  glMap.AutoMakeCurrent := FRenderPrefs.AutoMakeCurrent;
  
  if not FGLInitsCalled then
    GLInits;

  if not Assigned(FCurrConn) then begin
    DrawNoConnection;
    FDirty := false;
    exit;
  end;

  try
    dwStartTickCount := GetTickCount;
    if FInvaderHighlightLastSwap + 1000 < dwStartTickCount then begin
      UpdateFrameStats(dwStartTickCount - FInvaderHighlightLastSwap);
      FCurrConn.CheckForStaleObjects;
      FInvaderHighlightLastSwap := dwStartTickCount;
      FInvaderHighlight := not FInvaderHighlight;
    end;

    glClear(GL_COLOR_BUFFER_BIT);

    SetupRadarProjectionMatrix;
    glLightfv(GL_LIGHT0, GL_POSITION, @lightpos);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    SetSmoothingOpts;

      { at origin }
    CheckMouseOverUnproject;
    DrawMapElements;
    DrawGrid;
    DrawUnknownStealthers;
    DrawMobsAndPlayers;
    DrawLineToSelected;
    DrawGroundTarget;

    glTranslatef(FCurrConn.LocalPlayer.XProjected, FCurrConn.LocalPlayer.YProjected, 0);

      { at player pos }
    DrawMapRulers;
    DrawRangeCircles;
    DrawPlayerTriangle;

      { in a screen-size ortho }
    SetupScreenProjectionMatrix;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    DrawTargetHUD;
    DrawLocalPlayerHUD;
    DrawFrameStats;
    DrawMouseTooltip;
    DrawStealtherAlert;

    glFlush();
    CheckGLError();

    inc(FFrameCount);
    FDirty := false;
  except
    on e: Exception do
      Log('RenderError: ' + e.Message);
  end;
end;

procedure TfrmGLRender.glMapInit(Sender: TObject);
const
  diffuse: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  ambient: array[0..3] of GLfloat = (0.0, 0.0, 0.0, 1.0);
  material: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
begin
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
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @diffuse);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @ambient);
  glMaterialfv(GL_FRONT, GL_AMBIENT, @material);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @material);

  FGLInitsCalled := false;
  CheckGLError;
end;

procedure TfrmGLRender.glMapResize(Sender: TObject);
begin
  glViewport(0, 0, glMap.Width, glMap.Height);
  CheckGLError;
end;

procedure TfrmGLRender.slideZoomChange(Sender: TObject);
begin
  FRange := slideZoom.Position;
  if FRenderPrefs.ScaleMobTriangle then
    AdjustMobTriangleSize;

  Dirty;
end;

procedure TfrmGLRender.FormShow(Sender: TObject);
begin
  LoadSettings;
  UpdateStayOnTop;
  UpdateInventoryPanel(false);
  AutoSelectConnection;
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
    Log(string(PChar(gluErrorString(err))));
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
  sincos_quick(FCurrConn.LocalPlayer.Head, s, c);

    { Draw map rulers }
  glLineWidth(1.0);
  glBegin(GL_LINES);
    glColor3f(0.45, 0.45, 0.45);
    glVertex3f(-r, 0, 0);
    glVertex3f(r, 0, 0);
    glVertex3f(0, -r, 0);
    glVertex3f(0, r, 0);

    glColor3f(0.6, 0.6, 0.6);
    glVertex3f(0, 0, 0);
    glVertex3f(-s * r, c * r, 0);
  glEnd();
end;

procedure TfrmGLRender.DrawPlayerTriangle;
begin
  glEnable(GL_LIGHTING);
  glRotatef(FCurrConn.LocalPlayer.Head, 0, 0, 1);

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

procedure TfrmGLRender.DAOCAddObject(Sender: TObject; AObj: TDAOCObject);
var
  iOldIndex:  integer;
  iOldTop:    integer;
  iPos:       integer;
begin
  if Sender <> FCurrConn then exit;

  if FRenderPrefs.IsObjectInFilter(AObj) then begin
    AObj.Highlight := FRenderPrefs.HighlightMobs and
      FRenderPrefs.MobFilterList.Matches(AObj.Name, AObj.Level);

    iPos := FilteredObjectInsert(AObj);

    UpdateObjectCounts;

    lstObjects.Locked := true;

    iOldIndex := lstObjects.ItemIndex;
    iOldTop := lstObjects.TopIndex;
    SetObjectListRowCount(FFilteredObjects.Count);
      { if we inserted an object before the selected index, update the
        selected index to keep it the same }
    if iPos <= iOldIndex then
      SendMessage(lstObjects.Handle, LB_SETCURSEL, iOldIndex + 1, 0)
    else if iOldIndex <> -1 then
      SendMessage(lstObjects.Handle, LB_SETCURSEL, iOldIndex, 0);

    if iPos <= iOldTop then
      SendMessage(lstObjects.Handle, LB_SETTOPINDEX, iOldTop + 1, 0)
    else
      SendMessage(lstObjects.Handle, LB_SETTOPINDEX, iOldTop, 0);

    lstObjects.Locked := false;

    if FRenderPrefs.RedrawOnAdd then
      Dirty;

{$IFDEF MSWINDOWS}
    if FRenderPrefs.PlayAlert and
      (GlobalTickCount - FLastAlertIntervalTicks >= FRenderPrefs.AlertInterval) and
      FRenderPrefs.AlertForObject(AObj) then begin
        FRenderPrefs.MobFilterList.DoAlert(AObj.Name);
        FLastAlertIntervalTicks := GlobalTickCount;
      end;

    if FRenderPrefs.InvaderWarning and
      (AObj.ObjectClass = ocPlayer) and
      (AObj.Realm <> FCurrConn.LocalPlayer.Realm) and AObj.IsAlive then
      if GlobalTickCount - FLastInvaderWarningTicks >= FRenderPrefs.InvaderWarnMinTicks then begin
        // Log('Invader: ' + AObj.Name);
        PlaySound('invader.wav', 0, SND_FILENAME or SND_ASYNC or SND_NOWAIT);
        FLastInvaderWarningTicks := GlobalTickCount;
      end;
{$ENDIF MSWINDOWS}
  end;  { if onject in filter }
end;

procedure TfrmGLRender.DAOCDeleteObject(Sender: TObject; AObj: TDAOCObject);
var
  iOldPos:  integer;
  iOldTop:  integer;
  iPos:     integer;
begin
  if Sender <> FCurrConn then exit;

  if FRenderPrefs.IsObjectInFilter(AObj) then begin
    iPos := FFilteredObjects.Remove(AObj);
    if iPos = -1 then begin
      Log('Removing: ' + AObj.Name + ' not in list');
      exit;
    end;

    UpdateObjectCounts;
    lstObjects.Locked := true;

    iOldPos := lstObjects.ItemIndex;
    iOldTop := lstObjects.TopIndex;
    SetObjectListRowCount(FFilteredObjects.Count);
    if iPos <= iOldPos then
      SendMessage(lstObjects.Handle, LB_SETCURSEL, iOldPos - 1, 0)
    else
      SendMessage(lstObjects.Handle, LB_SETCURSEL, iOldPos, 0);

    if (iPos <= iOldTop) and (iOldTop > 0) then
      SendMessage(lstObjects.Handle, LB_SETTOPINDEX, iOldTop - 1, 0)
    else if iOldTop < lstObjects.Items.Count then
      SendMessage(lstObjects.Handle, LB_SETTOPINDEX, iOldTop, 0);

    lstObjects.Locked := false;

    if FRenderPrefs.RedrawOnDelete then
      Dirty;
  end;
end;

procedure TfrmGLRender.DAOCUpdateObject(Sender: TObject; AObj: TDAOCObject);
begin
  if Sender <> FCurrConn then exit;

{$IFDEF MSWINDOWS}
    if FRenderPrefs.PlayAlert and
      (GlobalTickCount - FLastAlertIntervalTicks >= FRenderPrefs.AlertInterval) and
      FRenderPrefs.AlertForObject(AObj) then begin
        FRenderPrefs.MobFilterList.DoAlert(AObj.Name);
        FLastAlertIntervalTicks := GlobalTickCount;
      end;
{$ENDIF MSWINDOWS}

  if AObj.HitPoints <> AObj.HitPointsLast then
    InvalidateListObject(AObj);
    { RefreshFilteredList calls Dirty for us }
  if FRenderPrefs.MobListSortOrder = msoDistance then
    RefreshFilteredList
  else if FRenderPrefs.RedrawOnUpdate then
    Dirty;
end;

procedure TfrmGLRender.glMapClick(Sender: TObject);
var
  pNearest: TDAOCObject;
  x, y:     Cardinal;
begin
  if not (FRenderPrefs.TrackMapClick and Assigned(FCurrConn)) then
    exit;

  MapUnproject(X, Y, true);

    { if this is a dungeon, use 3d distance else use 2d }
  if Assigned(FCurrConn.Zone) and (FCurrConn.Zone.ZoneType = dztDungeon) then
    pNearest := FFilteredObjects.FindNearest3D(x, y, FCurrConn.LocalPlayer.Z)
  else
    pNearest := FFilteredObjects.FindNearest2D(x, y);

  if not Assigned(pNearest) or
    (pNearest.DistanceSqr2D(x, y) > FCurrConn.LocalPlayer.DistanceSqr2D(x, y)) then
    pNearest := FCurrConn.LocalPlayer;

  if FRenderPrefs.TrackInGameSelect then
      { callback will update screen }
    FCurrConn.SelectedObject := pNearest
  else
    GridSelectObject(pNearest);
  Dirty;
end;

procedure TfrmGLRender.DrawLineToSelected;
var
  pSelected:  TDAOCObject;
begin
  pSelected := FCurrConn.SelectedObject;
  if Assigned(pSelected) then begin
    glDisable(GL_LIGHTING);

    glColor3f(1.0, 1.0, 1.0);
    glLineWidth(2.0);
    glBegin(GL_LINES);
    glVertex3i(FCurrConn.LocalPlayer.XProjected, FCurrConn.LocalPlayer.YProjected, 0);
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
  glEnable(GL_LIGHTING);

  for I := 0 to FFilteredObjects.Count - 1 do begin
    pObj := FFilteredObjects[I];
    if pObj.ObjectClass in [ocUnknown, ocMob, ocPlayer] then begin
        { quick and easy cull check.  Note: object must have its center inside
          the renderbounds }
      if not PointInRect(FRenderBounds, pObj.X, pObj.Y) then
        continue;

      pMovingObj := TDAOCMovingObject(pObj);
      if pObj.Stealthed then
        clMob := clBlack
      else if FRenderPrefs.SwapTringleRingShade then
        clMob := RealmColor(pObj.Realm)
      else
        clMob := pObj.GetConColor(FCurrConn.LocalPlayer.Level);

      DrawAIDestination(pObj, clMob);

      glTranslatef(pMovingObj.XProjected, pMovingObj.YProjected, 0);
      glRotatef(pObj.Head, 0, 0, 1);

      if pObj.Highlight then
        FObjectHighlight.GLRender(FRenderBounds);

      if pObj.ObjectClass = ocPlayer then
        DrawPlayerHighlightRing(TDAOCPlayer(pObj))
      else if FRenderPrefs.DrawTypeTag and (pObj.ObjectClass = ocMob) then
        DrawMobTypeTag(TDAOCMob(pObj));

      if pObj.LiveDataConfidence < LIVE_DATA_CONFIDENCE_MAX then
        SetGLColorFromTColor(clMob, pObj.LiveDataConfidencePct)
      else
        glColor3ubv(@clMob);
      FMobTriangle.GLRender(FRenderBounds);

      //glRotatef(-pObj.Head, 0, 0, 1);
      //glTranslatef(-pMovingObj.XProjected, -pMovingObj.YProjected, 0);
      glLoadIdentity();
    end  { if a class to draw }

    else if pObj.ObjectClass = ocObject then begin
      glTranslatef(pObj.X, pObj.Y, 0);
      if AnsiSameText(pObj.Name, 'Prescience Node') then
        FPrescienceNode.GLRender(FRenderBounds)
      else begin
        FObjectTriangle.Color := clWhite;
        FObjectTriangle.GLRender(FRenderBounds);
      end;
      //glTranslatef(-pObj.X, -pObj.Y, 0);
      glLoadIdentity();
    end

    else if pObj.ObjectClass = ocVehicle then begin
      DrawAIDestination(pObj, FBoat.Color);  // brown

      glTranslatef(TDAOCVehicle(pObj).XProjected, TDAOCVehicle(pObj).YProjected, 0);
      glRotatef(pObj.Head, 0, 0, 1);
      if pObj.LiveDataConfidence < LIVE_DATA_CONFIDENCE_MAX then
        FBoat.Alpha := pObj.LiveDataConfidencePct
      else
        FBoat.Alpha := 1;
      FBoat.GLRender(FRenderBounds);
      //glTranslatef(-pObj.X, -pObj.Y, 0);
      glLoadIdentity();
    end

    else if pObj.ObjectClass = ocDoor then begin
      glTranslatef(pObj.X, pObj.Y, 0);
      if pObj.DoorIsOpen then
        FObjectTriangle.Color := clGreen
      else
        FObjectTriangle.Color := clMaroon;
      FObjectTriangle.GLRender(FRenderBounds);
      //glTranslatef(-pObj.X, -pObj.Y, 0);
      glLoadIdentity();
    end
  end;  { for each object }
end;

procedure TfrmGLRender.FormCreate(Sender: TObject);
begin
    { create the glwindow first in case other loads require a context }
  CreateGLWindow;

  FBasePath := ExtractFilePath(ParamStr(0));
  FRangeCircles := TRangeCircleList.Create;

  FHTTPFetch := TBackgroundHTTPManager.Create;
  FHTTPFetch.AgentVersion := frmMain.Version;

  FMapElementsListList := TVectorMapElementListList.Create;
  FMapElementsListList.VersionFile := FBasePath + 'versions.ini';
  FMapElementsListList.VectorMapDir := FBasePath + 'maps' + PathDelim;
  FMapElementsListList.VectorMapCustomDir := FBasePath + 'custommaps' + PathDelim;
  FMapElementsListList.HTTPFetch := FHTTPFetch;

  FMapTexturesListList := TTextureMapElementListList.Create;
  FMapTexturesListList.VersionFile := FBasePath + 'versions.ini';
  FMapTexturesListList.TextureMapDir := FBasePath + 'maps' + PathDelim + 'dds' + PathDelim;
  FMapTexturesListList.TextureMapCustomDir := FBasePath + 'custommaps' + PathDelim + 'dds' + PathDelim;
  FMapTexturesListList.HTTPFetch := FHTTPFetch;

  FMobTriangle := T3DArrowHead.Create;
  FPrescienceNode := TGLPrescienceNode.Create;
  FPrescienceNode.ImageFileName := FBasePath + 'prescience.tga';
  //FHudConFlag := TGLHudConFlag.Create;
  //FHudConFlag.ImageFileName := FBasePath + 'conflag.tga';
  FObjectTriangle := T3DPyramid.Create;
  FGroundTarget := TGLBullsEye.Create;
  FVisibleRangeRep := TGLFlatViewFrustum.Create;
  FBoat := TGLBoat.Create;
  FFilteredObjects := TDAOCObjectList.Create(false);
  FPushPins := TVectorMapElementList.Create;
  FUnknownStealther := TGLUnknownStealther.Create;
  FObjectHighlight := TGLObjectHighlight.Create;
  FRenderPrefs := TRenderPreferences.Create;
  FRenderPrefs.OnObjectFilterChanged := RENDERPrefsObjectFilterChanged;
  FRenderPrefs.OnMobListOptionsChanged := RENDERPrefsMobListOptionChanged;
  FRenderPrefs.OnMobTriangleSizeChanged := RENDERPrefsMobTriangleSizeChanged;
  FRenderPrefs.OnMinFPSChanged := RENDERPrefsMinFPSChanged; 
  FRenderPrefs.HasOpenGL13 := Load_GL_version_1_3;

  if FRenderPrefs.HasOpenGL13 then
    Log('OpenGL 1.3:  Available (' + GetGLVersion + ')')
  else
    Log('OpenGL 1.3:  NOT FOUND (' + GetGLVersion + ')');

  UpdateObjectCounts;
  CreateObjectListBox;
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
  FHTTPFetch.Shutdown;
  FHTTPFetch.Free;
  FPushPins.Free;
  FPrescienceNode.Free;
  //FHudConFlag.Free;
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
  FBoat.GLCleanup;
  FPushPins.GLCleanup;
  FUnknownStealther.GLCleanup;
  FObjectHighlight.GLCleanup;
  FPrescienceNode.GLCleanup;
  //FHudConFlag.GLCleanup;

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
  FBoat.GLInitialize;
  FPushPins.GLInitialize;
  FUnknownStealther.GLInitialize;
  FObjectHighlight.GLInitialize;
  FPrescienceNode.GLInitialize;
  //FHudConFlag.GLInitialize;

  CheckGLError;
end;

procedure TfrmGLRender.DrawMapElements;
begin
  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDisable(GL_BLEND);
  glLineWidth(1.0);

  if FRenderPrefs.DrawMapTexture then
    FMapTexturesListList.GLRender(FRenderBounds);
  if FRenderPrefs.DrawMapVector then begin
    if FMapElementsListList.DrawInfoPoints <> FRenderPrefs.DrawInfoPoints then
      FMapElementsListList.DrawInfoPoints := FRenderPrefs.DrawInfoPoints;
    FMapElementsListList.GLRender(FRenderBounds);
  end;
  if FRenderPrefs.DrawPushPins then
    FPushPins.GLRender(FRenderBounds);
end;

procedure TfrmGLRender.DAOCZoneChanged(Sender: TObject);
begin
  if Sender <> FCurrConn then exit;

  if not Assigned(FCurrConn.Zone) then begin
    FZoneName := '';
    exit;
  end;

  FZoneName := FCurrConn.Zone.Name;

  UpdateMapURLs;
  ReloadMapElementsAndTextures;

  Dirty;
end;

procedure TfrmGLRender.DAOCRegionChanged(Sender: TObject);
begin
  if (Sender <> FCurrConn) or not Assigned(FDAOCConnectionList) then exit;

  UpdateCaption;
  FRenderPrefs.PlayerRealm := FCurrConn.LocalPlayer.Realm;
  DAOCSetGroundTarget(Sender);
  LoadRegionPushpins;
end;

procedure TfrmGLRender.GridSelectObject(ADAOCObject: TDAOCObject);
var
  I:        integer;
  iOldTop:  integer;
begin
  UpdateInventoryPanel(false);

  if Assigned(ADAOCObject) then
    I := FFilteredObjects.IndexOf(ADAOCObject)
  else
    I := -1;

  if I = lstObjects.ItemIndex then
    exit;

  iOldTop := lstObjects.TopIndex;

  if FRenderPrefs.AutoScrollMoblist then begin
    lstObjects.ItemIndex := I;
  end

  else begin
    lstObjects.Locked := true;
    SendMessage(lstObjects.Handle, LB_SETCURSEL, I, 0);
    SendMessage(lstObjects.Handle, LB_SETTOPINDEX, iOldTop, 0);
    lstObjects.Locked := false;
  end;
end;

procedure TfrmGLRender.DAOCSelectedObjectChanged(Sender: TObject; AObj: TDAOCObject);
begin
  if Sender <> FCurrConn then exit;

  if FRenderPrefs.TrackInGameSelect then begin
    GridSelectObject(AObj);
    Dirty;
  end;
end;

procedure TfrmGLRender.DrawPlayerHighlightRing(ADAOCObject: TDAOCPlayer);
var
  cl:     TColor;
  fSize:  GLfloat;
  fAlphaMax:  GLfloat;
begin
  if not FRenderPrefs.DrawPlayerHighlightRing then
    exit;
    
  if FRenderPrefs.SwapTringleRingShade then
    cl := ADAOCObject.GetConColor(FCurrConn.LocalPlayer.Level)
  else
    cl := RealmColor(ADAOCObject.Realm);

  if ADAOCObject.IsInGroup or ADAOCObject.IsInGuild then begin
    fSize :=  FMobTriangle.Size * 1.75;
    glShadeModel(GL_SMOOTH);
  end
  else
    fSize :=  FMobTriangle.Size * 1.33;

  if ADAOCObject.IsDead then
    SetGLColorFromTColorDarkened(cl, 1, 0.25)
  else begin
    fAlphaMax := 1;
    if (ADAOCObject.Realm <> FCurrConn.LocalPlayer.Realm) and FInvaderHighlight then
      fAlphaMax := 0.6;
    SetGLColorFromTColor(cl, fAlphaMax * ADAOCObject.LiveDataConfidencePct);
  end;

  glDisable(GL_LIGHTING);
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
  glEnable(GL_LIGHTING);
end;

procedure TfrmGLRender.tmrMinFPSTimer(Sender: TObject);
begin
  if FRenderPrefs.RedrawOnTimer then begin
    if Assigned(FCurrConn) and FCurrConn.Active then
      UpdateGlobalTickCount;
    Dirty;
  end;
end;

procedure TfrmGLRender.DrawTargetHUD;
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
      //FHudConFlag.Color := GetConColor(FCurrConn.LocalPlayer.Level);
      //FHudConFlag.GLRender(Rect(0, 0, Width, Height));
      SetGLColorFromTColor(GetConColor(FCurrConn.LocalPlayer.Level), 1);
      rastery := WriteGLUTTextH12(4, rastery, AName);
    end;
  end;

  procedure WriteMobLevelHealth;
  var
    s:  string;
  begin
    with TDAOCMovingObject(pMob) do begin
      s := 'Level ' + IntToStr(Level);
      if (pMob.ObjectClass = ocPlayer) and (TDAOCPlayer(pMob).RealmRank <> rrUnknown) then
        s := s + ' ' + TDAOCPlayer(pMob).RealmRankStr;
      if HitPoints <> 100 then
        if IsDead then
          s := s + ' (dead)'
        else
          s := s + ' (' + IntToStr(HitPoints) + '%)';
      if (pMob.ObjectClass = ocPlayer) and
        (TDAOCPlayer(pMob).ManaPct <> 0) and (TDAOCPlayer(pMob).ManaPct <> 100) then
        s := s + ' ' + IntToStr(TDAOCPlayer(pMob).ManaPct) + '%M';
      if (pMob.ObjectClass = ocLocalPlayer) and
        (TDAOCLocalPlayer(pMob).ManaPct <> 0) and (TDAOCLocalPlayer(pMob).ManaPct <> 100) then
        s := s + ' ' + IntToStr(TDAOCLocalPlayer(pMob).ManaPct) + '%M';
      rastery := WriteGLUTTextH12(4, rastery, s);
    end;
  end;

begin
  if not FRenderPrefs.DrawHUD then
    exit;

  pMob := FCurrConn.SelectedObject;
  if not Assigned(pMob) then
    exit;

  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  rastery := glMap.ClientHeight - 1;
  ShadedRect(1, rastery, 160, rastery - 56);

  glDisable(GL_BLEND);
  case pMob.ObjectClass of
    ocObject, ocDoor:
      begin
        glColor3f(0.9, 0.9, 0.9);
        rastery := WriteGLUTTextH12(4, rastery, pMob.Name);
        glColor4fv(@TEXT_COLOR);
        if pMob.ObjectClass = ocDoor then
          if pMob.DoorIsOpen then
            rastery := WriteGLUTTextH12(4, rastery, 'Door is OPEN')
          else
            rastery := WriteGLUTTextH12(4, rastery, 'Door is CLOSED');
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

    ocLocalPlayer:
      with TDAOCLocalPlayer(pMob) do begin
        WriteMobNameCon(FullName);
        glColor4fv(@TEXT_COLOR);
        if Guild <> '' then
          rastery := WriteGLUTTextH12(4, rastery, '<' + Guild + '>');
        WriteMobLevelHealth;
      end;  { ocLocalPlayer }

    ocVehicle:
      with TDAOCVehicle(pMob) do begin
        WriteMobNameCon(Name);
      end;  { ocVehicle }

    else
        glColor4fv(@TEXT_COLOR);
  end;    { case class }

  s := 'Dist: ' + FormatFloat('0', pMob.Distance3D(FCurrConn.LocalPlayer)) +
    ' ' + ZDeltaStr(pMob, false);
  if (pMob is TDAOCMovingObject) and (TDAOCMovingObject(pMob).Speed <> 0) then
    s := s + '  Speed: ' + TDAOCMovingObject(pMob).SpeedString;
  rastery := WriteGLUTTextH10(4, rastery, s);
end;

procedure TfrmGLRender.SetupRadarProjectionMatrix;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if FRenderPrefs.RotateMapWithPlayer then
    glRotatef(FCurrConn.LocalPlayer.Head - 180, 0, 0, 1)
  else if Assigned(FCurrConn.Zone) and (FCurrConn.Zone.Rotate > 0) then
    glRotatef(FCurrConn.Zone.Rotate, 0, 0, 1);
  glRotatef(180, 1, 0, 0);

  FRenderBounds.Left := FCurrConn.LocalPlayer.XProjected - FRange;
  inc(FRenderBounds.Left, FMapToPlayerOffset.X);
  FRenderBounds.Right := FRenderBounds.Left + (integer(FRange) * 2);

  FRenderBounds.Top := FCurrConn.LocalPlayer.YProjected - FRange;
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

procedure TfrmGLRender.RefreshFilteredList;
var
  pObj:   TDAOCObject;
  I:      integer;
  R:      TRect;
  pOldFilteredList:  TDAOCObjectList;
begin
  if not Assigned(FCurrConn) then exit;
  
    { save the old list because we might need it for invalidating }
  pOldFilteredList := FFilteredObjects;
  FFilteredObjects := TDAOCObjectList.Create(false);

  pObj := FCurrConn.DAOCObjects.Head;
  while Assigned(pObj) do begin
    if FRenderPrefs.IsObjectInFilter(pObj) then
      FilteredObjectInsert(pObj);
    pObj.Highlight := FRenderPrefs.HighlightMobs and
      FRenderPrefs.MobFilterList.Matches(pObj.Name, pObj.Level);
    pObj := pObj.Next;
  end;

  UpdateObjectCounts;
    { if the number of items hasn't changed then we need to selectively invalidate }
  if not SetObjectListRowCount(FFilteredObjects.Count) then
    for I := 0 to pOldFilteredList.Count - 1 do
      if pOldFilteredList[I] <> FFilteredObjects[I] then begin
        R := lstObjects.ItemRect(I);
        InvalidateRect(lstObjects.Handle, @R, false);
      end;

  pOldFilteredList.Free;
  Dirty;
end;

procedure TfrmGLRender.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  SetCurrentConnection(nil);
end;

function TfrmGLRender.FilteredObjectInsert(ADAOCObject: TDAOCObject) : integer;
begin
  case FRenderPrefs.MobListSortOrder of
    msoName:  Result := FilteredObjectInsertByName(ADAOCObject);
    msoDistance: Result := FilteredObjectInsertByDistance(ADAOCObject);
    else
      Result := FFilteredObjects.Add(ADAOCObject);
  end;  { case Sort order }
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
    glPushAttrib(GL_LIGHTING_BIT);
    glDisable(GL_LIGHTING);
    WriteGLUTTextH10(30, 30, AMob.TypeTag);
    glPopAttrib();
  end;
end;

function TfrmGLRender.SetObjectListRowCount(ACount: integer) : boolean;
{ Returns true if the number of items has changed }
begin
  if lstObjects.Count <> ACount then begin
    lstObjects.Count := ACount;
    Result := true;
  end
  else
    Result := false;
end;

function TfrmGLRender.PlayerMobListText(AMob: TDAOCPlayer) : string;
begin
  if AMob.Realm <> FCurrConn.LocalPlayer.Realm then begin
    Result := '';
    
    if AMob.RealmRank <> rrUnknown then
      Result := AMob.RealmRankStr + ' - ';

    Result := Result + AMob.Name;

    if AMob.CharacterClass <> ccUnknown then
      Result := Result + ' ' + DAOCCharacterClassToStr(AMob.CharacterClass);
  end

    { same realm }
  else begin
    if AMob.CharacterClass <> ccUnknown then
      Result := AMob.Name + ' (' + DAOCCharacterClassToStr(AMob.CharacterClass) + ')'
    else
      Result := AMob.Name;
  end;
end;

procedure TfrmGLRender.lstObjectsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  pMob:   TDAOCObject;
  R,G,B:  byte;
  sText:  string;
  cl:     TColor;
begin
    { we have to make a check here because LB_SETCURSEL forces a CNDrawItem call.
      This would cause us to draw the selected item every time it changes, even
      if it is not on the screen at the time }
  if lstObjects.Locked then
    exit;

  if Index < FFilteredObjects.Count then
    with lstObjects.Canvas do begin
      pMob := FFilteredObjects[Index];

        { highlight overrules everything }
      if pMob.Highlight then begin
        Font.Color := clBlack;
        Brush.Color := clYellow;
      end

        { objects are gray on white }
      else if pMob.ObjectClass = ocObject then begin
        Font.Color := clGray;
        Brush.Color := clWhite;
      end  { object }

        { mobs are con color on white }
      else if pMob.Realm = drNeutral then begin
        cl := pMob.GetConColor(FCurrConn.LocalPlayer.Level);
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
      if pMob is TDAOCPlayer then
        sText := PlayerMobListText(TDAOCPlayer(pMob))
      else if FRenderPrefs.AlternateMobListText and (pMob is TDAOCMob) then
        sText := TDAOCMob(pMob).TypeTag
      else
        sText := pMob.Name;
      TextOut(Rect.Left + 3, Rect.Top + 2, sText);

        { LEVEL }
      sText := IntToStr(pMob.Level);
      TextOut(Rect.Left + 150, Rect.Top + 2, sText);

        { HEALTH }
      if pMob.HitPoints > 0 then begin
        sText := IntToStr(pMob.HitPoints);
        TextOut(Rect.Left + 175, Rect.Top + 2, sText);
      end;

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

procedure TfrmGLRender.DAOCSetGroundTarget(Sender: TObject);
begin
  if Sender <> FCurrConn then exit;

  with FCurrConn.GroundTarget do
    FGroundTarget.Assign(X, Y);
    
  Dirty;
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
  dwStyle:  Cardinal;
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
  FMapElementsListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;
  FMapTexturesListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;

  FMapElementsListList.LoadForZone(FCurrConn.Zone, FRenderPrefs.AdjacentZones);
  FMapTexturesListList.LoadForZone(FCurrConn.Zone, FRenderPrefs.AdjacentZones);
end;

procedure TfrmGLRender.DoPrefsDialog;
var
  tmpPrefs:   TRenderPreferences;
begin
      { turn off attempt download if continuous failure }
  FRenderPrefs.AttemptMapDownload := FRenderPrefs.AttemptMapDownload and
    FMapTexturesListList.AttemptDownload and FMapElementsListList.AttemptDownload;

  tmpPrefs := FRenderPrefs.Clone;

  if TfrmRenderPrefs.Execute(Self, FRenderPrefs, FRangeCircles) then begin
    FMapTexturesListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;
    FMapElementsListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;

    if (tmpPrefs.AdjacentZones <> FRenderPrefs.AdjacentZones) or
      (tmpPrefs.AdjacentZones <> FRenderPrefs.AttemptMapDownload) then
      ReloadMapElementsAndTextures;
    UpdateStayOnTop;
    tmpPrefs.Free;
  end

  else begin
    FRenderPrefs.Free;
    FRenderPrefs := tmpPrefs;
    FRenderPrefs.OnObjectFilterChanged := RENDERPrefsObjectFilterChanged;
    FRenderPrefs.OnMobListOptionsChanged := RENDERPrefsMobListOptionChanged;
    FRenderPrefs.OnMobTriangleSizeChanged := RENDERPrefsMobTriangleSizeChanged;
    FRenderPrefs.OnMinFPSChanged := RENDERPrefsMinFPSChanged;

    RENDERPrefsObjectFilterChanged(FRenderPrefs);
    RENDERPrefsMobListOptionChanged(FRenderPrefs);
    RENDERPrefsMobTriangleSizeChanged(FRenderPrefs);
    RENDERPrefsMinFPSChanged(FRenderPrefs);
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

procedure TfrmGLRender.DrawLocalPlayerHUD;
var
  rastery:  integer;
  rasterx:  integer;
  s:    string;
begin
  if not FRenderPrefs.DrawHUD then
    exit;

  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  rastery := glMap.ClientHeight - 1;
  rasterx := glMap.ClientWidth - 125;

  ShadedRect(rasterx, rastery, rasterx + 124, rastery - 43);
  inc(rasterx, 2);

  glDisable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  with FCurrConn do begin
    if FZoneName <> '' then
      rastery := WriteGLUTTextH10(rasterx, rastery, FZoneName);
    s := Format('(%d,%d,%d)', [PlayerZoneX, PlayerZoneY, PlayerZoneZ]);
    rastery := WriteGLUTTextH10(rasterx, rastery, s);
    s := 'Heading ' + IntToStr(PlayerZoneHead) + ' Speed ' + LocalPlayer.SpeedString;
    WriteGLUTTextH10(rasterx, rastery, s);
  end;
end;

procedure TfrmGLRender.DrawAIDestination(AObj: TDAOCObject; AColor: TColor);
var
  pMovingObj:   TDAOCMovingObject;
begin
  if not FRenderPrefs.DrawAIDestination then
    exit;

    { if the mob is on the move, draw a line to its destination }
  if (AObj.ObjectClass = ocMob) and Assigned(TDAOCMob(AObj).Target) then
    DrawLineToMobTarget(TDAOCMob(AObj))
  else if (AObj.DestinationX <> 0) and (AObj.DestinationY <> 0) then begin
    pMovingObj := TDAOCMovingObject(AObj);

    glLineWidth(3.0);
    SetGLColorFromTColor(AColor, 0.33);

    glBegin(GL_LINES);
      glVertex3f(pMovingObj.XProjected, pMovingObj.YProjected, 0);
      glVertex3f(pMovingObj.DestinationX, pMovingObj.DestinationY, 0);
    glEnd();
  end;  { if destinaton set }
end;

procedure TfrmGLRender.DrawGrid;
const
  GRID_STEP = 10000;
var
  X:    integer;
  Y:    integer;
  XMin: integer;
  YMin: integer;
  XMax: integer;
  YMax: integer;
begin
  if not FRenderPrefs.DrawGrid then
    exit;

    { the grid only draws over the current zone }
  if not Assigned(FCurrConn.Zone) then
    exit;

  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glLineWidth(1.0);
  glColor4f(0, 0, 1, 0.5);

    { round down to the next 10,000 mark }
//  XMin := (FRenderBounds.Left div GRID_STEP) * GRID_STEP;
//  YMin := (FRenderBounds.Top div GRID_STEP) * GRID_STEP;
    { round up to the next 10,000 mark }
//  XMax := ((FRenderBounds.Right + GRID_STEP - 1) div GRID_STEP) * GRID_STEP;
//  YMax := ((FRenderBounds.Bottom + GRID_STEP - 1) div GRID_STEP) * GRID_STEP;

  with FCurrConn.Zone do begin
    XMin := BaseLoc.X;
    YMin := BaseLoc.Y;
    XMax := MaxLoc.X;
    YMax := MaxLoc.Y;
  end;

  Y := YMin + GRID_STEP;
  while Y <= YMax do begin
    glBegin(GL_LINES);
        { horizontal line }
      glVertex2i(XMin, Y);
      glVertex2i(XMax, Y);
    glEnd();

    inc(Y, GRID_STEP);
  end;  { for Y }

  X := XMin + GRID_STEP;
  while X <= XMax do begin
    glBegin(GL_LINES);
        { vertical line }
      glVertex2i(X, YMin);
      glVertex2i(X, YMax);
    glEnd();

    inc(X, GRID_STEP);
  end;  { for X }
end;

procedure TfrmGLRender.MapUnproject(var X, Y: Cardinal; ANeedMVPSetup: boolean);
var
  projmatrix: T16dArray;
  modmatrix:  T16dArray;
  viewport:   TViewPortArray;
  fx, fy, fz: GLdouble;
  pt:         TPoint;
begin
  pt := glMap.ScreenToClient(Mouse.CursorPos);
  if (pt.X < 0) or (pt.Y < 0) or (pt.X > glMap.Width) or (pt.Y > glMap.Height) then begin
    X := 0;
    Y := 0;
    exit;
  end;

  if ANeedMVPSetup then begin
    SetupRadarProjectionMatrix;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
  end;

  glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, @modmatrix);
  glGetIntegerv(GL_VIEWPORT, @viewport);

  gluUnProject(pt.X, glMap.Height - pt.Y, 0, modmatrix, projmatrix, viewport,
    @fx, @fy, @fz);

  X := round(fx);
  Y := round(fy);
end;

procedure TfrmGLRender.glMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
    { for mouse tooltip }
  if ssCtrl in Shift then
    Dirty;
end;

procedure TfrmGLRender.DrawMouseTooltip;
var
  ptMouse:  TPoint;
  iHeight:  integer;
  iDist:    integer;
  ZoneX:    Cardinal;
  ZoneY:    Cardinal;
  pNearest: TDAOCObject;
begin
  // (GetAsyncKeyState(VK_CONTROL) = 0) or
  if not Assigned(FCurrConn.Zone) or ((FMouseLocX = 0) and (FMouseLocY = 0)) then
    exit;

    { distance from here to the mouse }
  iDist := round(FCurrConn.LocalPlayer.Distance2D(FMouseLocX, FMouseLocY));

  if FCurrConn.Zone.ZoneType = dztDungeon then
    pNearest := FFilteredObjects.FindNearest3D(FMouseLocX, FMouseLocY, FCurrConn.LocalPlayer.Z)
  else
    pNearest := FFilteredObjects.FindNearest2D(FMouseLocX, FMouseLocY);

  if Assigned(pNearest) and
    (round(pNearest.Distance2D(FMouseLocX, FMouseLocY)) > (2 * FMobTriangle.Size)) then
    pNearest := nil;

  if Assigned(pNearest) then
    iHeight := 42
  else
    iHeight := 14;

  ptMouse := glMap.ScreenToClient(Mouse.CursorPos);
  ptMouse.X := ptMouse.X + 2;
  ptMouse.Y := glMap.ClientHeight - ptMouse.Y + iHeight;

  ZoneX := FMouseLocX - Cardinal(FCurrConn.Zone.BaseLoc.X);
  ZoneY := FMouseLocY - Cardinal(FCurrConn.Zone.BaseLoc.Y);

  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  ShadedRect(ptMouse.X, ptMouse.Y, ptMouse.X + 120, ptMouse.Y - iHeight);
  glColor3f(1, 1, 1);

  ptMouse.X := ptMouse.X + 1;
  ptMouse.Y := ptMouse.Y + 2;
  if Assigned(pNearest) then begin
    ptMouse.Y := WriteGLUTTextH10(ptMouse.X, ptMouse.Y, pNearest.Name);
    ptMouse.Y := WriteGLUTTextH10(ptMouse.X, ptMouse.Y, ZDeltaStr(pNearest, true));
  end;
  WriteGLUTTextH10(ptMouse.X, ptMouse.Y, Format('%d,%d Dist %d', [ZoneX, ZoneY, iDist]));
end;

procedure TfrmGLRender.MobListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pnlLeft.BeginDrag(false);
  if (lstObjects.ItemIndex <> -1) and (lstObjects.ItemIndex < FFilteredObjects.Count) then
    FCurrConn.SelectedObject := FFilteredObjects[lstObjects.ItemIndex];
end;

procedure TfrmGLRender.pnlLeftEndDock(Sender, Target: TObject; X,
  Y: Integer);
begin
  if Assigned(pnlLeft.Parent) then
    pnlLeft.Align := alLeft;
end;

procedure TfrmGLRender.DAOCCharacterLogin(Sender: TObject);
begin
  if Sender <> FCurrConn then exit;

  FRenderPrefs.PlayerLevel := FCurrConn.LocalPlayer.Level;
end;

procedure TfrmGLRender.RENDERPrefsMobListOptionChanged(Sender: TObject);
begin
  RefreshFilteredList;
  lstObjects.Invalidate;
  UpdateInventoryPanel(false);
end;

function TfrmGLRender.FilteredObjectInsertByDistance(ADAOCObject: TDAOCObject): integer;
var
  I:   integer;
begin
  if Assigned(FCurrConn.Zone) then
    for I := 0 to FFilteredObjects.Count - 1 do
      if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) < 0 then begin
        FFilteredObjects.Insert(I, ADAOCObject);
        Result := I;
        exit;
      end
      else if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) = 0 then begin
        if FRenderPrefs.GroupByRealm and (ADAOCObject.Realm < FFilteredObjects[I].Realm) then begin
          FFilteredObjects.Insert(I, ADAOCObject);
          Result := I;
          exit;
        end

        else if not FRenderPrefs.GroupByRealm or (ADAOCObject.Realm = FFilteredObjects[I].Realm) then begin
          if ((FCurrConn.Zone.ZoneType = dztDungeon) and
              (ADAOCObject.DistanceSqr3D(FCurrConn.LocalPlayer) < FFilteredObjects[I].DistanceSqr3D(FCurrConn.LocalPlayer)))
            or
            ((FCurrConn.Zone.ZoneType <> dztDungeon) and
              (ADAOCObject.DistanceSqr2D(FCurrConn.LocalPlayer) < FFilteredObjects[I].DistanceSqr2D(FCurrConn.LocalPlayer)))
            then begin
            FFilteredObjects.Insert(I, ADAOCObject);
            Result := I;
            exit;
          end;
        end;  { if same realm }
      end;  { if samce object class }

  Result := FFilteredObjects.Add(ADAOCObject);
end;

function TfrmGLRender.FilteredObjectInsertByName(ADAOCObject: TDAOCObject): integer;
var
  I:   integer;
begin
  for I := 0 to FFilteredObjects.Count - 1 do
    if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) < 0 then begin
      FFilteredObjects.Insert(I, ADAOCObject);
      Result := I;
      exit;
    end

    else if CompareObjectClasses(ADAOCObject.ObjectClass, FFilteredObjects[I].ObjectClass) = 0 then begin
      if FRenderPrefs.GroupByRealm and (ADAOCObject.Realm < FFilteredObjects[I].Realm) then begin
        FFilteredObjects.Insert(I, ADAOCObject);
        Result := I;
        exit;
      end

      else if not FRenderPrefs.GroupByRealm or (ADAOCObject.Realm = FFilteredObjects[I].Realm) then begin
        if AnsiCompareText(ADAOCObject.Name, FFilteredObjects[I].Name) < 0 then begin
          FFilteredObjects.Insert(I, ADAOCObject);
          Result := I;
          exit;
        end;
      end;  { if same realm }
    end;  { if samce object class }

  Result := FFilteredObjects.Add(ADAOCObject);
end;

function TfrmGLRender.CompareObjectClasses(A, B: TDAOCObjectClass) : integer;
const
  OBJECT_ORDER: array[TDAOCObjectClass] of integer = (5, 4, 3, 2, 1, 0, 6);
begin
  if not FRenderPrefs.GroupByClass then
    Result := 0
  else if OBJECT_ORDER[A] < OBJECT_ORDER[B] then
    Result := -1
  else if OBJECT_ORDER[A] > OBJECT_ORDER[B] then
    Result := 1
  else
    Result := 0;
end;

procedure TfrmGLRender.DAOCPlayerPosUpdate(Sender: TObject);
begin
  if Sender <> FCurrConn then exit;

    { RefreshFilteredList calls Dirty for us }
  if FRenderPrefs.MobListSortOrder = msoDistance then
    RefreshFilteredList
  else
    Dirty;
end;

procedure TfrmGLRender.UpdateMapURLs;
begin
  FMapTexturesListList.MapBaseURL := FRenderPrefs.MapBaseURL +
    's=' + FCurrConn.ServerIP + '&';
  FMapElementsListList.MapBaseURL := FRenderPrefs.MapBaseURL +
    's=' + FCurrConn.ServerIP + '&';
end;

procedure TfrmGLRender.CreateObjectListBox;
begin
  lstObjects := TLockableListBox.Create(Self);
  lstObjects.Parent := pnlLeft;
  lstObjects.Align := alClient;
  lstObjects.Style := lbVirtualOwnerDraw;
  lstObjects.BorderStyle := bsNone;
  lstObjects.Ctl3D := false;
  lstObjects.ItemHeight := 16;
  lstObjects.OnDrawItem := lstObjectsDrawItem;
  lstObjects.OnMouseDown := MobListMouseDown;
end;

procedure TfrmGLRender.LoadRegionPushpins;
begin
  FPushPins.GLCleanup;
  FPushPins.LoadFromFile(Format('%sregion%3.3d.pin', [FBasePath, FCurrConn.RegionID]));
  FPushPins.GLInitialize;
end;

procedure TfrmGLRender.AddPushPin;
var
  pPin:   TMapElementPoint;
begin
  if not Assigned(FCurrConn.Zone) then
    exit;

  pPin := TfrmAddPushpin.Execute(FCurrConn.LocalPlayer.X,
    FCurrConn.LocalPlayer.Y, FCurrConn.LocalPlayer.Z, FCurrConn.Zone.Name);

  if Assigned(pPin) then begin
    FPushPins.Add(pPin);
    FPushPins.Save('Pushpin format: P,<label>,<color>,<world x>,<world y>,<world z>');
  end;
end;

procedure TfrmGLRender.DAOCUnknownStealther(Sender: TObject; AObj: TDAOCObject);
begin
  if Sender <> FCurrConn then exit;

  Dirty;
end;

procedure TfrmGLRender.DrawUnknownStealthers;
var
  pObj:   TDAOCObject;
begin
  if not FRenderPrefs.AnonymousStealthers then
    exit;
    
  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  pObj := FCurrConn.UnknownStealthers.Head;
  while Assigned(pObj) do begin
    glTranslatef(pObj.X, pObj.Y, 0);
    FUnknownStealther.Alpha := pObj.LiveDataConfidencePct * 0.33;
    FUnknownStealther.GLRender(FRenderBounds);
    //glTranslatef(-pObj.X, -pObj.Y, 0);
    glLoadIdentity();

    pObj := pObj.Next;
  end;
end;

procedure TfrmGLRender.DrawStealtherAlert;
begin
  if not FRenderPrefs.AnonymousStealthers then
    exit;

  if not Assigned(FCurrConn.UnknownStealthers.Head) then
    exit;

  //glDisable(GL_LIGHTING);  // should already be off?
  glColor3f(0, 0, 0);
  WriteGLUTTextH12(glMap.ClientWidth - 117, 15, 'Stealther in proximity');
  glColor3f(1, 0, 0);
  WriteGLUTTextH12(glMap.ClientWidth - 118, 16, 'Stealther in proximity');
end;

procedure TfrmGLRender.SetSmoothingOpts;
begin
  if FRenderPrefs.SmoothLines then
    glEnable(GL_LINE_SMOOTH)
  else
    glDisable(GL_LINE_SMOOTH);

  if FRenderPrefs.SmoothPolygons then
    glEnable(GL_POLYGON_SMOOTH)
  else
    glDisable(GL_POLYGON_SMOOTH);

  if FRenderPrefs.SmoothPoints then
    glEnable(GL_POINT_SMOOTH)
  else
    glDisable(GL_POINT_SMOOTH);
end;

procedure TfrmGLRender.CreateGLWindow;
begin
  glMap := TglWindow.Create(Self);

  glMap.OnClick := glMapClick;
  glMap.OnMouseDown := glMapMouseDown;
  glMap.OnMouseMove := glMapMouseMove;
  glMap.OnResize := glMapResize;
  glMap.OnDraw := glMapDraw;
  glMap.OnInit := glMapInit;

  glMap.Parent := pnlMap;
  glMap.Align := alClient;
  glMap.ColorDepth := c16bits;
  glMap.DepthBufferEnabled := False;
  glMap.WindowFlags := [wfDrawToWindow, wfSupportOpenGL, wfGenericAccelerated, wfDoubleBuffer];
  glMap.PopupMenu := pumRadar;

  glMap.Initialize;
end;

procedure TfrmGLRender.AdjustMobTriangleSize;
var
  iSize:  integer;
  dwDiv:  Cardinal;
begin
  if FRenderPrefs.ScaleMobTriangle then begin
    dwDiv := 6000 div FRenderPrefs.MobTriangleNom;

    if dwDiv = 0 then
      dwDiv := 1;

    iSize := FRange div dwDiv;
    if iSize > FRenderPrefs.MobTriangleMax then
      iSize := FRenderPrefs.MobTriangleMax
    else if iSize < FRenderPrefs.MobTriangleMin then
      iSize := FRenderPrefs.MobTriangleMin;
  end
  else
    iSize := FRenderPrefs.MobTriangleNom;

  FMobTriangle.Size := iSize;
  FBoat.Size := 2 * iSize;

  if Visible then begin
    FMobTriangle.GLCleanup;
    FMobTriangle.GLInitialize;
    FBoat.GLCleanup;
    FBoat.GLInitialize;
    Dirty;
  end;
end;

procedure TfrmGLRender.RENDERPrefsMobTriangleSizeChanged(Sender: TObject);
begin
  AdjustMobTriangleSize;
end;

function TfrmGLRender.ZDeltaStr(AObj: TDAOCObject; AVerbose: boolean): string;
var
  iZDelta:    integer;
begin
  iZDelta := AObj.Z - FCurrConn.LocalPlayer.Z;
  if iZDelta < 0 then begin
    if AVerbose then
      Result := IntToStr(-iZDelta) + ' below you'
    else
      Result := IntToStr(iZDelta);
  end
  else begin
    if AVerbose then
      if iZDelta = 0 then
        Result := 'Even Z'
      else
        Result := IntToStr(iZDelta) + ' above you'
    else
      Result := '+' + IntToStr(iZDelta);
  end;
end;

procedure TfrmGLRender.CheckMouseOverUnproject;
begin
  if (FRenderPrefs.EasyMouseOvers and (GetForegroundWindow = Handle)) or
    (GetAsyncKeyState(VK_CONTROL) <> 0) then
    MapUnproject(FMouseLocX, FMouseLocY, false)
  else begin
    FMouseLocX := 0;
    FMouseLocY := 0;
  end;
end;

procedure TfrmGLRender.DAOCDoorPositionUpdate(Sender: TObject; ADoor: TDAOCObject);
begin
  if Sender <> FCurrConn then exit;

  DAOCUpdateObject(Sender, ADoor);
end;

procedure TfrmGLRender.SetDAOCConnectionList(const Value: TDAOCConnectionList);
begin
  FDAOCConnectionList := Value;
  AutoSelectConnection;
end;

procedure TfrmGLRender.NextConnection;
var
  iIdx: integer;
begin
  if FDAOCConnectionList.Count < 2 then
    exit;

  iIdx := FDAOCConnectionList.IndexOf(FCurrConn);
  inc(iIdx);

  if iIdx >= FDAOCConnectionList.Count then
    iIdx := 0;

  SetCurrentConnection(FDAOCConnectionList[iIdx]);
end;

procedure TfrmGLRender.PreviousConnection;
var
  iIdx: integer;
begin
  if FDAOCConnectionList.Count < 2 then
    exit;

  iIdx := FDAOCConnectionList.IndexOf(FCurrConn);
  dec(iIdx);

  if iIdx < 0 then
    iIdx := FDAOCConnectionList.Count - 1;

  SetCurrentConnection(FDAOCConnectionList[iIdx]);
end;

procedure TfrmGLRender.SetCurrentConnection(AConn: TDAOCConnection);
begin
  if FCurrConn = AConn then
    exit;

  FCurrConn := AConn;

  if Assigned(FCurrConn) then begin
      { BRY: I call these in kind of an arbitrary order according to how
        they would be called in normal life, this might need to be adjusted }
    DAOCCharacterLogin(AConn);
    DAOCRegionChanged(AConn);
    DAOCZoneChanged(AConn);
    DAOCPlayerPosUpdate(AConn);

    RefreshFilteredList;
    tmrMinFPS.Enabled := true;
  end

    { no connection.  Clean up }
  else begin
    FFilteredObjects.Clear;
    SetObjectListRowCount(0);
    UpdateObjectCounts;
    tmrMinFPS.Enabled := false;
  end;

  Dirty;
end;

procedure TfrmGLRender.DrawNoConnection;
begin
  { called when no connection is set, just setup a screen sized world and
    spit out some text }
  glClear(GL_COLOR_BUFFER_BIT);

  SetupScreenProjectionMatrix;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glDisable(GL_BLEND);
  glDisable(GL_LIGHTING);
  
  glColor3f(1.0, 1.0, 1.0);
  WriteGLUTTextH12(5, glMap.ClientHeight, 'No connection assigned.');
end;

procedure TfrmGLRender.RENDERPrefsMinFPSChanged(Sender: TObject);
begin
  AdjustMinFPSTimer;
end;

procedure TfrmGLRender.AdjustMinFPSTimer;
begin
  tmrMinFPS.Interval := (1000 div FRenderPrefs.MinFPS);
end;

procedure TfrmGLRender.DAOCDisconnect(Sender: TObject);
begin
  SaveSettings;
  
  if FCurrConn = Sender then
    AutoSelectConnection;
  UpdateCaption;
end;

procedure TfrmGLRender.DAOCConnect(Sender: TObject);
begin
  LoadSettings;
  
  if not Assigned(FCurrConn) then
    AutoSelectConnection;
  UpdateCaption;
end;

procedure TfrmGLRender.AutoSelectConnection;
var
  iActiveIdx:   integer;
begin
    { if theres any active connections in this list, then select the last one }
  for iActiveIdx := FDAOCConnectionList.Count - 1 downto 0 do
    if FDAOCConnectionList[iActiveIdx].Active then begin
      SetCurrentConnection(FDAOCConnectionList[iActiveIdx]);
      exit;
    end;

    { else clear the current connection, because there isn't one }
  if Assigned(FCurrConn) then
    SetCurrentConnection(nil);
end;

procedure TfrmGLRender.UpdateCaption;
var
  sCaption:   string;
begin
  if Assigned(FCurrConn) then
    sCaption := FCurrConn.LocalPlayer.Name
  else
    sCaption := '';

  sCaption := sCaption + S_CAPTION_SUFFIX;

  if Assigned(FDAOCConnectionList) and (FDAOCConnectionList.ActiveCount > 1) then
    sCaption := sCaption + ', '  + IntToStr(FDAOCConnectionList.ActiveCount) +
      ' active connections (F11 to toggle)';
  Caption := sCaption;
end;

procedure TfrmGLRender.LoadSettings;
begin
  if not FPrefsLoaded and (FPrefsFile <> '') then begin
    FRenderPrefs.LoadSettings(FPrefsFile);
    FRangeCircles.LoadFromFile(FPrefsFile);
    Left := FRenderPrefs.Left;
    Top := FRenderPrefs.Top;
    Width := FRenderPrefs.Width;
    Height := FRenderPrefs.Height;
    FRange := FRenderPrefs.Range;
    slideZoom.Position := FRange;
    glMap.AutoMakeCurrent := FRenderPrefs.AutoMakeCurrent;

    FMapTexturesListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;
    FMapElementsListList.AttemptDownload := FRenderPrefs.AttemptMapDownload;
    FPrefsLoaded := true;
  end
  else
    slideZoomChange(Self);
end;

procedure TfrmGLRender.SaveSettings;
begin
  if FPrefsFile <> '' then begin
    FRenderPrefs.Left := Left;
    FRenderPrefs.Top := Top;
    FRenderPrefs.Width := Width;
    FRenderPrefs.Height := Height;
    FRenderPrefs.Range := FRange;
      { turn off attempt download if continuous failure }
    FRenderPrefs.AttemptMapDownload := FRenderPrefs.AttemptMapDownload and
      FMapTexturesListList.AttemptDownload and FMapElementsListList.AttemptDownload;

    FRenderPrefs.SaveSettings(FPrefsFile);
    FRangeCircles.SaveToFile(FPrefsFile);
  end;
end;

procedure TfrmGLRender.mniShowZoneInfoClick(Sender: TObject);
begin
  if Assigned(FCurrConn) and Assigned(FCurrConn.Zone) then
    ShowMessage(FCurrConn.Zone.AsString);
end;

procedure TfrmGLRender.pumRadarPopup(Sender: TObject);
begin
  mniShowZoneInfo.Enabled := Assigned(FCurrConn) and Assigned(FCurrConn.Zone);
end;

procedure TfrmGLRender.UpdateInventoryPanel(AForce: boolean);
var
  pObj:   TDAOCMovingObject;
begin
  if Assigned(FCurrConn) and Assigned(FCurrConn.SelectedObject) and
      { the idea here is that ShowPlayerInventory only works on ocPlayer,
        and Force works on Mobs, Players, UnknownMoving, and LocalPlayer }
    (
      (FRenderPrefs.ShowPlayerInventory and (FCurrConn.SelectedObject.ObjectClass = ocPlayer))
    or
      (AForce and (FCurrConn.SelectedObject.ObjectClass in [ocUnknown, ocMob, ocPlayer, ocLocalPlayer]))
    ) then begin

    pObj := TDAOCMovingObject(FCurrConn.SelectedObject);
    lblInventory.Caption := pObj.Inventory.AsString(false);
    pnlInventory.Height := lblInventoryHeader.Height + (pObj.Inventory.Count * 13);
    pnlInventory.Visible := true;
  end
  else begin
    lblInventory.Caption := '';
    pnlInventory.Visible := false;
  end;
end;

procedure TfrmGLRender.DAOCMobInventoryChanged(Sender: TObject; AObj: TDAOCMovingObject);
begin
  if (Sender = FCurrConn) and (AObj = FCurrConn.SelectedObject) then
    UpdateInventoryPanel(false);
end;

procedure TfrmGLRender.mniForceContextCurrentClick(Sender: TObject);
begin
  glMap.MakeCurrent;
end;

procedure TfrmGLRender.atnShowInventoryExecute(Sender: TObject);
begin
    { force to show if not already visible }
  UpdateInventoryPanel(not pnlInventory.Visible);
end;

procedure TfrmGLRender.atnInvaderWarningToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawMapTexture := not FRenderPrefs.DrawMapTexture;
  Dirty;
end;

procedure TfrmGLRender.atnDrawTextureToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawMapTexture := not FRenderPrefs.DrawMapTexture;
  Dirty;
end;

procedure TfrmGLRender.atnDrawRangeCirclesToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawRangeCircles := not FRenderPrefs.DrawRangeCircles;
  Dirty;
end;

procedure TfrmGLRender.atnDrawAIDestinationToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawAIDestination := not FRenderPrefs.DrawAIDestination;
  Dirty;
end;

procedure TfrmGLRender.atnDrawFriendlyPlayersToggleExecute(
  Sender: TObject);
begin
  FRenderPrefs.DrawFriendlyPlayers := not FRenderPrefs.DrawFriendlyPlayers;
  Dirty;
end;

procedure TfrmGLRender.atnDrawGridToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawGrid := not FRenderPrefs.DrawGrid;
  Dirty;
end;

procedure TfrmGLRender.atnDrawHUDToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawHUD := not FRenderPrefs.DrawHUD;
  Dirty;
end;

procedure TfrmGLRender.atnAddPushPinExecute(Sender: TObject);
begin
  AddPushPin;
end;

procedure TfrmGLRender.atnDrawMobsToggleExecute(Sender: TObject);
begin
  FRenderPrefs.XORObjectClassFilter(ocMob);
end;

procedure TfrmGLRender.atnDrawObjectsToggleExecute(Sender: TObject);
begin
  FRenderPrefs.XORObjectClassFilter(ocObject);
end;

procedure TfrmGLRender.atnDrawPlayersToggleExecute(Sender: TObject);
begin
  FRenderPrefs.XORObjectClassFilter(ocPlayer);
end;

procedure TfrmGLRender.atnDrawRulersToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawRulers := not FRenderPrefs.DrawRulers;
  Dirty;
end;

procedure TfrmGLRender.atnStayOnTopToggleExecute(Sender: TObject);
begin
  FRenderPrefs.StayOnTop := not FRenderPrefs.StayOnTop;
  UpdateStayOnTop;
end;

procedure TfrmGLRender.atnDrawUnknownToggleExecute(Sender: TObject);
begin
  FRenderPrefs.XORObjectClassFilter(ocUnknown);
end;

procedure TfrmGLRender.atnDrawTypeTagToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawTypeTag := not FRenderPrefs.DrawTypeTag;
  FRenderPrefs.AlternateMobListText := FRenderPrefs.DrawTypeTag;
  lstObjects.Invalidate;
  Dirty;
end;

procedure TfrmGLRender.atnDrawVectorToggleExecute(Sender: TObject);
begin
  FRenderPrefs.DrawMapVector := not FRenderPrefs.DrawMapVector;
  Dirty;
end;

procedure TfrmGLRender.atnNextConnectionExecute(Sender: TObject);
begin
  NextConnection;
end;

procedure TfrmGLRender.atnPreviousConnectionExecute(Sender: TObject);
begin
  PreviousConnection;
end;

procedure TfrmGLRender.atnZoomInExecute(Sender: TObject);
begin
  if lstObjects.Focused then
    exit;
  slideZoom.Position := slideZoom.Position - slideZoom.PageSize;
end;

procedure TfrmGLRender.atnZoomOutExecute(Sender: TObject);
begin
  if lstObjects.Focused then
    exit;
  slideZoom.Position := slideZoom.Position + slideZoom.PageSize;
end;

procedure TfrmGLRender.atnRecenterMapExecute(Sender: TObject);
begin
  FMapToPlayerOffset.X := 0;
  FMapToPlayerOffset.Y := 0;
end;

procedure TfrmGLRender.atnScrollLeftExecute(Sender: TObject);
begin
  dec(FMapToPlayerOffset.X, FRange div 10);
end;

procedure TfrmGLRender.atnScrollRightExecute(Sender: TObject);
begin
  inc(FMapToPlayerOffset.X, FRange div 10);
end;

procedure TfrmGLRender.atnScrollUpExecute(Sender: TObject);
begin
  dec(FMapToPlayerOffset.Y, FRange div 10);
end;

procedure TfrmGLRender.atnScrollDownExecute(Sender: TObject);
begin
  inc(FMapToPlayerOffset.Y, FRange div 10);
end;

procedure TfrmGLRender.atnShowPrefsDialogExecute(Sender: TObject);
begin
  DoPrefsDialog;
end;

procedure TfrmGLRender.atnShowHideMobListExecute(Sender: TObject);
begin
  pnlLeft.Visible := not pnlLeft.Visible;
end;

procedure TfrmGLRender.atnScreenShotExecute(Sender: TObject);
var
  bmp:    TBitmap;
  jpg:    TJPEGImage;
begin
  bmp := TBitmap.Create;
  bmp.Width := glMap.ClientWidth;
  bmp.Height := glMap.ClientHeight;
  bmp.PixelFormat := pf24Bit;

  glReadPixels(0, 0, bmp.Width, bmp.Height, GL_BGR, GL_UNSIGNED_BYTE, bmp.ScanLine[bmp.Height - 1]);

  jpg := TJPEGImage.Create;
  jpg.Performance := jpBestQuality;
  jpg.CompressionQuality := 80;
  jpg.Smoothing := false;
  jpg.Assign(bmp);
  bmp.Free;
  jpg.JPEGNeeded;
  jpg.SaveToFile('screenshot.jpg');
  jpg.Free;
end;

end.


unit GLRender;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, glWindow, GL, GLU, GLext, DAOCControl, ComCtrls, DAOCObjs,
  StdCtrls, GLRenderObjects, MapElementList, Grids, DAOCRegion;

type
  TfrmGLRender = class(TForm)
    glMap: TglWindow;
    slideZoom: TTrackBar;
    grdObjects: TDrawGrid;
    procedure glMapDraw(Sender: TObject);
    procedure glMapInit(Sender: TObject);
    procedure glMapResize(Sender: TObject);
    procedure slideZoomChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure glMapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grdObjectsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdObjectsClick(Sender: TObject);
  private
    FDControl: TDAOCControl;
    FRange:     DWORD;
    FGLInitsCalled:     boolean;
    FDirty:   boolean;
    FMapElements:   TVectorMapElementList;
    FMapTextures:   TTextureMapElementList;
    FRangeCircles:  TRangeCircleList;
    FMobTriangle:     T3DArrowHead;
    FObjectTriangle:  T3DPyramid;

    procedure SetDControl(const Value: TDAOCControl);
    procedure Log(const s: string);
    procedure CheckGLError;
    procedure DrawMapRulers;
    procedure DrawRangeCircles;
    procedure DrawPlayerTriangle;
    procedure DrawLineToSelected;
    procedure DrawMobsAndPlayers;
    procedure DrawMapElements;
    procedure GLInits;
    procedure GLCleanups;
    procedure GridSelectObject(ADAOCObject: TDAOCObject);
    procedure DrawPlayerHighlightRing(ADAOCObject: TDAOCObject);
  protected
  public
    procedure DAOCAddObject(AObj: TDAOCObject);
    procedure DAOCDeleteObject(AObj: TDAOCObject);
    procedure DAOCUpdateObject(AObj: TDAOCObject);
    procedure DAOCSelectedObjectChanged(AObj: TDAOCObject);
    procedure DAOCRegionChanged;
    procedure DAOCZoneChanged;

    procedure Dirty;
    property DAOCControl: TDAOCControl read FDControl write SetDControl;
    property RangeCircles: TRangeCircleList read FRangeCircles;
  end;

var
  frmGLRender: TfrmGLRender;

implementation

uses DAOCConnection, Unit1;

const
  COL_NAME = 0;
  COL_LEVEL = 1;
  COL_HEALTH = 2;

{$R *.dfm}

function RealmColor(ARealm: TDAOCRealm) : TColor;
begin
  case ARealm of
    drFriend:   Result := clWhite;
    drAlbion:   Result := clRed;
    drMidgard:  Result := $efae00;
    drHibernia: Result := $33cc33;
    else
      Result := clFuchsia;
  end;
end;

{ TfrmGLRender }

procedure TfrmGLRender.SetDControl(const Value: TDAOCControl);
begin
  FDControl := Value;
end;

procedure TfrmGLRender.glMapDraw(Sender: TObject);
var
  minx:   integer;
  miny:   integer;
  maxx:   integer;
  maxy:   integer;
begin
  if not Assigned(FDControl) then
    exit;

  if not FGLInitsCalled then
    GLInits;

  // Clear the color and depth buffers
//  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
//  glColor3f(0.0, 1.0, 0.0);
//  glRectf(100, 100, 150, 150);

  glClear(GL_COLOR_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if Assigned(FDControl.Zone) and (FDControl.Zone.Rotate > 0) then
    glRotatef(FDControl.Zone.Rotate, 0, 0, 1);
  glRotatef(180, 1, 0, 0);

  minx := FDControl.LocalPlayer.X - FRange;
  maxx := FDControl.LocalPlayer.X + FRange;
  miny := FDControl.LocalPlayer.Y - FRange;
  maxy := FDControl.LocalPlayer.Y + FRange;
  glOrtho(minx, maxx, miny, maxy, 1, -200);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_LIGHTING);

    { at origin }
  DrawMapElements;
  DrawMobsAndPlayers;
  DrawLineToSelected;

  glTranslatef(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y, 0);

    { at player pos }
  DrawMapRulers;
  DrawRangeCircles;
  DrawPlayerTriangle;

  CheckGLError();

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
    raise Exception.Create('OpenGL 1.3 support required for terrain textures');

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
begin
  FRange := slideZoom.Position;
  glMap.ReDraw;
end;

procedure TfrmGLRender.FormShow(Sender: TObject);
begin
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
  headrad:  GLfloat;
begin
  glDisable(GL_LIGHTING);
  
    { Draw map rulers }
  glColor3f(0.45, 0.45, 0.45);
  glLineWidth(1.0);
  glBegin(GL_LINES);
    glVertex3f(-(FRange * 1.5), 0, 0);
    glVertex3f((FRange * 1.5), 0, 0);
    glVertex3f(0, -(FRange * 1.5), 0);
    glVertex3f(0, (FRange * 1.5), 0);

    headrad := ((FDControl.LocalPlayer.Head - 180) * PI) / 180;
    glVertex3f(0, 0, 0);
    glVertex3f(cos(headrad + PI / 2) * (FRange * 1.5),
      sin(headrad + PI / 2) * (FRange * 1.5), 0);
  glEnd();
end;

procedure TfrmGLRender.DrawPlayerTriangle;
begin
  glEnable(GL_LIGHTING);
  glColor3f(1, 1, 0);
  glRotatef(FDControl.LocalPlayer.Head - 180, 0, 0, 1);
  FMobTriangle.GLRender;
end;

procedure TfrmGLRender.DrawRangeCircles;
begin
  glDisable(GL_LIGHTING);
  FRangeCircles.GLRender;
end;

procedure TfrmGLRender.Dirty;
begin
  if not FDirty then begin
    FDirty := true;
    glMap.Invalidate;
  end;
end;

procedure TfrmGLRender.DAOCAddObject(AObj: TDAOCObject);
begin
  grdObjects.RowCount := FDControl.DAOCObjects.Count + 1;
  Dirty;
end;

procedure TfrmGLRender.DAOCDeleteObject(AObj: TDAOCObject);
begin
  grdObjects.RowCount := FDControl.DAOCObjects.Count + 1;
  Dirty;
end;

procedure TfrmGLRender.DAOCUpdateObject(AObj: TDAOCObject);
begin
  grdObjects.Invalidate;
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
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, @modmatrix);
  glGetIntegerv(GL_VIEWPORT, @viewport);

  pt := glMap.ScreenToClient(Mouse.CursorPos);
  gluUnProject(pt.X, glMap.Height - pt.Y, 0, modmatrix, projmatrix, viewport,
    @x, @y, @z);

  pNearest := FDControl.DAOCObjects.FindNearest(trunc(x), trunc(y), FDControl.LocalPlayer.Z);
  if Assigned(pNearest) then
      { callback will update screen }
    FDControl.SelectedObject := pNearest;
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
    glVertex3i(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y, 0);  // c->playerz
    glVertex3i(pSelected.X, pSelected.Y, 0);  // m->getZ()
    glEnd();
  end;
end;

procedure TfrmGLRender.DrawMobsAndPlayers;
var
  I:    integer;
  clMob:  TColor;
  pMob:   TDAOCObject;
begin
  for I := 0 to FDControl.DAOCObjects.Count - 1 do begin
    pMob := FDControl.DAOCObjects[I];

    if pMob.ObjectClass in [ocUnknown, ocMob, ocPlayer] then begin
      glPushMatrix();
      glTranslatef(pMob.X, pMob.Y, 0);
      glRotatef(pMob.Head - 180, 0, 0, 1);

      if pMob.ObjectClass = ocPlayer then
        DrawPlayerHighlightRing(pMob);

      clMob := pMob.GetConColor(FDControl.LocalPlayer.Level);
      if pMob.Stealthed then
        clMob := clGreen;
      glColor3ubv(PGLubyte(@clMob));
      FMobTriangle.GLRender;

      glPopMatrix();

        { if the mob is on the move, draw a line to its destination }
      if (pMob.DestinationX <> 0) and (pMob.DestinationY <> 0) then begin
        glLineWidth(2.0);
        glColor4f(1, 0, 0, 0.8);
        glBegin(GL_LINES);
          glVertex3f(pMob.X, pMob.Y, 0);
          glVertex3f(pMob.DestinationX, pMob.DestinationY, 0);
        glEnd();
      end;  { if destinaton set }
    end;  { if a class to draw }

    if pMob.ObjectClass = ocObject then begin
      glPushMatrix();
      glTranslatef(pMob.X, pMob.Y, 0);
      FObjectTriangle.GLRender;
      glPopMatrix();
    end;
  end;  { for each object }
end;

procedure TfrmGLRender.FormCreate(Sender: TObject);
var
  rngCircle:  TRangeCircle;
begin
  FRangeCircles := TRangeCircleList.Create;
  rngCircle := TRangeCircle.CreateRange(500);
  rngCircle.Color := clRed;
  FRangeCircles.Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(1500);
  rngCircle.Color := clLime;
  FRangeCircles.Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(7000);
  rngCircle.Color := clSilver;
  FRangeCircles.Add(rngCircle);

  FMapElements := TVectorMapElementList.Create;
  FMapTextures := TTextureMapElementList.Create;
  FMobTriangle := T3DArrowHead.Create;
  FObjectTriangle := T3DPyramid.Create;

  grdObjects.DoubleBuffered := true;
end;

procedure TfrmGLRender.FormDestroy(Sender: TObject);
begin
  GLCleanups;
  FMobTriangle.Free;
  FObjectTriangle.Free;
  FMapElements.Free;
  FMapTextures.Free;
  FRangeCircles.Free;
end;

procedure TfrmGLRender.GLCleanups;
begin
  FMobTriangle.GLCleanup;
  FObjectTriangle.GLCleanup;
  FRangeCircles.GLCleanup;
  FMapElements.GLCleanup;
  FMapTextures.GLCleanup;
end;

procedure TfrmGLRender.GLInits;
begin
  FGLInitsCalled := true;
  FMobTriangle.GLInitialize;
  FObjectTriangle.GLInitialize;
  FRangeCircles.GLInitialize;
  FMapElements.GLInitialize;
  FMapTextures.GLInitialize;
  
  CheckGLError;
end;

procedure TfrmGLRender.DrawMapElements;
begin
  glPushAttrib(GL_ENABLE_BIT);

  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDisable(GL_BLEND);

  glEnable(GL_TEXTURE_2D);
  FMapTextures.GLRender;
  glDisable(GL_TEXTURE_2D);
  
  FMapElements.GLRender;

  glPopAttrib();
end;

procedure TfrmGLRender.DAOCZoneChanged;
begin
  if not Assigned(FDControl.Zone) then
    exit;

  { BRY:  We really need to select the GL context here, but I don't because
    glWindow doesn't have a function to activate its context and we'll just
    see if this works }
     
    { cleanup any old objects }
  FMapElements.GLCleanup;

  FMapElements.OffsetX := FDControl.Zone.BaseLoc.X;
  FMapElements.OffsetY := FDControl.Zone.BaseLoc.Y;
  FMapElements.LoadFromFile('maps\' + FDControl.Zone.MapName);
  FMapElements.GLInitialize;

  FMapTextures.GLCleanup;
  FMapTextures.OffsetX := FDControl.Zone.BaseLoc.X;
  FMapTextures.OffsetY := FDControl.Zone.BaseLoc.Y;
  FMapTextures.LoadFromFile(Format('maps\dds\zone%3.3d.dds', [FDControl.Zone.ZoneNum]));
  FMapTextures.GLInitialize;
end;

procedure TfrmGLRender.DAOCRegionChanged;
begin
  ;
end;

procedure TfrmGLRender.grdObjectsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  pMob:   TDAOCObject;
  cl:     TColor;
  R,G,B:  BYTE;
  sText:  string;
begin
  if ARow = 0 then
    with grdObjects.Canvas do begin
      Brush.Color := clBtnFace;
      Font.Color := clBtnText;

      FillRect(Rect);
      case ACol of
        COL_NAME:   TextOut(Rect.Left + 2, Rect.Top + 1, 'Name');
        COL_LEVEL:  TextOut(Rect.Left + 2, Rect.Top + 1, 'Level');
        COL_HEALTH: TextOut(Rect.Left + 2, Rect.Top + 1, 'Health');
      end;
    end  { Row 0 / with }

  else if ARow <= FDControl.DAOCObjects.Count then
    with grdObjects.Canvas do begin
      pMob := FDControl.DAOCObjects[ARow - 1];

        { objects are gray on white }
      if pMob.ObjectClass = ocObject then begin
        Font.Color := clGray;
        Brush.Color := clWhite;
      end  { object }

        { mobs are con color on white }
      else if pMob.Realm = drFriend then begin
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
        Brush.Color := RealmColor(pMob.Realm)
      end;

      case ACol of
        COL_NAME: sText := pMob.Name;
        COL_LEVEL: sText := IntToStr(pMob.Level);
        COL_HEALTH:
          if pMob is TDAOCMovingObject then
            sText := IntToStr(TDAOCMovingObject(pMob).HitPoints)
          else
            sText := '';
        else
          sText := 'col' + IntToStr(ACol);
      end;

      FillRect(Rect);
      TextOut(Rect.Left + 2, Rect.Top + 2, sText);

      if gdSelected in State then begin
        Pen.Color := clBlack; // $00ffcc;
        case ACol of
          COL_NAME:
            begin
              MoveTo(Rect.Right, Rect.Bottom-1);
              LineTo(Rect.Left, Rect.Bottom-1);
              LineTo(Rect.Left, Rect.Top);
              LineTo(Rect.Right, Rect.Top);
            end;
          COL_HEALTH:
            begin
              MoveTo(Rect.Left, Rect.Bottom-1);
              LineTo(Rect.Right, Rect.Bottom-1);
              LineTo(Rect.Right, Rect.Top);
              LineTo(Rect.Left, Rect.Top);
            end;
          else
            begin
              MoveTo(Rect.Left, Rect.Bottom-1);
              LineTo(Rect.Right, Rect.Bottom-1);
              MoveTo(Rect.Left, Rect.Top);
              LineTo(Rect.Right, Rect.Top);
            end;
        end;
      end;  { if selected }
    end  { data row / with }

      { blank row }
    else begin
      grdObjects.Canvas.Brush.Color := clWhite;
      grdObjects.Canvas.FillRect(Rect);
    end;
end;

procedure TfrmGLRender.grdObjectsClick(Sender: TObject);
begin
  if grdObjects.Row < 1 then
    exit;

  if grdObjects.Row <= FDControl.DAOCObjects.Count then
    FDControl.SelectedObject := FDControl.DAOCObjects[grdObjects.Row - 1];

  Dirty;
end;

procedure TfrmGLRender.GridSelectObject(ADAOCObject: TDAOCObject);
var
  I:    integer;
begin
  for I := 0 to FDControl.DAOCObjects.Count - 1 do
    if FDControl.DAOCObjects[I] = ADAOCObject then
      if (I + 1) < grdObjects.RowCount then begin
        grdObjects.Row := I + 1;
        exit;
      end;
end;

procedure TfrmGLRender.DAOCSelectedObjectChanged(AObj: TDAOCObject);
begin
  GridSelectObject(AObj);
  Dirty;
end;

procedure TfrmGLRender.DrawPlayerHighlightRing(ADAOCObject: TDAOCObject);
var
  fSize:  GLfloat;
begin
  fSize :=  FMobTriangle.Size * 1.33;
  SetGLColorFromTColor(RealmColor(ADAOCObject.Realm), 1);

  glBegin(GL_TRIANGLES);
    glNormal3f(0, 0, 1);
    glVertex3f(-fSize, -fSize, 0);
    glVertex3F(0, 2 * fSize, 0);
    glVertex3f(fSize, -fSize, 0);
  glEnd();
end;

end.


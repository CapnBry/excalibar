unit GLRender;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, glWindow, OpenGL12, DAOCControl, ComCtrls, DAOCObjs,
  StdCtrls;

type
  TfrmGLRender = class(TForm)
    glMap: TglWindow;
    slideZoom: TTrackBar;
    lstMobs: TListBox;
    procedure glMapDraw(Sender: TObject);
    procedure glMapInit(Sender: TObject);
    procedure glMapResize(Sender: TObject);
    procedure slideZoomChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure glMapClick(Sender: TObject);
  private
    FDControl: TDAOCControl;
    FRange:     DWORD;
    FCallListsCreated:  boolean;
    listTriangle: TGLuint;
    FDirty:   boolean;
    FImage:   TGLuint;

    procedure SetDControl(const Value: TDAOCControl);
    procedure CreateCallLists;
    procedure Log(const s: string);
    procedure CheckGLError;
    procedure DrawMapRulers;
    procedure DrawRangeCircles;
    procedure DrawPlayerTriangle;
    procedure DrawLineToSelected;
  protected
    procedure LoadRAWImage(const AFName: string);
  public
    procedure AddDAOCObject(AObj: TDAOCObject);
    procedure DeleteDAOCObject(AObj: TDAOCObject);
    procedure UpdateDAOCObject(AObj: TDAOCObject);

    procedure Dirty;
    property DAOCControl: TDAOCControl read FDControl write SetDControl;
  end;

var
  frmGLRender: TfrmGLRender;

implementation

uses DAOCConnection, Unit1;

{$R *.dfm}

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

  clMob:  TColor;
  I:    integer;
  pMob: TDAOCObject;
begin
  if not Assigned(FDControl) then
    exit;

  if not FCallListsCreated then
    CreateCallLists;

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
  glOrtho(minx, maxx, miny, maxy, 500, -500);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_LIGHTING);

  for I := 0 to FDControl.DAOCObjects.Count - 1 do begin
    pMob := FDControl.DAOCObjects[I];

    if pMob.ObjectClass in [ocUnknown, ocMob, ocPlayer] then begin
      glPushMatrix();
      glTranslatef(pMob.X, pMob.Y, 0);
      glRotatef(pMob.Head - 180, 0, 0, 1);

      clMob := pMob.GetConColor(FDControl.LocalPlayer.Level);
      if pMob.Stealthed then
        clMob := clGreen;
      glColor3ubv(PGLubyte(@clMob));
      glCallList(listTriangle);
      glPopMatrix();
    end;
  end;  { for each object }

  DrawLineToSelected();

  glPushMatrix();
  glTranslatef(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y, 0);
  DrawMapRulers;
  DrawRangeCircles;
  DrawPlayerTriangle;
  if FImage <> 0 then begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FImage);
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glBegin(GL_QUADS);
      glTexCoord2f(1.0, 0.0);
      glVertex3f(75, -75, 0.0);
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-75, -75, 0.0);
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-75, 75, 0.0);
      glTexCoord2f(1.0, 1.0);
      glVertex3f(75, 75, 0.0);
    glEnd();
    glDisable(GL_TEXTURE_2D);
  end;
  glPopMatrix();

  CheckGLError();

  FDirty := false;
end;

procedure TfrmGLRender.glMapInit(Sender: TObject);
const
  lightpos: array[0..3] of TGLfloat = (0.5, -1.0, 1.0, 0.0);
  diffuse: array[0..3] of TGLfloat = (0.5, 0.5, 0.5, 1.0);
  ambient: array[0..3] of TGLfloat = (0.0, 0.0, 0.0, 1.0);
  material: array[0..3] of TGLfloat = (0.5, 0.5, 0.5, 1.0);
begin
  InitOpenGL;

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

  listTriangle := glGenLists(1);
  FCallListsCreated := false;

  // LoadRAWImage('\temp\sword.raw');
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

procedure TfrmGLRender.CreateCallLists;
var
  w:  integer;
  l:  integer;
begin
  w := 75;
  l := w * 2;

  glEdgeFlag(ByteBool(GL_TRUE));
  glNewList(listTriangle, GL_COMPILE);

  glBegin(GL_TRIANGLE_FAN);
    glVertex3i(0, 0, w);

    glNormal3f(-(l+w), w, l);
    glVertex3i(-w, -w, 0);
    glVertex3i(0, l, 0);

    glNormal3f(l+w, w, l);
    glVertex3i(w, -w, 0);

    glNormal3f(0, -w, w);
    glVertex3i(-w, -w, 0);
  glEnd();
  glEndList();

  CheckGLError;
end;

procedure TfrmGLRender.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmGLRender.CheckGLError;
var
  err:  TGLenum;
begin
  err := glGetError();
  if err <> GL_NO_ERROR then
    Log(string(gluErrorString(err)));
end;

procedure TfrmGLRender.DrawMapRulers;
var
  headrad:  TGLfloat;
begin
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
  glCallList(listTriangle);
end;

procedure TfrmGLRender.DrawRangeCircles;
begin
;
end;

procedure TfrmGLRender.Dirty;
begin
  if not FDirty then begin
    FDirty := true;
    glMap.Invalidate;
  end;
end;

procedure TfrmGLRender.AddDAOCObject(AObj: TDAOCObject);
begin
  lstMobs.Items.AddObject(Format('%s (%d)', [AObj.Name, AObj.Level]), AObj);
  Dirty;
end;

procedure TfrmGLRender.DeleteDAOCObject(AObj: TDAOCObject);
var
  iIdx:   integer;
begin
  for iIdx := 0 to lstMobs.Items.Count - 1 do
    if lstMobs.Items.Objects[iIdx] = AObj then begin
      lstMobs.Items.Delete(iIdx);
      Dirty;
      exit;
    end;
end;

procedure TfrmGLRender.UpdateDAOCObject(AObj: TDAOCObject);
begin
  Dirty;
end;

procedure TfrmGLRender.LoadRAWImage(const AFName: string);
var
  FS:   TFileStream;
  MS:   TMemoryStream;
begin
  FS := TFileStream.Create(AFName, fmOpenRead or fmShareDenyNone);
  MS := TMemoryStream.Create;
  MS.CopyFrom(FS, 0);
  FS.Free;

  if FImage = 0 then
    glGenTextures(1, @FImage);
  glBindTexture(GL_TEXTURE_2D, FImage);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, MS.Memory);
  MS.Free;
end;

procedure TfrmGLRender.glMapClick(Sender: TObject);
var
  projmatrix: TMatrix4d;
  modmatrix:  TMatrix4d;
  viewport:   TVector4i;
  x, y, z:    TGLdouble;
  pt:   TPoint;
  pNearest:   TDAOCObject;
begin
  glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, @modmatrix);
  glGetIntegerv(GL_VIEWPORT, @viewport);

  pt := glMap.ScreenToClient(Mouse.CursorPos);
  gluUnProject(pt.X, glMap.Height - pt.Y, 100, modmatrix, projmatrix, viewport,
    @x, @y, @z);

  pNearest := FDControl.DAOCObjects.FindNearest(trunc(x), trunc(y), FDControl.LocalPlayer.Z);
  if Assigned(pNearest) then begin
    FDControl.SelectedObject := pNearest;
    Dirty;
  end;
end;

procedure TfrmGLRender.DrawLineToSelected;
var
  pSelected:  TDAOCObject;
begin
  pSelected := FDControl.SelectedObject;
  if Assigned(pSelected) then begin
    glColor3f(1.0, 1.0, 1.0);
    glLineWidth(2.0);
    glBegin(GL_LINES);
    glVertex3i(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y, 0);  // c->playerz
    glVertex3i(pSelected.X, pSelected.Y, 0);  // m->getZ()
    glEnd();
  end;
end;

end.


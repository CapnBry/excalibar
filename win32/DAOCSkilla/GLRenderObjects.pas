unit GLRenderObjects;

interface

uses
  Types, Windows, SysUtils, Classes, Graphics, Contnrs, GL, GLext, GLU, 
  DDSImage, Intersections, QuickSinCos, INIFiles, GLUT, TGA2;

type
  TGLRenderObject = class(TObject)
  private
  protected
    FInitialized:   boolean;
    FColor:   TColor;
    FName:    string;
    FOffsetY: integer;
    FOffsetX: integer;
    FUseColor: boolean;
    function ColorToStr : string;
  public
    constructor Create; virtual;

    procedure SetColorFromString(const AColor: string);
    function ToString : string; virtual;

    procedure GLInitialize; virtual;
    procedure GLRender(const ARenderBounds: TRect); virtual;
    procedure GLCleanup; virtual;

    property Color: TColor read FColor write FColor;
    property Name: string read FName write FName;
    property OffsetX: integer read FOffsetX write FOffsetX;
    property OffsetY: integer read FOffsetY write FOffsetY;
  end;

  TGLCallListObject = class(TGLRenderObject)
  protected
    FGLList:    GLuint;
  public
    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;
    procedure GLCleanup; override;
  end;

  TRangeCircle = class(TGLCallListObject)
  private
    FRange:       integer;
    FSmoothness:  integer;
    procedure SetSmoothness(const Value: integer);
    procedure SetRange(const Value: integer);
  public
    constructor Create; override;
    constructor CreateRange(ARange, ASmoothness: integer);

    procedure GLInitialize; override;

    property Range: integer read FRange write SetRange;
    property Smoothness: integer read FSmoothness write SetSmoothness;
  end;

  TRangeCircleList = class(TGLRenderObject)
  private
    FList:  TObjectList;
    function GetItems(I: integer): TRangeCircle;
    function GetCount: integer;
    procedure CreateDefaultRangeCircles;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;
    procedure GLCleanup; override;

    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    procedure Add(ACircle: TRangeCircle);
    procedure Delete(AIndex: integer);
    property Items[I: integer]: TRangeCircle read GetItems; default;
    property Count: integer read GetCount;
  end;

  TMapElementPoint = class(TGLRenderObject)
  private
    FY: GLuint;
    FX: GLuint;
    FZ: GLuint;
  public
    constructor Create; override;
    procedure GLRender(const ARenderBounds: TRect); override;

    procedure Assign(AX, AY, AZ: GLuint);
    function ToString : string; override;

    property X: GLuint read FX write FX;
    property Y: GLuint read FY write FY;
    property Z: GLuint read FZ write FZ;
  end;

  PMapElementLinePoint = ^TMapElementLinePoint;
  TMapElementLinePoint = record
    X, Y, Z:    GLint;
  end;

  TMapElementLine = class(TGLCallListObject)
  private
    FBounds: TRect;
  protected
    FPoints:    TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GLInitialize; override;

    procedure AddPoint(X, Y, Z: GLint);
    procedure ClearPoints;

    property Bounds: TRect read FBounds;
  end;

  TMapElementTerrrainTexture = class(TGLRenderObject)
  private
    FGLTexture:   GLuint;
    FDDSChunk:    TDDSImagePixelsChunk;
    FBounds: TRect;

    procedure UploadTexture;
  public
    destructor Destroy; override;

    procedure GLRender(const ARenderBounds: TRect); override;
    procedure GLCleanup; override;

    procedure TakeDDSChunk(X, Y, AScale: integer; ADDSChunk: TDDSImagePixelsChunk);

    property Bounds: TRect read FBounds;
  end;

  T3DArrowHead = class(TGLCallListObject)
  private
    FSize: integer;
    procedure SetSize(const Value: integer);
  public
    constructor Create; override;
    procedure GLInitialize; override;

    property Size: integer read FSize write SetSize;
  end;

  T3DPyramid = class(TGLCallListObject)
  private
    FSize: integer;
    procedure SetSize(const Value: integer);
  public
    constructor Create; override;
    procedure GLInitialize; override;

    property Size: integer read FSize write SetSize;
  end;

  TGLBullsEye = class(TGLCallListObject)
  private
    FSize: integer;
    FY: GLuint;
    FX: GLuint;
    procedure SetSize(const Value: integer);
  public
    constructor Create; override;

    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;

    procedure Assign(AX, AY: GLuint);
    property X: GLuint read FX write FX;
    property Y: GLuint read FY write FY;
    property Size: integer read FSize write SetSize;
  end;

  TGLFlatViewFrustum = class(TGLCallListObject)
  private
    FFOVy: integer;
    FZFar: integer;
  public
    constructor Create; override;
    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;

    property ZFar: integer read FZFar write FZFar;
    property FOVy: integer read FFOVy write FFOVy; // degrees!
  end;

  TGLBoat = class(TGLCallListObject)
  private
    FSize: integer;
    FAlpha: GLfloat;
    procedure SetSize(const Value: integer);
  public
    constructor Create; override;
    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;

    property Size: integer read FSize write SetSize;
    property Alpha: GLfloat read FAlpha write FAlpha;  // only for boat not sail
  end;

  TGLUnkownStealther = class(TGLCallListObject)
  private
    FAlpha: GLfloat;
    FSize: integer;
    procedure SetSize(const Value: integer);
  public
    constructor Create; override;
    procedure GLInitialize; override;
    procedure GLRender(const ARenderBounds: TRect); override;

    property Size: integer read FSize write SetSize;
    property Alpha: GLfloat read FAlpha write FAlpha;
  end;

  TGLPrescienceNode = class(TGLCallListObject)
  private
    FImage: GLuint;
    FImageFileName: string;
  public
    procedure GLInitialize; override;
    procedure GLCleanup; override;

    property ImageFileName: string read FImageFileName write FImageFileName;
  end;

procedure SetGLColorFromTColor(AColor: TColor; AAlpha: GLfloat);
procedure SetGLColorFromTColorDarkened(AColor: TColor; AAlpha: GLfloat; ADark: GLfloat);
function WriteGLUTTextH10(X, Y: integer; const s: string): integer;
function WriteGLUTTextH12(X, Y: integer; const s: string): integer;
procedure ShadedRect(Left, Top, Right, Bottom: integer);

implementation

const
  D_TO_R = PI / 180;
  RGB_SCALE = 1 / 255;

procedure SetGLColorFromTColor(AColor: TColor; AAlpha: GLfloat);
var
  R, G, B: BYTE;
begin
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);
  glColor4f(R * RGB_SCALE, G  * RGB_SCALE, B  * RGB_SCALE, AAlpha);
end;

procedure SetGLColorFromTColorDarkened(AColor: TColor; AAlpha: GLfloat; ADark: GLfloat);
begin
  glColor4f(
    GetRValue(AColor) * ADark * RGB_SCALE,
    GetGValue(AColor) * ADark * RGB_SCALE,
    GetBValue(AColor) * ADark * RGB_SCALE,
    AAlpha);
end;

function WriteGLUTTextH10(X, Y: integer; const s: string): integer;
var
  I:    integer;
begin
  Result := Y - 13;

  if not Assigned(glutBitmapCharacter) or (s = '') then
    exit;

  glRasterPos2i(X, Result);
  for I := 1 to Length(s) do
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10, ord(s[I]));
end;

function WriteGLUTTextH12(X, Y: integer; const s: string): integer;
var
  I:    integer;
begin
  Result := Y - 13;

  if not Assigned(glutBitmapCharacter) or (s = '') then
    exit;

  glRasterPos2i(X, Result);
  for I := 1 to Length(s) do
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(s[I]));
end;

procedure ShadedRect(Left, Top, Right, Bottom: integer);
begin
  glColor4f(0, 0, 0, 0.5);
  glBegin(GL_QUADS);
    glVertex2i(Left, Top);
    glVertex2i(Left, Bottom);
    glVertex2i(Right, Bottom);
    glVertex2i(Right, Top);
  glEnd;

  glLineWidth(1.0);
  
  glColor3f(0, 0, 0); // 1, 0.8, 0.4); // orange
  glBegin(GL_LINE_LOOP);
    glVertex2i(Left, Top);
    glVertex2i(Left, Bottom);
    glVertex2i(Right, Bottom);
    glVertex2i(Right, Top);
  glEnd;
end;

{ TRangeCircle }

constructor TRangeCircle.Create;
begin
  inherited;
  FColor := clWhite;
  FRange := 500;
  FSmoothness := 36;
end;

constructor TRangeCircle.CreateRange(ARange, ASmoothness: integer);
begin
  inherited Create;

  FColor := clWhite;
  FRange := ARange;
  FSmoothness := ASmoothness;
end;

procedure TRangeCircle.GLInitialize;
var
  I:    integer;
begin
  inherited;

  glNewList(FGLList, GL_COMPILE);

  I := -180;
  glBegin(GL_LINE_LOOP);
  while I < 180 do begin
    glVertex3f(FRange * cos(I * D_TO_R), FRange * sin(I * D_TO_R), 0);
    inc(I, 360 div FSmoothness);
  end;
  glEnd();

  glEndList;
end;

procedure TRangeCircle.SetRange(const Value: integer);
begin
  FRange := Value;
end;

procedure TRangeCircle.SetSmoothness(const Value: integer);
begin
//  if 360 mod Value <> 0 then
//    raise Exception.Create('Range Cicle smoothness must divide 360 evenly'); 
  FSmoothness := Value;
end;

{ TRangeCircleList }

procedure TRangeCircleList.Add(ACircle: TRangeCircle);
begin
  FList.Add(ACircle);
end;

constructor TRangeCircleList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

procedure TRangeCircleList.CreateDefaultRangeCircles;
var
  rngCircle:  TRangeCircle;
begin
  rngCircle := TRangeCircle.CreateRange(500, 24);
  rngCircle.Color := clRed;
  Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(1500, 24);
  rngCircle.Color := clLime;
  Add(rngCircle);
  rngCircle := TRangeCircle.CreateRange(6000, 40);
  rngCircle.Color := clGray;
  Add(rngCircle);
end;

procedure TRangeCircleList.Delete(AIndex: integer);
{ Delete from the list and free the circle  }
begin
  FList.Delete(AIndex);
end;

destructor TRangeCircleList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRangeCircleList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TRangeCircleList.GetItems(I: integer): TRangeCircle;
begin
  Result := TRangeCircle(FList[I]);
end;

procedure TRangeCircleList.GLCleanup;
var
  I:    integer;
begin
  for I := 0 to FList.Count - 1 do
    Items[I].GLCleanup;
    
  inherited;
end;

procedure TRangeCircleList.GLInitialize;
var
  I:    integer;
begin
  for I := 0 to FList.Count - 1 do
    Items[I].GLInitialize;

  inherited;
end;

procedure TRangeCircleList.GLRender(const ARenderBounds: TRect);
var
  I:    integer;
begin
  inherited;

  glLineWidth(1.0);
  for I := 0 to FList.Count - 1 do
    Items[I].GLRender(ARenderBounds);
end;

procedure TRangeCircleList.LoadFromFile(const AFileName: string);
var
  I:        integer;
  iCount:   integer;
  pCircle:  TRangeCircle;
begin
  FList.Clear;
  with TINIFile.Create(AFileName) do begin
    iCount := ReadInteger('RangeCircles', 'Count', 0);
    if iCount = 0 then
      CreateDefaultRangeCircles
    else begin
      for I := 0 to iCount - 1 do begin
        pCircle := TRangeCircle.CreateRange(
          ReadInteger('RangeCircles', 'Range' + IntToStr(I), 0),
          ReadInteger('RangeCircles', 'Smoothness' + IntToStr(I), 2)
        );
        pCircle.Color := TColor(ReadInteger('RangeCircles', 'Color' + IntToStr(I), integer(clWhite)));
        if pCircle.Range > 0 then
          Add(pCircle)
        else
          pCircle.Free;
      end;  { for I to count }
    end;  { if count > 0 }

    Free;
  end;  { with INI }
end;

procedure TRangeCircleList.SaveToFile(const AFileName: string);
var
  I:    integer;
begin
  with TINIFile.Create(AFileName) do begin
    EraseSection('RangeCircles');
    WriteInteger('RangeCircles', 'Count', Count);
    for I := 0 to Count - 1 do
      with Items[I] do begin
        WriteInteger('RangeCircles', 'Range' + IntToStr(I), Range);
        WriteInteger('RangeCircles', 'Smoothness' + IntToStr(I), Smoothness);
        WriteInteger('RangeCircles', 'Color' + IntToStr(I), integer(Color));
      end;  { for I / with }

    Free;
  end;  { with INI }
end;

{ TGLRenderObject }

function TGLRenderObject.ColorToStr: string;
begin
  Result := ColorToString(FColor);  
end;

constructor TGLRenderObject.Create;
begin
  FColor := clWhite;
  FUseColor := true;
end;

procedure TGLRenderObject.GLCleanup;
begin
  FInitialized := false;
end;

procedure TGLRenderObject.GLInitialize;
begin
  FInitialized := true;
end;

procedure TGLRenderObject.GLRender(const ARenderBounds: TRect);
begin
  if not FInitialized then
    GLInitialize;
    
  if FUseColor then
    SetGLColorFromTColor(FColor, 1);
end;

procedure TGLRenderObject.SetColorFromString(const AColor: string);
begin
  if AnsiSameText(AColor, 'white') then
    FColor := clWhite
  else if AnsiSameText(AColor, 'gray') then
    FColor := clSilver
  else if AnsiSameText(AColor, 'green') then
    FColor := clLime
  else if AnsiSameText(AColor, 'brown') then
    FColor := $004080
  else if AnsiSameText(AColor, 'orange') then
    FColor := $007fff
  else if AnsiSameText(AColor, 'blue') then
    FColor := clBlue
  else if AnsiSameText(AColor, 'red') then
    FColor := clRed
  else if AnsiSameText(AColor, 'pink') then
    FColor := $ffcff
  else if AnsiSameText(AColor, 'yellow') then
    FColor := clYellow
  else if AnsiSameText(AColor, 'cyan') then
    FColor := clAqua
  else if AnsiSameText(AColor, 'magenta') then
    FColor := clMaroon
  else if AnsiSameText(AColor, 'gold') then
    FColor := $00ccff
  else if AnsiSameText(AColor, 'darkgreen') then
    FColor := clGreen
  else if AnsiSameText(AColor, 'purple') then
    FColor := clFuchsia
  else if AnsiSameText(AColor, 'silver') then
    FColor := clSilver
  else if AnsiSameText(AColor, 'black') then
    FColor := clBlack
  else if AnsiSameText(AColor, 'darkgray') then
    FColor := clGray
  else if AnsiSameText(AColor, 'grey') then
    FColor := clSilver
  else
    try
      FColor := StringToColor(AColor);
    except
      FColor := clFuchsia;
    end;
end;

function TGLRenderObject.ToString: string;
begin
  Result := '';
end;

{ TGLCallListObject }

procedure TGLCallListObject.GLCleanup;
begin
  if FGLList <> 0 then begin
    glDeleteLists(FGLList, 1);
    FGLList := 0;
  end;

  inherited;
end;

procedure TGLCallListObject.GLInitialize;
begin
  inherited;

  FGLList := glGenLists(1);
end;

procedure TGLCallListObject.GLRender(const ARenderBounds: TRect);
begin
  inherited;
  glCallList(FGLList);
end;

{ TMapElementPoint }

procedure TMapElementPoint.Assign(AX, AY, AZ: GLuint);
begin
  FX := AX + GLuint(FOffsetX);
  FY := AY + GLuint(FOffsetY);
  FZ := AZ;
end;

constructor TMapElementPoint.Create;
begin
  inherited;
  FColor := clWhite;
end;

procedure TMapElementPoint.GLRender(const ARenderBounds: TRect);
begin
  if not PointInRect(ARenderBounds, X, Y) then
    exit;

  inherited;

  glPointSize(3.0);
  glBegin(GL_POINTS);
    glVertex3i(X, Y, 0);  // Z
  glEnd();

  WriteGLUTTextH10(X + 50, Y + 20, FName)
end;

function TMapElementPoint.ToString: string;
begin
  Result := Format('P,%s,%s,%u,%u,%u', [FName, ColorToStr, FX, FY, FZ]);
end;

{ TMapElementLine }

procedure TMapElementLine.AddPoint(X, Y, Z: GLint);
var
  pTmp:   PMapElementLinePoint;
begin
  inc(X, FOffsetX);
  inc(Y, FOffsetY);

  New(pTmp);
  FPoints.Add(pTmp);
  pTmp^.X := X;
  pTmp^.Y := Y;
  pTmp^.Z := Z;

  if FPoints.Count = 1 then
    FBounds := Rect(X, Y, X, Y)
  else begin
    if X < FBounds.Left then
      FBounds.Left := X;
    if Y < FBounds.Top then
      FBounds.Top := Y;
    if X > FBounds.Right then
      FBounds.Right := Y;
    if Y > FBounds.Bottom then
      FBounds.Bottom := Y;
  end;
end;

procedure TMapElementLine.ClearPoints;
var
  I:    integer;
begin
  for I := 0 to FPoints.Count - 1 do
    Dispose(PMapElementLinePoint(FPoints[I]));
  FPoints.Clear;
end;

constructor TMapElementLine.Create;
begin
  inherited;
  FPoints := TList.Create;
end;

destructor TMapElementLine.Destroy;
begin
  ClearPoints;
  FPoints.Free;
  inherited;
end;

procedure TMapElementLine.GLInitialize;
var
  I:  integer;
begin
  inherited;
  glNewList(FGLList, GL_COMPILE);

  glBegin(GL_LINE_STRIP);
  for I := 0 to FPoints.Count - 1 do
    with PMapElementLinePoint(FPoints[I])^ do
      glVertex3f(X, Y, 0);  // Z
  glEnd();

  glEndList();
end;

{ TMapElementTerrrainTexture }

destructor TMapElementTerrrainTexture.Destroy;
begin
  FDDSChunk.Free;
  FDDSChunk := nil;
  inherited;
end;

procedure TMapElementTerrrainTexture.GLCleanup;
begin
  if FGLTexture <> 0 then begin
    glDeleteTextures(1, @FGLTexture);
    FGLTexture := 0;
  end;

  FreeAndNil(FDDSChunk);

  inherited;
end;

procedure TMapElementTerrrainTexture.GLRender(const ARenderBounds: TRect);
begin
  if not RectsIntersect(ARenderBounds, FBounds) then
    exit;

  inherited;

  if (FGLTexture = 0) and Assigned(FDDSChunk) then
    UploadTexture;

    { if the texture is 0 then this should effectively disable the texture
      for this quad }
  glBindTexture(GL_TEXTURE_2D, FGLTexture);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2i(FBounds.Left, FBounds.Top);
    glTexCoord2f(1, 0);
    glVertex2i(FBounds.Right, FBounds.Top);
    glTexCoord2f(1, 1);
    glVertex2i(FBounds.Right, FBounds.Bottom);
    glTexCoord2f(0, 1);
    glVertex2i(FBounds.Left, FBounds.Bottom);
  glEnd;
end;

procedure TMapElementTerrrainTexture.TakeDDSChunk(X, Y, AScale: integer;
  ADDSChunk: TDDSImagePixelsChunk);
begin
  FBounds.Left := FOffsetX + (X * AScale);
  FBounds.Top := FOffsetY + (Y * AScale);
  FBounds.Right := FBounds.Left + (ADDSChunk.Width * AScale);
  FBounds.Bottom := FBounds.Top + (ADDSChunk.Height * AScale);

  FreeAndNil(FDDSChunk);
  FDDSChunk := ADDSChunk;
end;

procedure TMapElementTerrrainTexture.UploadTexture;
begin
  if not Assigned(FDDSChunk) then
    exit;

  glGenTextures(1, @FGLTexture);
  glBindTexture(GL_TEXTURE_2D, FGLTexture);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  if Assigned(glCompressedTexImage2D) then
    glCompressedTexImage2D(GL_TEXTURE_2D, 0, FDDSChunk.internalFormat,
      FDDSChunk.Width, FDDSChunk.Height, 0, FDDSChunk.PixelsSize, FDDSChunk.Pixels);

  FreeAndNil(FDDSChunk);
end;

{ T3DArrowHead }

constructor T3DArrowHead.Create;
begin
  inherited;
  FSize := 150;
  FUseColor := false;
end;

procedure T3DArrowHead.GLInitialize;
var
  w:  integer;
  l:  integer;
begin
  inherited;
  w := FSize;
  l := w * 2;

  glEdgeFlag(GL_TRUE);
  glNewList(FGLList, GL_COMPILE);

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
end;

procedure T3DArrowHead.SetSize(const Value: integer);
begin
  FSize := Value;
end;

{ T3DPyramid }

constructor T3DPyramid.Create;
begin
  inherited;
  FSize := 150;
end;

procedure T3DPyramid.GLInitialize;

begin
  inherited;

  glEdgeFlag(GL_TRUE);
  glNewList(FGLList, GL_COMPILE);

  glBegin(GL_TRIANGLE_FAN);
    glVertex3i(0,0,FSize);

    glNormal3f(0,FSize,FSize);
    glVertex3i(-FSize,FSize,-FSize);
    glVertex3i(FSize,FSize,-FSize);

    glNormal3f(FSize,0,FSize);
    glVertex3i(FSize,-FSize,-FSize);

    glNormal3f(0,-FSize,FSize);
    glVertex3i(-FSize,-FSize,-FSize);

    glNormal3f(-FSize,0,FSize);
    glVertex3i(-FSize,FSize,-FSize);
  glEnd();
  glEndList();
end;

procedure T3DPyramid.SetSize(const Value: integer);
begin
  FSize := Value;
end;

{ TGLBullsEye }

procedure TGLBullsEye.Assign(AX, AY: GLuint);
begin
  FX := AX;
  FY := AY;
end;

constructor TGLBullsEye.Create;
begin
  inherited;
  FSize := 350;
end;

procedure TGLBullsEye.GLInitialize;
var
  pQuadric:   PGLUquadric;
begin
  inherited;
  FUseColor := false;

  pQuadric := gluNewQuadric();
  gluQuadricOrientation(pQuadric, GLU_INSIDE);

  glNewList(FGLList, GL_COMPILE);

  glColor4f(1, 0, 0, 0.50);
  gluDisk(pQuadric, (2/3) * FSize, FSize, 15, 2);

  glColor4f(1, 1, 1, 0.50);
  gluDisk(pQuadric, (1/3) * FSize, (2/3) * FSize, 15, 2);

  glColor4f(1, 0, 0, 0.50);
  gluDisk(pQuadric, 0, (1/3) * FSize, 15, 2);

  glEndList();

  gluDeleteQuadric(pQuadric);
end;

procedure TGLBullsEye.GLRender(const ARenderBounds: TRect);
begin
  if (FX = 0) or (FY = 0) or not PointInRect(ARenderBounds, FX, FY) then
    exit;

  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);

  glPushMatrix();
  glTranslatef(FX, FY, 0);

  inherited;

  glPopMatrix();
end;

procedure TGLBullsEye.SetSize(const Value: integer);
begin
  FSize := Value;
end;

{ TGLViewFrustum }

constructor TGLFlatViewFrustum.Create;
begin
  inherited;
  FUseColor := false;
  FFOVy := 60;
  FZFar := 3300;
end;

procedure TGLFlatViewFrustum.GLInitialize;
var
  pQuadric:   PGLUquadric;
  a:  single;
begin
  inherited;

  a := FFOVy * 0.5 * (4 / 3);

  pQuadric := gluNewQuadric();
  gluQuadricOrientation(pQuadric, GLU_INSIDE);
  
  glNewList(FGLList, GL_COMPILE);
    gluPartialDisk(pQuadric, 0, FZFar, 6, 1, -a, 2 * a);
  glEndList();

  gluDeleteQuadric(pQuadric);
end;

procedure TGLFlatViewFrustum.GLRender(const ARenderBounds: TRect);
begin
  glPushAttrib(GL_ENABLE_BIT);
  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);

  glColor4f(0, 1, 0, 0.15);
  inherited;

  glPopAttrib();
end;

{ TGLBoat }

constructor TGLBoat.Create;
begin
  inherited;
  FSize := 150;
  FColor := $336699;
  FAlpha := 1;

    { we need to do our own alpha so don't use color }
  FUseColor := false;
end;

procedure TGLBoat.GLInitialize;
var
  l:    GLfloat;
begin
  inherited;

  l := FSize;

  glNewList(FGLList, GL_COMPILE);
      { the basis of the boat is a flat [ ]>  (pointing up though) }
    glBegin(GL_TRIANGLE_FAN);
      glNormal3f(0, 0, 1);
      glVertex3f(0, 0, 0);  // center
      glVertex3f(-l, -l, 0);
      glVertex3f(-l, 2 * l, 0);
      glVertex3f(0, 3 * l, 0);  // top
      glVertex3f(l, 2 * l, 0);
      glVertex3f(l, -l, 0);
      glVertex3f(-l, -l, 0);
    glEnd();

      { then we put an inverted version inside with some normals to make
        it look spiffy }
    glBegin(GL_TRIANGLE_FAN);
      glNormal3f(0, 0, 1);
      glVertex3f(0, 0, 0);  // center
      glNormal3f(1, 0, 1);
      glVertex3f(-0.8 * l, -0.8 * l, 0);
      glVertex3f(-0.8 * l, 1.8 * l, 0);
      glNormal3f(1, -1, 1);
      glVertex3f(0, 2.8 * l, 0);  // top
      glNormal3f(-1, -1, 1);
      glVertex3f(0.8 * l, 1.8 * l, 0);
      glNormal3f(-1, 0, 1);
      glVertex3f(0.8 * l, -0.8 * l, 0);
      glNormal3f(0, -1, 1);
      glVertex3f(-0.8 * l, -0.8 * l, 0);
    glEnd();

      { now add a little sail like /  \  }
    glColor3f(1, 1, 1);
    glBegin(GL_QUADS);
      glNormal3f(0, 1, 1);
      glVertex3f(-1.5 * l, 0, 0);
      glVertex3f(-l, l, 0);
      glVertex3f(l, l, 0);
      glVertex3f(1.5 * l, 0, 0);
    glEnd();
  glEndList();
end;

procedure TGLBoat.GLRender(const ARenderBounds: TRect);
begin
  SetGLColorFromTColor(FColor, FAlpha);
  inherited;
end;

procedure TGLBoat.SetSize(const Value: integer);
begin
  FSize := Value;
end;

{ TGLUnkownStealther }

constructor TGLUnkownStealther.Create;
begin
  inherited;
  FSize := 1000;
  FColor := $00ccff;
  FAlpha := 1;

    { we need to do our own alpha so don't use color }
  FUseColor := false;
end;

procedure TGLUnkownStealther.GLInitialize;
var
  pQuadric:   PGLUquadric;
begin
  inherited;
  FUseColor := false;

  pQuadric := gluNewQuadric();
  gluQuadricOrientation(pQuadric, GLU_INSIDE);

  glNewList(FGLList, GL_COMPILE);
    gluDisk(pQuadric, 0, FSize, 20, 1);
  glEndList();

  gluDeleteQuadric(pQuadric);
end;

procedure TGLUnkownStealther.GLRender(const ARenderBounds: TRect);
begin
  SetGLColorFromTColor(FColor, FAlpha);
  inherited;
end;

procedure TGLUnkownStealther.SetSize(const Value: integer);
begin
  FSize := Value;
end;

{ TGLPrescienceNode }

procedure TGLPrescienceNode.GLCleanup;
begin
  if FImage <> 0 then begin
    glDeleteTextures(1, @FImage);
    FImage := 0;
  end;
end;

procedure TGLPrescienceNode.GLInitialize;
var
  tga2:   TTGAImage;
  orig_format:  DWORD;
begin
  inherited;
  FColor := $ffffff;  // white

  if FileExists(FImageFileName) then begin
    glGenTextures(1, @FImage);
    tga2 := TTGAImage.Create;
    tga2.LoadFromFile(FImageFileName);

    if tga2.Header.ImgSpec.Depth = 32 then
      orig_format := GL_BGRA
    else
      orig_format := GL_BGR;

    glBindTexture(GL_TEXTURE_2D, FImage);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tga2.Header.ImgSpec.Width, tga2.Header.ImgSpec.Height,
      0, orig_format, GL_UNSIGNED_BYTE, tga2.Data);

    tga2.Free;

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end  { if texture exists }
  else
    FImage := 0;

  glNewList(FGLList, GL_COMPILE);

  if FImage <> 0 then begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FImage);
  end;

  glBegin(GL_QUADS);
    glTexCoord2f(0, 1);
    glVertex3f(-1000, -1000, 0);
    glTexCoord2f(1, 1);
    glVertex3f(1000, -1000, 0);
    glTexCoord2f(1, 0);
    glVertex3f(1000, 1000, 0);
    glTexCoord2f(0, 0);
    glVertex3f(-1000, 1000, 0);
  glEnd();

  if FImage <> 0 then
    glDisable(GL_TEXTURE_2D);
    
  glEndList();
end;

end.

unit GLRenderObjects;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, GL, GLext, DDSImage;

type
  TGLRenderObject = class(TObject)
  private
  protected
    FColor:   TColor;
    FName:    string;
    FOffsetY: integer;
    FOffsetX: integer;
  public
    constructor Create; virtual;

    procedure SetColorFromString(const AColor: string);

    procedure GLInitialize; virtual;
    procedure GLRender; virtual;
    procedure GLCleanup; virtual;

    property Color: TColor read FColor write FColor;
    property Name: string read FName write FName;
    property OffsetX: integer read FOffsetX write FOffsetX;
    property OffsetY: integer read FOffsetY write FOffsetY;
  end;

  TRangeCircle = class(TGLRenderObject)
  private
    FRange:   integer;
  public
    constructor Create; override;
    constructor CreateRange(ARange: integer);

    procedure GLRender; override;

    property Range: integer read FRange write FRange;
  end;

  TRangeCircleList = class(TGLRenderObject)
  private
    FList:  TObjectList;
    function GetItems(I: integer): TRangeCircle;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GLInitialize; override;
    procedure GLRender; override;
    procedure GLCleanup; override;

    procedure Add(ACircle: TRangeCircle);
    property Items[I: integer]: TRangeCircle read GetItems; default;
  end;

  TGLCallListObject = class(TGLRenderObject)
  protected
    FGLList:    GLuint;
  public
    procedure GLInitialize; override;
    procedure GLRender; override;
    procedure GLCleanup; override;
  end;

  TMapElementPoint = class(TGLRenderObject)
  private
    FY: GLuint;
    FX: GLuint;
    FZ: GLuint;
  public
    constructor Create; override;
    procedure GLRender; override;

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

    procedure GLRender; override;
    procedure GLCleanup; override;

    procedure TakeDDSChunk(X, Y, AScale: integer; ADDSChunk: TDDSImagePixelsChunk);

    property Bounds: TRect read FBounds;
  end;

implementation

uses Types;

const
  D_TO_R = PI / 180;

procedure SetGLColorFromTColor(AColor: TColor; AAlpha: GLfloat);
var
  R, G, B: BYTE;
begin
  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);
  glColor4f(R / 255, G / 255, B / 255, AAlpha);
end;

{ TRangeCircle }

constructor TRangeCircle.Create;
begin
  inherited;
  FColor := clWhite;
end;

constructor TRangeCircle.CreateRange(ARange: integer);
begin
  inherited Create;
  
  FColor := clWhite;
  FRange := ARange;
end;

procedure TRangeCircle.GLRender;
var
  I:    integer;
begin
  inherited;

  I := -180;
  glBegin(GL_LINE_LOOP);
  while I < 180 do begin
    glVertex3f(FRange * cos(I * D_TO_R), FRange * sin(I * D_TO_R), 0);
    inc(I, 20);
  end;
  glEnd();
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

destructor TRangeCircleList.Destroy;
begin
  FList.Free;
  inherited;
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

procedure TRangeCircleList.GLRender;
var
  I:    integer;
begin
  inherited;

  for I := 0 to FList.Count - 1 do
    Items[I].GLRender;
end;

{ TGLRenderObject }

constructor TGLRenderObject.Create;
begin
  FColor := clWhite;
end;

procedure TGLRenderObject.GLCleanup;
begin
;
end;

procedure TGLRenderObject.GLInitialize;
begin
;
end;

procedure TGLRenderObject.GLRender;
begin
  SetGLColorFromTColor(FColor, 1);
end;

procedure TGLRenderObject.SetColorFromString(const AColor: string);
begin
  if AnsiSameText(AColor, 'white') then
    FColor := clWhite
  else if AnsiSameText(AColor, 'gray') then
    FColor := clGray
  else if AnsiSameText(AColor, 'silver') then
    FColor := clSilver
  else if AnsiSameText(AColor, 'black') then
    FColor := clBlack
  else if AnsiSameText(AColor, 'brown') then
    FColor := $004080
  else if AnsiSameText(AColor, 'blue') then
    FColor := clBlue
  else if AnsiSameText(AColor, 'green') then
    FColor := clGreen
  else
    FColor := clFuchsia;
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

procedure TGLCallListObject.GLRender;
begin
  inherited;
  glCallList(FGLList);
end;

{ TMapElementPoint }

constructor TMapElementPoint.Create;
begin
  inherited;
  FColor := clWhite;
end;

procedure TMapElementPoint.GLRender;
begin
  inherited;

  glBegin(GL_POINTS);
    glVertex3i(X, Y, 0);  // Z
  glEnd();
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

procedure TMapElementTerrrainTexture.GLRender;
begin
  inherited;

  if (FGLTexture = 0) and Assigned(FDDSChunk) then
    UploadTexture;

  glColor3f(1, 1, 1);
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

  glCompressedTexImage2D(GL_TEXTURE_2D, 0, FDDSChunk.internalFormat,
    FDDSChunk.Width, FDDSChunk.Height, 0, FDDSChunk.PixelsSize, FDDSChunk.Pixels);

  FreeAndNil(FDDSChunk);
end;

end.
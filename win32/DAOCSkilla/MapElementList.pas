unit MapElementList;

interface

uses
  SysUtils, Types, Contnrs, Classes, GLRenderObjects, LinedFileStream,
  CSVLineParser, DDSImage, GL, DAOCRegion, BackgroundHTTP;

type
  TGLRenderObjectList = class(TObjectList)
  protected
    FOffsetY: integer;
    FOffsetX: integer;
    function GetItems(I: integer): TGLRenderObject;
  public
    procedure GLInitialize; virtual;
    procedure GLRender(const ARenderBounds: TRect); virtual;
    procedure GLCleanup; virtual;

    property Items[I: integer]: TGLRenderObject read GetItems; default;
    property OffsetX: integer read FOffsetX write FOffsetX;
    property OffsetY: integer read FOffsetY write FOffsetY;
  end;

  TZoneGLRenderObjectList = class(TGLRenderObjectList)
  private
    FZoneNum: integer;
  public
    property ZoneNum: integer read FZoneNum write FZoneNum;
  end;

  TVectorMapElementList = class(TZoneGLRenderObjectList)
  public
    procedure LoadFromFile(const AFileName: string);
    procedure AppendFromFile(const AFileName: string);
  end;

  TTextureMapElementList = class(TZoneGLRenderObjectList)
  private
    FFileName:  string;
  public
    procedure ReloadFile;
    procedure DeleteFile;
    procedure LoadFromSingleDDSFile(const AFileName: string);
    procedure GLRender(const ARenderBounds: TRect); override;
  end;

  TZoneGLRenderObjectListList = class(TObjectList)
  private
    function GetItems(I: integer): TZoneGLRenderObjectList;
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); virtual;
  public
    procedure GLInitialize;
    procedure GLRender(const ARenderBounds: TRect);
    procedure GLCleanup;

    procedure GLInitializeExcept(AZoneNum: integer);
    procedure GLCleanupExcept(AZoneNum: integer);

    procedure LoadForZone(AZone: TDAOCZoneInfo; ALoadAdjacent: boolean);

    property Items[I: integer]: TZoneGLRenderObjectList read GetItems; default;
  end;

  TVectorMapElementListList = class(TZoneGLRenderObjectListList)
  private
    FVectorMapDir: string;
    function GetItems(I: integer): TVectorMapElementList;
    procedure SetVectorMapDir(const Value: string);
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); override;
  public
    property Items[I: integer]: TVectorMapElementList read GetItems; default;
    property VectorMapDir: string read FVectorMapDir write SetVectorMapDir;
  end;

  TTextureMapElementListList = class(TZoneGLRenderObjectListList)
  private
    FTextureMapDir: string;
    FHTTPTextureFetch:  TBackgroundHTTPManager;
    FAttemptMapDownload: boolean;
    FMapBaseURL: string;
    function GetItems(I: integer): TTextureMapElementList;
    procedure SetTextureMapDir(const Value: string);
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); override;
    procedure HTTPComplete(ARequest: TBackgroundHTTPRequest);
    procedure HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest);
  public
    constructor Create;
    destructor Destroy; override;

    function FindZone(AZoneNum: integer) : TTextureMapElementList;

    property Items[I: integer]: TTextureMapElementList read GetItems; default;
    property TextureMapDir: string read FTextureMapDir write SetTextureMapDir;
    property MapBaseURL: string read FMapBaseURL write FMapBaseURL;  
    property AttemptMapDownload: boolean read FAttemptMapDownload write FAttemptMapDownload;
  end;

implementation

{ TVectorMapElementList }

procedure TVectorMapElementList.AppendFromFile(const AFileName: string);
var
  FS:   TLinedFileStream;
  CSV:  TCSVLineParser;
  tmpItem:  TGLRenderObject;
  iPointCount:  integer;
  I:    integer;
begin
  if not FileExists(AFileName) then
    exit;
    
  FS := TLinedFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  CSV := TCSVLineParser.Create;
  try
    FS.ReadLn;  // header

    while not FS.EOF do begin
      CSV.DataString := FS.ReadLn;

      if CSV.FieldCount < 1 then
        continue;

        { POINT }
      if CSV[0] = 'P' then begin
        tmpItem := TMapElementPoint.Create;
        tmpItem.OffsetX := FOffsetX;
        tmpItem.OffsetY := FOffsetY;
        Add(tmpItem);
        with TMapElementPoint(tmpItem) do begin
          Name := CSV[1];
          SetColorFromString(CSV[2]);
          Assign(CSV.FieldAsInt(3, 0), CSV.FieldAsInt(4, 0), CSV.FieldAsInt(5, 0));
        end;
      end  { point }

        { LINE }
      else if (CSV[0] = 'M') or (CSV[0] = 'F') then begin
        tmpItem := TMapElementLine.Create;
        tmpItem.OffsetX := FOffsetX;
        tmpItem.OffsetY := FOffsetY;
        Add(tmpItem);
        with TMapElementLine(tmpItem) do begin
          Name := CSV[1];
          SetColorFromString(CSV[2]);
          iPointCount := CSV.FieldAsInt(3, 0);
          for I := 0 to iPointCount - 1 do
            AddPoint(CSV.FieldAsInt(I * 3 + 4, 0), CSV.FieldAsInt(I * 3 + 5, 0),
              CSV.FieldAsInt(I * 3 + 6, 0));
        end;  { with line element }
      end;  { line }
    end;  { while !EOF }
  finally
    FS.Free;
    CSV.Free;
  end;
end;

procedure TVectorMapElementList.LoadFromFile(const AFileName: string);
begin
  Clear;
  AppendFromFile(AFileName);
end;

{ TTextureMapElementList }

procedure TTextureMapElementList.DeleteFile;
begin
  if FFileName <> '' then
    SysUtils.DeleteFile(FFileName);
end;

procedure TTextureMapElementList.GLRender(const ARenderBounds: TRect);
begin
  glEnable(GL_TEXTURE_2D);
  inherited;
  glDisable(GL_TEXTURE_2D);
end;

procedure TTextureMapElementList.LoadFromSingleDDSFile(const AFileName: string);
const
  DDS_SPLIT_COUNT = 4;   // divide DDS into 16 parts
var
  dds:    TDDSImage;
  X, Y:   integer;
  tmpTex: TMapElementTerrrainTexture;
  iScale: integer;
begin
  Clear;
  FFileName := AFileName;
  if not FileExists(AFileName) then
    exit;

  dds := TDDSImage.Create;
  dds.LoadFromFile(AFileName);
    { if no pixel data then give up.  fskers tried to trick me }
  if dds.PixelsSize = 0 then begin
    dds.Free;
    exit;
  end;

  iScale := $10000 div dds.Width;

  Y := 0;
  while Y < dds.Height do begin
    X := 0;
    while X < dds.Width do begin
      tmpTex := TMapElementTerrrainTexture.Create;
      Add(tmpTex);
      tmpTex.OffsetX := FOffsetX;
      tmpTex.OffsetY := FOffsetY;
      tmpTex.TakeDDSChunk(X, Y, iScale, dds.CopyChunk(X, Y,
        dds.Height div DDS_SPLIT_COUNT, dds.Width div DDS_SPLIT_COUNT));

      inc(X, dds.Width div DDS_SPLIT_COUNT);
    end;

    inc(Y, dds.Height div DDS_SPLIT_COUNT);
  end;

  dds.Free;
end;

procedure TTextureMapElementList.ReloadFile;
begin
  LoadFromSingleDDSFile(FFileName);
end;

{ TGLRenderObjectList }

function TGLRenderObjectList.GetItems(I: integer): TGLRenderObject;
begin
  Result := TGLRenderObject(inherited Items[I]);
end;

procedure TGLRenderObjectList.GLCleanup;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLCleanup;
end;

procedure TGLRenderObjectList.GLInitialize;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLInitialize;
end;

procedure TGLRenderObjectList.GLRender(const ARenderBounds: TRect);
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLRender(ARenderBounds);
end;

{ TZoneGLRenderObjectListList }

procedure TZoneGLRenderObjectListList.AddZone(AZone: TDAOCZoneInfo);
begin
end;

function TZoneGLRenderObjectListList.GetItems(I: integer): TZoneGLRenderObjectList;
begin
  Result := TZoneGLRenderObjectList(inherited Items[I]);
end;

procedure TZoneGLRenderObjectListList.GLCleanup;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLCleanup;
end;

procedure TZoneGLRenderObjectListList.GLCleanupExcept(AZoneNum: integer);
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ZoneNum <> AZoneNum then
      Items[I].GLCleanup;
end;

procedure TZoneGLRenderObjectListList.GLInitialize;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLInitialize;
end;

procedure TZoneGLRenderObjectListList.GLInitializeExcept(AZoneNum: integer);
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ZoneNum <> AZoneNum then
      Items[I].GLInitialize;
end;

procedure TZoneGLRenderObjectListList.GLRender(const ARenderBounds: TRect);
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLRender(ARenderBounds);
end;

procedure TZoneGLRenderObjectListList.LoadForZone(AZone: TDAOCZoneInfo; ALoadAdjacent: boolean);
var
  I:    integer;
  bAddedRequested:  boolean;
begin
  GLCleanupExcept(AZone.ZoneNum);

  for I := Count - 1 downto 0 do
    if Items[I].ZoneNum <> AZone.ZoneNum then
      Delete(I);

  { we should have 0 or 1 items left in the list.  If we have 1 it is the
    zone we're looking for }
  if Count = 0 then begin
    AddZone(AZone);
    bAddedRequested := true;
  end
  else
    bAddedRequested := false;

  if ALoadAdjacent then
    for I := 0 to AZone.AdjacentZones.Count - 1 do
      AddZone(AZone.AdjacentZones[I]);

  if bAddedRequested then
    GLInitialize
  else
    GLInitializeExcept(AZone.ZoneNum);
end;

{ TVectorMapElementListList }

procedure TVectorMapElementListList.AddZone(AZone: TDAOCZoneInfo);
var
  pTmpZone:   TVectorMapElementList;
begin
  pTmpZone := TVectorMapElementList.Create;
  Add(pTmpZone);
  pTmpZone.ZoneNum := AZone.ZoneNum;
  pTmpZone.OffsetX := AZone.BaseLoc.X;
  pTmpZone.OffsetY := AZone.BaseLoc.Y;
  pTmpZone.LoadFromFile(FVectorMapDir + AZone.MapName);
end;

function TVectorMapElementListList.GetItems(I: integer): TVectorMapElementList;
begin
  Result := TVectorMapElementList(inherited Items[I]);
end;

procedure TVectorMapElementListList.SetVectorMapDir(const Value: string);
begin
  FVectorMapDir := IncludeTrailingPathDelimiter(Value);
end;

{ TTextureMapElementListList }

procedure TTextureMapElementListList.AddZone(AZone: TDAOCZoneInfo);
var
  pTmpZone:   TTextureMapElementList;
  pHTTPRequest: TBackgroundHTTPRequest;
  sDestFileName:  string;
begin
    { make sure we have the directory the DDS files are in, in case
      we have to download em }
  ForceDirectories(FTextureMapDir);

  pTmpZone := TTextureMapElementList.Create;
  Add(pTmpZone);
  pTmpZone.ZoneNum := AZone.ZoneNum;
  pTmpZone.OffsetX := AZone.BaseLoc.X;
  pTmpZone.OffsetY := AZone.BaseLoc.Y;
  sDestFileName := Format('%szone%3.3d.dds', [FTextureMapDir, AZone.ZoneNum]);
  pTmpZone.LoadFromSingleDDSFile(sDestFileName);

    { zone didn't load.  Try to get it from the woooooooorld wide web }
  if FAttemptMapDownload and (pTmpZone.Count = 0) and
    (AZone.ZoneType in [dztOverworld, dztHousing])  then begin
    pHTTPRequest := TBackgroundHTTPRequest.CreateGET;
    pHTTPRequest.URL := FMapBaseURL + 'z=' + IntToStr(AZone.ZoneNum);
    pHTTPRequest.Tag := AZone.ZoneNum;
    pHTTPRequest.ResponseStream := TFileStream.Create(sDestFileName, fmCreate);
    FHTTPTextureFetch.Request(pHTTPRequest);
  end;
end;

function TTextureMapElementListList.FindZone(AZoneNum: integer) : TTextureMapElementList;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ZoneNum = AZoneNum then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

constructor TTextureMapElementListList.Create;
begin
  inherited;
  FHTTPTextureFetch := TBackgroundHTTPManager.Create;
  FHTTPTextureFetch.OnRequestComplete := HTTPComplete;
  FHTTPTextureFetch.OnHTTPError := HTTPError;
end;

destructor TTextureMapElementListList.Destroy;
begin
  FHTTPTextureFetch.Shutdown;
  FHTTPTextureFetch.Free;
  
  inherited;
end;

function TTextureMapElementListList.GetItems(I: integer): TTextureMapElementList;
begin
  Result := TTextureMapElementList(inherited Items[I]);
end;

procedure TTextureMapElementListList.HTTPComplete(ARequest: TBackgroundHTTPRequest);
var
  pZoneTexList:  TTextureMapElementList;
begin
  pZoneTexList := FindZone(ARequest.Tag);
    { request is complete.  Attempt a reload }
  if Assigned(pZoneTexList) then begin
      { we have to close the stream to make sure window will share the file }
    ARequest.ResponseStream.Free;
    ARequest.ResponseStream := nil;
    pZoneTexList.ReloadFile;
  end;
end;

procedure TTextureMapElementListList.SetTextureMapDir(const Value: string);
begin
  FTextureMapDir := IncludeTrailingPathDelimiter(Value);
end;

procedure TTextureMapElementListList.HTTPError(const AErr: string;
  ARequest: TBackgroundHTTPRequest);
var
  pZoneTexList:  TTextureMapElementList;
begin
  pZoneTexList := FindZone(ARequest.Tag);
    { The damn response is the error message from the http server }
  if Assigned(pZoneTexList) then begin
      { close the filestream }
    ARequest.ResponseStream.Free;
    ARequest.ResponseStream := nil;
    pZoneTexList.DeleteFile;
  end;
end;

end.

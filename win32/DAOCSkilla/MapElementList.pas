unit MapElementList;

interface

uses
  SysUtils, Contnrs, GLRenderObjects, LinedFileStream, CSVLineParser,
  DDSImage;

type
  TGLRenderObjectList = class(TObjectList)
  protected
    FOffsetY: integer;
    FOffsetX: integer;
    function GetItems(I: integer): TGLRenderObject;
  public
    procedure GLInitialize;
    procedure GLRender;
    procedure GLCleanup;

    property Items[I: integer]: TGLRenderObject read GetItems; default;
    property OffsetX: integer read FOffsetX write FOffsetX;
    property OffsetY: integer read FOffsetY write FOffsetY;
  end;

  TVectorMapElementList = class(TGLRenderObjectList)
  private
  public
    procedure LoadFromFile(const AFileName: string);
    procedure AppendFromFile(const AFileName: string);
  end;

  TTextureMapElementList = class(TGLRenderObjectList)
  private
  public
    procedure LoadFromFile(const AFileName: string);
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
          X := CSV.FieldAsInt(3, 0);
          Y := CSV.FieldAsInt(4, 0);
          Z := CSV.FieldAsInt(5, 0);
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

procedure TTextureMapElementList.LoadFromFile(const AFileName: string);
const
  DDS_SPLIT_COUNT = 4;   // divide DDS into 16 parts
var
  dds:    TDDSImage;
  X, Y:   integer;
  tmpTex: TMapElementTerrrainTexture;
  iScale: integer;
begin
  Clear;
  if not FileExists(AFileName) then
    exit;

  dds := TDDSImage.Create;
  dds.LoadFromFile(AFileName);

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

procedure TGLRenderObjectList.GLRender;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    Items[I].GLRender;
end;

end.

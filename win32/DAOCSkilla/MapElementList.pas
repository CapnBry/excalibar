unit MapElementList;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  SysUtils, Types, Contnrs, Classes, INIFiles, GLRenderObjects, LinedFileStream,
  CSVLineParser, DDSImage, GL, DAOCRegion
{$IFDEF MSWINDOWS}
  ,BackgroundHTTP
{$ENDIF}
  ;

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
  private
    FFileName:  string;
    FDrawInfoPoints: boolean;
    procedure SetDrawInfoPoints(const Value: boolean);
  public
    constructor Create;

    procedure ReloadFile;
    procedure DeleteFile;
    procedure LoadFromFile(const AFileName: string);
    procedure AppendFromFile(const AFileName: string);
    procedure Save(const ATitle: string);

    property DrawInfoPoints: boolean read FDrawInfoPoints write SetDrawInfoPoints;
    property FileName: string read FFileName write FFileName;
  end;

  TTextureMapElementList = class(TZoneGLRenderObjectList)
  private
    FFileName:  string;
  public
    procedure ReloadFile;
    procedure DeleteFile;
    procedure LoadFromSingleDDSFile(const AFileName: string);
    procedure GLRender(const ARenderBounds: TRect); override;

    property FileName: string read FFileName write FFileName;
  end;

  TZoneGLRenderObjectListList = class(TObjectList)
  private
    FAttemptDownload: boolean;
    FVersionFile: string;
{$IFDEF MSWINDOWS}
    FHTTPFetch: TBackgroundHTTPManager;
{$ENDIF MSWINDOWS}
    function GetItems(I: integer): TZoneGLRenderObjectList;
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); virtual;
{$IFDEF MSWINDOWS}
    procedure HTTPComplete(ARequest: TBackgroundHTTPRequest); virtual;
    procedure HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest); virtual;
{$ENDIF MSWINDOWS}
    procedure HTTPDownload(const AURL, ADestFile: string; ATag: integer = 0);
    function HaveLatestVersion(const ALocalFile, ASection, AKey: string) : boolean;
  public
    procedure GLInitialize;
    procedure GLRender(const ARenderBounds: TRect);
    procedure GLCleanup;

    procedure GLInitializeExcept(AZoneNum: integer);
    procedure GLCleanupExcept(AZoneNum: integer);

    procedure LoadForZone(AZone: TDAOCZoneInfo; ALoadAdjacent: boolean);

    property AttemptDownload: boolean read FAttemptDownload write FAttemptDownload;
    property Items[I: integer]: TZoneGLRenderObjectList read GetItems; default;
{$IFDEF MSWINDOWS}
    property HTTPFetch: TBackgroundHTTPManager read FHTTPFetch write FHTTPFetch;
{$ENDIF MSWINDOWS}
    property VersionFile: string read FVersionFile write FVersionFile;
  end;

  TVectorMapElementListList = class(TZoneGLRenderObjectListList)
  private
    FVectorMapDir: string;
    FMapBaseURL: string;
    FVectorMapCustomDir: string;
    FDrawInfoPoints: boolean;
    function GetItems(I: integer): TVectorMapElementList;
    procedure SetVectorMapDir(const Value: string);
    procedure SetVectorMapCustomDir(const Value: string);
    procedure SetDrawInfoPoints(const Value: boolean);
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); override;
    procedure HTTPComplete(ARequest: TBackgroundHTTPRequest); override;
    procedure HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest); override;
  public
    constructor Create;
    function FindZone(AZoneNum: integer) : TVectorMapElementList;

    property DrawInfoPoints: boolean read FDrawInfoPoints write SetDrawInfoPoints;
    property Items[I: integer]: TVectorMapElementList read GetItems; default;
    property VectorMapDir: string read FVectorMapDir write SetVectorMapDir;
    property VectorMapCustomDir: string read FVectorMapCustomDir write SetVectorMapCustomDir;
    property MapBaseURL: string read FMapBaseURL write FMapBaseURL;
  end;

  TTextureMapElementListList = class(TZoneGLRenderObjectListList)
  private
    FTextureMapDir: string;
    FMapBaseURL: string;
    FTextureMapCustomDir: string;
    function GetItems(I: integer): TTextureMapElementList;
    procedure SetTextureMapDir(const Value: string);
    procedure SetTextureMapCustomDir(const Value: string);
  protected
    procedure AddZone(AZone: TDAOCZoneInfo); override;
    procedure HTTPComplete(ARequest: TBackgroundHTTPRequest); override;
    procedure HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest); override;
  public
    function FindZone(AZoneNum: integer) : TTextureMapElementList;

    property Items[I: integer]: TTextureMapElementList read GetItems; default;
    property TextureMapDir: string read FTextureMapDir write SetTextureMapDir;
    property TextureMapCustomDir: string read FTextureMapCustomDir write SetTextureMapCustomDir;
    property MapBaseURL: string read FMapBaseURL write FMapBaseURL;
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

        { POINT / INFOPOINT }
      if (CSV[0] = 'P') or (CSV[0] = 'I') then begin
        if CSV[0] = 'P' then
          tmpItem := TMapElementPoint.Create
        else begin
          tmpItem := TMapElementInfoPoint.Create;
          TMapElementInfoPoint(tmpItem).Enabled := FDrawInfoPoints;
        end;
        tmpItem.OffsetX := FOffsetX;
        tmpItem.OffsetY := FOffsetY;
        Add(tmpItem);
        with TMapElementPoint(tmpItem) do begin
          Name := CSV[1];
          SetColorFromString(CSV[2]);
          Assign(CSV.FieldAsInt(3, 0), CSV.FieldAsInt(4, 0), CSV.FieldAsInt(5, 0));
        end;
      end  { point }

        { LINE / FILLEDAREA }
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

constructor TVectorMapElementList.Create;
begin
  inherited;
  FDrawInfoPoints := true;
end;

procedure TVectorMapElementList.DeleteFile;
begin
  if FFileName <> '' then
    SysUtils.DeleteFile(FFileName);
end;

procedure TVectorMapElementList.LoadFromFile(const AFileName: string);
begin
  Clear;
  FFileName := AFileName;
  AppendFromFile(FFileName);
end;

procedure TVectorMapElementList.ReloadFile;
begin
  LoadFromFile(FFileName);
end;

procedure TVectorMapElementList.Save(const ATitle: string);
const
  CRLF: array[0..1] of char = (#13, #10);
var
  FS:   TFileStream;
  I:    integer;
  s:    string;
begin
  if FFileName = '' then
    raise Exception.Create('Filename is blank.');

  FS := TFileStream.Create(FFileName, fmCreate);
  FS.Write(ATitle[1], Length(ATitle));
  FS.Write(CRLF, sizeof(CRLF));

  for I := 0 to Count - 1 do begin
    s := Items[I].ToString + #13#10;
    FS.Write(s[1], Length(s));
  end;

  FS.Free;
end;

procedure TVectorMapElementList.SetDrawInfoPoints(const Value: boolean);
var
  I:    integer;
begin
  FDrawInfoPoints := Value;
  for I := 0 to Count - 1 do
    if Items[I] is TMapElementInfoPoint then
      TMapElementInfoPoint(Items[I]).Enabled := Value;
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

function TZoneGLRenderObjectListList.HaveLatestVersion(const ALocalFile,
  ASection, AKey: string): boolean;
var
  sVersionDate:   string;
  dtVersion:      TDateTime;
  dtFile:         TDateTime;
  yr, mo, da: WORD;
  hh, nn, ss: WORD;
begin
    { no version file?  I'll assume you have the best because I have no way to check }
  if FVersionFile = '' then begin
    Result := true;
    exit;
  end;

    { get the date of the latest version from the version file }
  with TINIFile.Create(FVersionFile) do begin
    sVersionDate := ReadString(ASection, AKey, '');
    Free;
  end;

    { the key wasn't in the file, or our percieved version file is missing,
      still no way to know if we have the latest version, so err on the safe side
      and assume we're up to date.  Returning false here might get us in an
      endless loop if the version file goes missing. }
  if sVersionDate = '' then begin
    Result := true;
    exit;
  end;

  if FileExists(ALocalFile) then
    dtFile := FileDateToDateTime(FileAge(ALocalFile))
  else begin
      { if we don't have the file at all, we're definately not up to date }
    Result := false;
    exit;
  end;

  yr := StrToIntDef(copy(sVersionDate, 1, 4), 1899);
  mo := StrToIntDef(copy(sVersionDate, 5, 2), 12);
  da := StrToIntDef(copy(sVersionDate, 7, 2), 30);
  hh := StrToIntDef(copy(sVersionDate, 9, 2), 0);
  nn := StrToIntDef(copy(sVersionDate, 11, 2), 0);
  ss := StrToIntDef(copy(sVersionDate, 13, 2), 0);

  dtVersion := EncodeDate(yr, mo, da) + EncodeTime(hh, nn, ss, 0);
  Result := dtFile >= dtVersion;
end;

procedure TZoneGLRenderObjectListList.HTTPComplete(ARequest: TBackgroundHTTPRequest);
begin
end;

procedure TZoneGLRenderObjectListList.HTTPDownload(const AURL, ADestFile: string; ATag: integer);
{$IFDEF MSWINDOWS}
var
  pHTTPRequest: TBackgroundHTTPRequest;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  try
      { zone didn't load.  Try to get it from the woooooooorld wide web }
    if FAttemptDownload and Assigned(FHTTPFetch) then begin
      pHTTPRequest := TBackgroundHTTPRequest.CreateGET;
      pHTTPRequest.URL := AURL;
      pHTTPRequest.Tag := ATag;
      pHTTPRequest.ResponseStream := TFileStream.Create(ADestFile, fmCreate);
      pHTTPRequest.OnRequestComplete := HTTPComplete;
      pHTTPRequest.OnHTTPError := HTTPError;
      FHTTPFetch.Request(pHTTPRequest);
    end;
  except
    on E: Exception do begin
      FAttemptDownload := false;
      raise;
    end;
  end;
{$ENDIF MSWINDOWS}
end;

procedure TZoneGLRenderObjectListList.HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest);
begin
end;

procedure TZoneGLRenderObjectListList.LoadForZone(AZone: TDAOCZoneInfo; ALoadAdjacent: boolean);
var
  I:    integer;
  bAddedRequested:  boolean;
begin
  if not Assigned(AZone) then begin
    GLCleanupExcept(-1);
    exit;
  end;

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
  sDestFileName:  string;
begin
  pTmpZone := TVectorMapElementList.Create;
  Add(pTmpZone);
  pTmpZone.ZoneNum := AZone.ZoneNum;
  pTmpZone.OffsetX := AZone.BaseLoc.X;
  pTmpZone.OffsetY := AZone.BaseLoc.Y;
  pTmpZone.DrawInfoPoints := FDrawInfoPoints;

  sDestFileName := FVectorMapCustomDir + AZone.MapName;
    { custom map overrides all! }
  if FileExists(sDestFileName) then
    pTmpZone.LoadFromFile(sDestFileName)

  else begin
    sDestFileName := FVectorMapDir + AZone.MapName;
    pTmpZone.FileName := sDestFileName;

    if not HaveLatestVersion(sDestFileName, 'vector', Format('zone%03d.map', [AZone.ZoneNum])) then begin
      ForceDirectories(FVectorMapDir);
      HTTPDownload(FMapBaseURL + 'f=vector&z=' + IntToStr(AZone.ZoneNum), sDestFileName,
        AZone.ZoneNum);
    end
    else
      pTmpZone.LoadFromFile(sDestFileName)
  end;
end;

constructor TVectorMapElementListList.Create;
begin
  inherited;
  FDrawInfoPoints := true;
end;

function TVectorMapElementListList.FindZone(AZoneNum: integer): TVectorMapElementList;
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

function TVectorMapElementListList.GetItems(I: integer): TVectorMapElementList;
begin
  Result := TVectorMapElementList(inherited Items[I]);
end;

{$IFDEF MSWINDOWS}
procedure TVectorMapElementListList.HTTPComplete(ARequest: TBackgroundHTTPRequest);
var
  pZoneVecList:  TVectorMapElementList;
begin
  pZoneVecList := FindZone(ARequest.Tag);
    { request is complete.  Attempt a reload }
  if Assigned(pZoneVecList) then begin
      { we have to close the stream to make sure window will share the file }
    ARequest.ResponseStream.Free;
    ARequest.ResponseStream := nil;
    pZoneVecList.ReloadFile;
  end;
end;

procedure TVectorMapElementListList.HTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest);
var
  pZoneVecList:  TVectorMapElementList;
begin
  pZoneVecList := FindZone(ARequest.Tag);
    { The damn response is the error message from the http server }
  if Assigned(pZoneVecList) then begin
      { close the filestream }
    ARequest.ResponseStream.Free;
    ARequest.ResponseStream := nil;
    pZoneVecList.DeleteFile;
  end;
end;
{$ENDIF MSWINDOWS}

procedure TVectorMapElementListList.SetDrawInfoPoints(const Value: boolean);
var
  I:    integer;
begin
  if FDrawInfoPoints = Value then
    exit;

  FDrawInfoPoints := Value;

  for I := 0 to Count - 1 do
    Items[I].DrawInfoPoints := Value;
end;

procedure TVectorMapElementListList.SetVectorMapCustomDir(const Value: string);
begin
  FVectorMapCustomDir := IncludeTrailingPathDelimiter(Value);
end;

procedure TVectorMapElementListList.SetVectorMapDir(const Value: string);
begin
  FVectorMapDir := IncludeTrailingPathDelimiter(Value);
end;

{ TTextureMapElementListList }

procedure TTextureMapElementListList.AddZone(AZone: TDAOCZoneInfo);
var
  pTmpZone:   TTextureMapElementList;
  sDestFileName:  string;
begin
  pTmpZone := TTextureMapElementList.Create;
  Add(pTmpZone);
  pTmpZone.ZoneNum := AZone.ZoneNum;
  pTmpZone.OffsetX := AZone.BaseLoc.X;
  pTmpZone.OffsetY := AZone.BaseLoc.Y;

  sDestFileName := Format('%szone%3.3d.dds', [FTextureMapCustomDir, AZone.ZoneNum]);
    { custom map overrides all! }
  if FileExists(sDestFileName) then
    pTmpZone.LoadFromSingleDDSFile(sDestFileName)

  else begin
    sDestFileName := Format('%szone%3.3d.dds', [FTextureMapDir, AZone.ZoneNum]);
    pTmpZone.FileName := sDestFileName;

    if not HaveLatestVersion(sDestFileName, 'dds', Format('zone%03d.dds', [AZone.ZoneNum])) then begin
      ForceDirectories(FTextureMapDir);
      HTTPDownload(FMapBaseURL + 'f=dds&z=' + IntToStr(AZone.ZoneNum), sDestFileName,
        AZone.ZoneNum);
    end
    else
      pTmpZone.LoadFromSingleDDSFile(sDestFileName);
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

function TTextureMapElementListList.GetItems(I: integer): TTextureMapElementList;
begin
  Result := TTextureMapElementList(inherited Items[I]);
end;

{$IFDEF MSWINDOWS}
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
{$ENDIF MSWINDOWS}

procedure TTextureMapElementListList.SetTextureMapCustomDir(const Value: string);
begin
  FTextureMapCustomDir := IncludeTrailingPathDelimiter(Value);
end;

procedure TTextureMapElementListList.SetTextureMapDir(const Value: string);
begin
  FTextureMapDir := IncludeTrailingPathDelimiter(Value);
end;

end.


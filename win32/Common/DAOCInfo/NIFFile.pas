unit NIFFile;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TBinaryReader = class(TObject)
  private
    FStrm:  TStream;
  public
    constructor Create(AStrm: TStream);
    destructor Destroy; override;

    function ReadStringLen : string;
    function ReadStringNTS : string;
    function ReadStringLF : string;
    function ReadInt : integer;
    function ReadWord : WORD;
    function ReadByte : BYTE;
    function ReadSingle : single;
    function Seek(const Offset: Integer; Origin: Word): Integer;
    function SeekOff(const Offset: integer) : integer;
  end;

  TNIFVersion = (nifUnknown, nif3, nif4);

  TNIFNode = class(TObject)
  protected
    FID:      integer;
    FVersion: TNIFVersion;
    procedure LoadFromBinReader3(ABR: TBinaryReader); virtual;
    procedure LoadFromBinReader4(ABR: TBinaryReader); virtual;
  public
    constructor Create; virtual;

    procedure LoadFromBinReader(ABR: TBinaryReader; AVer: TNIFVersion); virtual;
    property ID: integer read FID write FID;
  end;

  TNiAVObject = class(TNIFNode)
  protected
    FName:  string;
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    property Name: string read FName;
  end;

  TTransformableObject = class(TNiAVObject)
  private
    FSelectiveUpdateEnabled:  boolean;
    FSelectiveUpdatePropControllers:  boolean;
    FSelectiveUpdateTranforms:  boolean;
    FSelectiveUpdateRigid:      boolean;
    FRotation:    array[0..8] of single;
    FTranslation: array[0..2] of single;
    FScale:       single;
    // FExtraData:   TObjectList;
    function GetTranslationX: single;
    function GetTranslationY: single;
    function GetTranslationZ: single;
    function GetPropertyCount: integer;
    function GetProperties(I: integer): TNIFNode;
  protected
    FPropIDs:     array of Cardinal;
    FProperties:  TObjectList;
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property TranslationX: single read GetTranslationX;
    property TranslationY: single read GetTranslationY;
    property TranslationZ: single read GetTranslationZ;
    property Scale: single read FScale;
    property PropertyCount: integer read GetPropertyCount;
    property Properties[I: integer]: TNIFNode read GetProperties;
  end;

  TNiNode = class(TTransformableObject)
  private
    function GetChildCount: integer;
    function GetChildren(I: integer): TNIFNode;
  protected
    FChildIDs:    array of Cardinal;
    FChildren:    TObjectList;
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ChildByName(const AName: string) : TNIFNode;
    
    property ChildCount: integer read GetChildCount;
    property Children[I: integer]: TNIFNode read GetChildren;
  end;

  TNiTriShape = class(TTransformableObject)
  private
    FMeshID:    integer;
    FMesh:      TNIFNode;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    property Mesh: TNIFNode read FMesh;
  end;

  TNiProperty = class(TNiAVObject)
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiSwitchNode = class(TNiNode)
  private
    //FUpdateOnlyActive:  boolean;
    FIndex:   integer;
  protected
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiLODNode = class(TNiSwitchNode)
  private
    FCenter:  array[0..2] of single;
    FN:   single;
    FF:   single;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiZBufferProperty = class(TNiProperty)
  private
    FZTest:   boolean;
    FZWrite:  boolean;
    FZTestType:   integer;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
  end;

  TNiTextureModeProperty = class(TNiProperty)
  private
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiVertexColorProperty = class(TNiProperty)
  private
    FLightingMode:  integer;
    FVertexMode:    integer;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiMaterialProperty = class(TNiProperty)
  private
    FShininess:   single;
    FAlpha:       single;
    FAmbientColor:  array[0..2] of single;
    FDiffuseColor:  array[0..2] of single;
    FSpecularColor: array[0..2] of single;
    FEmissiveColor: array[0..2] of single;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiMultiTextureProperty = class(TNiProperty)
  private
    FTextureID1:  integer;
    FTextureID2:  integer;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
  end;

  TNiTextureProperty = class(TNiProperty)
  private
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  PNIFTexturingPropertyRec = ^TNIFTexturingPropertyRec;
  TNIFTexturingPropertyRec = record
    TexID:      integer;
    TextureSet: integer;
    ClampMode:  Cardinal;
    FilterMode: Cardinal;
    ApplyMode:  Cardinal;
    PS2LSettings: SmallInt;  // signed 16-bit
    PS2KSettings: SmallInt;
  end;

  TNiTexturingProperty = class(TNiProperty)
  private
    function GetTextureProps(I: integer) : PNIFTexturingPropertyRec;
  protected
    FNumTextures:   integer;
    FTexPropRecs:   array of TNIFTexturingPropertyRec;
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    property TextureCount: integer read FNumTextures;
    property TextureProps[I: integer]: PNIFTexturingPropertyRec read GetTextureProps;
  end;

  TNiExtraData = class(TNIFNode)
  protected
    FNext:    integer;
    FUISize:  integer;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  TNiStringExtraData = class(TNiExtraData)
  private
    FString:  string;
  protected
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  end;

  PNIFVertex = ^TNIFVertex;
  TNIFVertex = packed record
    x, y, z: single;
  end;

  PNIFColor = ^TNIFColor;
  TNIFColor = packed record
    r, g, b, a: single;
  end;

  PNIFTexCoord = ^TNIFTexCoord;
  TNIFTexCoord = packed record
    s, t: single;
  end;

  TNiTriShapeData = class(TNIFNode)
  private
    function GetColors(I: integer): PNIFColor;
    function GetNormals(I: integer): PNIFVertex;
    function GetTexCoords(I: integer): PNIFTexCoord;
    function GetTriangleIndicies(I: integer): WORD;
    function GetVerticies(I: integer): PNIFVertex;
  protected
    FHasNormals:    boolean;
    FHasTexCoords:  boolean;
    FHasColors:     boolean;
    FVertexCount:   integer;
    FVerticies:     array of TNIFVertex;
    FNormals:       array of TNIFVertex;
    FColors:        array of TNIFColor;
    FTexCoords:     array of TNIFTexCoord;
    FTriangleCount: integer;
    FTriangleIndicieCount:  integer;
    FTriangleIndicies:   array of WORD;  // indexes into FVerticies
    procedure ReadTexCoords(ABR: TBinaryReader; iNumTextures: integer);
    procedure ReadColors(ABR: TBinaryReader);
    procedure ReadNormals(ABR: TBinaryReader);
    procedure ReadUnkIndexes(ABR: TBinaryReader; iCount: integer);
    procedure ReadVerticies(ABR: TBinaryReader);

    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    property HasColors: boolean read FHasColors;
    property HasNormals: boolean read FHasNormals;
    property HasTexCoords: boolean read FHasTexCoords;
    property VertexCount: integer read FVertexCount;
    property Verticies[I: integer]: PNIFVertex read GetVerticies;
    property Normals[I: integer]: PNIFVertex read GetNormals;
    property Colors[I: integer]: PNIFColor read GetColors;
    property TexCoords[I: integer]: PNIFTexCoord read GetTexCoords;
    property TriangleCount: integer read FTriangleCount;
    property TriangleIndicieCount: integer read FTriangleIndicieCount;
    property TriangleIndicies[I: integer]: WORD read GetTriangleIndicies;
  end;

  TNiImage = class(TNIFNode)
  private
    FTextureName:   string;
  protected
    procedure LoadFromBinReader3(ABR: TBinaryReader); override;
    procedure LoadFromBinReader4(ABR: TBinaryReader); override;
  public
    property TextureName: string read FTextureName;
  end;

  TLogNotify = procedure (const s: string) of object;

  TNIFFile = class(TObject)
  private
    FNIFVersion:  TNIFVersion;
    FNodes:       TObjectList;
    function NodeByID(AID: integer) : TNIFNode;
    function StrToNIFVersion(const AVerStr: string) : TNIFVersion;
    procedure Log(const s: string);
    procedure SeekToNextNiNode(AStrm: TStream);
    procedure LinkChildren;
    procedure LinkMeshes;
    procedure LinkProperties;
    function GetNodes(I: integer) : TNIFNode;
    function GetRoot: TNiNode;
    function GetNodeCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(AStrm: TStream);
    procedure LoadFromFile(const AFileName: string);

    property Root: TNiNode read GetRoot;
    property NodeCount: integer read GetNodeCount;
    property Nodes[I: integer]: TNIFNode read GetNodes;
  end;

var
  Logger:   TLogNotify;

implementation

{ TNIFFile }

procedure TNIFFile.Clear;
begin
  FNodes.Clear;
end;

constructor TNIFFile.Create;
begin
  inherited Create;
  FNodes := TObjectList.Create;
end;

destructor TNIFFile.Destroy;
begin
  Clear;
  FNodes.Free;
  
  inherited Destroy;
end;

function TNIFFile.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

function TNIFFile.GetNodes(I: integer): TNIFNode;
begin
  Result := TNIFNode(FNodes[I]);
end;

function TNIFFile.GetRoot: TNiNode;
begin
  if (FNodes.Count > 0) and (Nodes[0].ID = 0) then
    Result := Nodes[0] as TNiNode
  else
    Result := nil;
end;

procedure TNIFFile.LinkChildren;
var
  I:    integer;
  iChild:   integer;
begin
  for I := 0 to FNodes.Count - 1 do
    if Nodes[I] is TNiNode then
      with TNiNode(Nodes[I]) do begin
        FChildren.Clear;
        for iChild := 0 to ChildCount - 1 do
          FChildren.Add(NodeByID(FChildIDs[iChild]));
      end;  { if NiNode / with NiNode }
end;

procedure TNIFFile.LinkMeshes;
var
  I:    integer;
begin
  for I := 0 to FNodes.Count - 1 do
    if Nodes[I] is TNiTriShape then
      with TNiTriShape(Nodes[I]) do
        FMesh := NodeByID(FMeshID);
end;

procedure TNIFFile.LinkProperties;
var
  I:        integer;
  iProp:    integer;
begin
  for I := 0 to FNodes.Count - 1 do
    if Nodes[I] is TTransformableObject then
      with TTransformableObject(Nodes[I]) do begin
        FProperties.Clear;
        for iProp := 0 to PropertyCount - 1 do
          FProperties.Add(NodeByID(FPropIDs[iProp]));
      end;  { if NiNode / with NiNode }
end;

procedure TNIFFile.LoadFromFile(const AFileName: string);
var
  FS:   TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TNIFFile.LoadFromStream(AStrm: TStream);
var
  BR:     TBinaryReader;
  tmpNode:    TNIFNode;
  sNodeType:  string;
  sVersion:   string;
  dwID:       Cardinal;
begin
  BR := TBinaryReader.Create(AStrm);
  try
    sVersion := BR.ReadStringLF;   // NetImmerse File Format, Version 3.1
    FNIFVersion := StrToNIFVersion(sVersion);
    if FNifVersion = nif3 then begin
      BR.ReadStringLF;   // Numerical Design Limited, Chapel Hill, NC 27514
      BR.ReadStringLF;   // Copyright (c) 1996-2000
      BR.ReadStringLF;   // All Rights Reserved

      BR.ReadStringLen;  // Top Level Object
    end
    else begin
      // First 4 bytes is byte-wise NI version again LSB first dcba
      BR.SeekOff(8);
    end;

    dwID := 0;
    while AStrm.Position <> AStrm.Size do begin
      sNodeType := BR.ReadStringLen;
      if (sNodeType = '') or AnsiSameText(sNodeType, 'End Of File') then
        continue;
      if AnsiSameText(sNodeType, 'NiNode') then
        tmpNode := TNiNode.Create
      else if AnsiSameText(sNodeType, 'NiLODNode') then
        tmpNode := TNiLODNode.Create
      else if AnsiSameText(sNodeType, 'NiTriShape') then
        tmpNode := TNiTriShape.Create
      else if AnsiSameText(sNodeType, 'NiZBufferProperty') then
        tmpNode := TNiZBufferProperty.Create
      else if AnsiSameText(sNodeType, 'NiVertexColorProperty') then
        tmpNode := TNiVertexColorProperty.Create
      else if AnsiSameText(sNodeType, 'NiTextureModeProperty') then
        tmpNode := TNiTextureModeProperty.Create
      else if AnsiSameText(sNodeType, 'NiMaterialProperty') then
        tmpNode := TNiMaterialProperty.Create
      else if AnsiSameText(sNodeType, 'NiTriShapeData') then
        tmpNode := TNiTriShapeData.Create
      else if AnsiSameText(sNodeType, 'NiMultiTextureProperty') then
        tmpNode := TNiMultiTextureProperty.Create
      else if AnsiSameText(sNodeType, 'NiTextureProperty') then
        tmpNode := TNiTextureProperty.Create
      else if AnsiSameText(sNodeType, 'NiTexturingProperty') then
        tmpNode := TNiTexturingProperty.Create
      else if AnsiSameText(sNodeType, 'NiImage') then
        tmpNode := TNiImage.Create
      else if AnsiSameText(sNodeType, 'NiSourceTexture') then
        tmpNode := TNiImage.Create
      else
        tmpNode := nil;

      Log(Format('%s (%u)', [sNodeType, dwID]));

      if not Assigned(tmpNode) then begin
        Log(Format('  Unknown NiNode type at offset (%d)', [AStrm.Position]));
        SeekToNextNiNode(AStrm);
      end
      else begin
        tmpNode.ID := dwID;

        FNodes.Add(tmpNode);
        tmpNode.LoadFromBinReader(BR, FNIFVersion);

        if (tmpNode is TNiAVObject) and (TNiAVObject(tmpNode).Name <> '') then
          Log('  "' + TNiAVObject(tmpNode).Name + '"');
        if (tmpNode is TNiImage) and (TNiImage(tmpNode).TextureName <> '') then
          Log('  ->' + TNiImage(tmpNode).TextureName);
      end;  { if know that node type }

      inc(dwID);
    end;  { while !EOF }
  finally
    BR.Free;
  end;

  LinkChildren;
  LinkMeshes;
  LinkProperties;
end;

procedure TNIFFile.Log(const s: string);
begin
  if Assigned(Logger) then
    Logger(s);
  //WriteLn(s);
end;

function TNIFFile.NodeByID(AID: integer): TNIFNode;
var
  I:    integer;
begin
  if (AID < FNodes.Count) and (Nodes[AID].ID = AID) then begin
    Result := Nodes[AID];
    exit;
  end;

  for I := 0 to FNodes.Count - 1 do
    if Nodes[I].ID = AID then begin
      Result := Nodes[I];
      exit;
    end;

  Result := nil;
end;

procedure TNIFFile.SeekToNextNiNode(AStrm: TStream);
var
  iMatchChars:  integer;
  bChar:  BYTE;
//  iExpectedLen: integer;
begin
    { super slow, but working, look for n,0,0,0,Ni(n-2 printable chars) }
  iMatchChars := 0;
  while AStrm.Position <> AStrm.Size do begin
    AStrm.Read(bChar, sizeof(bChar));

    if (iMatchChars = 5) and (char(bChar) = 'i') then begin
        { match! }
      AStrm.Seek(-6, soFromCurrent);
      exit;
    end

    else if ((iMatchChars = 4) and (char(bChar) = 'N')) or
      ((bChar = 0) and (iMatchChars in [1..4])) then
      inc(iMatchChars)

    else if bChar in [3..32] then begin
//      iExpectedLen := bChar;
      iMatchChars := 1;
    end

    else
      iMatchChars := 0;
  end;  { while !EOF }
end;

function TNIFFile.StrToNIFVersion(const AVerStr: string): TNIFVersion;
var
  iPos: integer;
begin
  iPos := Pos('Version ', AVerStr);
  if iPos <> 0 then
    Result := TNIFVersion(ord(AVerStr[iPos + 8]) - ord('2'))
  else
    Result := nifUnknown;
end;

{ TNiNode }

function TNiNode.ChildByName(const AName: string): TNIFNode;
var
  I:    integer;
begin
  for I := 0 to FChildren.Count - 1 do
    if Assigned(FChildren[I]) and (FChildren[I] is TNiAVObject) and
      AnsiSameText(TNiAVObject(FChildren[I]).Name, AName) then begin
      Result := Children[I];
      exit;
    end;

  Result := nil;
end;

constructor TNiNode.Create;
begin
  inherited Create;
  FChildren := TObjectList.Create(false);
end;

destructor TNiNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TNiNode.GetChildCount: integer;
begin
  Result := Length(FChildIDs);
end;

function TNiNode.GetChildren(I: integer): TNIFNode;
begin
  Result := TNIFNode(FChildren[I]);
end;

procedure TNiNode.LoadFromBinReader3(ABR: TBinaryReader);
var
  iChildCount:  integer;
  I:    integer;
begin
  inherited;

  iChildCount := ABR.ReadInt;
  if iChildCount > 0 then begin
    SetLength(FChildIDs, iChildCount);
    for I := 0 to iChildCount - 1 do
      FChildIDs[I] := ABR.ReadInt;
  end;   { if has children }

  ABR.SeekOff(4);
end;

procedure TNiNode.LoadFromBinReader4(ABR: TBinaryReader);
var
  iChildCount:    integer;
  I:    integer;
begin
  inherited;
  iChildCount := ABR.ReadInt;
  if iChildCount > 0 then begin
    SetLength(FChildIDs, iChildCount);
    for I := 0 to iChildCount - 1 do
      FChildIDs[I] := ABR.ReadInt;
  end;   { if has children }

  ABR.SeekOff(4);

(*

  if bHasExtraData then begin
    repeat
      sNodeType := ABR.ReadStringLen;
      if AnsiSameText(sNodeType, 'NiStringExtraData') then
        pExtraData := TNiStringExtraData.Create
      else
        pExtraData := nil;

      if not Assigned(pExtraData) then
        raise Exception.Create('Unknown extra data name: ' + sNodeType);

      FExtraData.Add(pExtraData);
      pExtraData.LoadFromBinReader4(ABR);
    until pExtraData.FNext = -1;
  end;  { if extra data }

  for I := 0 to iPropCount - 1 do begin
    sNodeType := ABR.ReadStringLen;
    if AnsiSameText(sNodeType, 'NiZBufferProperty') then
      pProp := TNiZBufferProperty.Create
    else if AnsiSameText(sNodeType, 'NiVertexColorProperty') then
      pProp := TNiVertexColorProperty.Create
    else
      pProp := nil;

    if not Assigned(pProp) then
      raise Exception.Create('Unknown property name: ' + sNodeType);

    FProperties.Add(pProp);
    pProp.LoadFromBinReader4(ABR);
  end;  { for I to propcount }

  if (iPropCount = 0) and (iChildCount = 0) then
    ABR.SeekOff(4);
*)
end;

{ TNiZBufferProperty }

procedure TNiZBufferProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;

  FZTest := ABR.ReadWord = 0;
  FZWrite := ABR.ReadWord = 0;
    // 0: TEST_ALWAYS
    // 1: TEST_LESS
    // 2: TEST_EQUAL
    // 3: TEST_LESSEQUAL
    // 4: TEST_GREATER
    // 5: TEST_NOTEQUAL
    // 6: TEST_GREATEREQUAL
    // 7: TEST_NEVER
  FZTestType := ABR.ReadWord;
end;

procedure TNiZBufferProperty.LoadFromBinReader4(ABR: TBinaryReader);
var
  wZ:   WORD;
begin
  inherited;

  wZ := ABR.ReadWord;
  FZTest := (wZ and $01) <> 0;
  FZWrite := (wZ and $02) <> 0;
    // 0: TEST_ALWAYS
    // 1: TEST_LESS
    // 2: TEST_EQUAL
    // 3: TEST_LESSEQUAL
    // 4: TEST_GREATER
    // 5: TEST_NOTEQUAL
    // 6: TEST_GREATEREQUAL
    // 7: TEST_NEVER
  FZTestType := ABR.ReadWord;

  ABR.SeekOff(2);
end;

{ TNIFNode }

constructor TNIFNode.Create;
begin
;
end;

procedure TNIFNode.LoadFromBinReader(ABR: TBinaryReader; AVer: TNIFVersion);
begin
  FVersion := AVer;
  case AVer of
    nif3:  LoadFromBinReader3(ABR);
    nif4:  LoadFromBinReader4(ABR);
  end;
end;

procedure TNIFNode.LoadFromBinReader3(ABR: TBinaryReader);
begin
  FID := ABR.ReadInt;
end;

procedure TNIFNode.LoadFromBinReader4(ABR: TBinaryReader);
begin
;
end;

{ TBinaryReader }

constructor TBinaryReader.Create(AStrm: TStream);
begin
  inherited Create;
  FStrm := AStrm;
end;

destructor TBinaryReader.Destroy;
begin
  inherited Destroy;
end;

function TBinaryReader.ReadByte: BYTE;
begin
  FStrm.Read(Result, sizeof(Result));
end;

function TBinaryReader.ReadInt: integer;
begin
  FStrm.Read(Result, sizeof(Result));
end;

function TBinaryReader.ReadSingle: single;
begin
  FStrm.Read(Result, sizeof(Result));
end;

function TBinaryReader.ReadStringLen: string;
var
  iLen:   integer;
begin
  iLen := ReadInt;
  if iLen = 1 then begin
    SeekOff(4);
    Result := '';
  end
  else if iLen > 0 then begin
    if iLen > 20 then
      iLen := iLen;
    SetLength(Result, iLen);
    FStrm.Read(Result[1], iLen);
  end
  else
    Result := '';
end;

function TBinaryReader.ReadStringLF: string;
var
  c:  char;
begin
  Result := '';

  while (FStrm.Position <> FStrm.Size) do begin
    FStrm.Read(c, sizeof(c));
    if c = #10 then
      break;
    Result := Result + c;
  end;
end;

function TBinaryReader.ReadStringNTS: string;
var
  c:  char;
begin
  Result := '';

  while (FStrm.Position <> FStrm.Size) do begin
    FStrm.Read(c, sizeof(c));
    if c = #0 then
      break;
    Result := Result + c;
  end;
end;

function TBinaryReader.ReadWord: word;
begin
  FStrm.Read(Result, sizeof(Result));
end;

function TBinaryReader.Seek(const Offset: Integer; Origin: Word): integer;
begin
  Result := FStrm.Seek(Offset, Origin);
end;

function TBinaryReader.SeekOff(const Offset: integer): integer;
begin
  Result := FStrm.Seek(Offset, soFromCurrent);
end;

{ TNiProperty }

procedure TNiProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
end;

procedure TNiProperty.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
end;

{ TNiVertexColorProperty }

procedure TNiVertexColorProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;

// ???
  ABR.SeekOff(10);
    // 0: SOURCE_IGNORE
  FVertexMode := ABR.ReadWord;  // AKA  "Source"
    // 0: LIGHTING_E
    // 1: LIGHTING_E_A_D
  FLightingMode := ABR.ReadWord;
//  ABR.SeekOff(2);
end;

procedure TNiVertexColorProperty.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
  ABR.SeekOff(4);
    // 0: SOURCE_IGNORE
  FVertexMode := ABR.ReadWord;  // AKA  "Source"
    // 0: LIGHTING_E
    // 1: LIGHTING_E_A_D
  FLightingMode := ABR.ReadWord;
  ABR.SeekOff(2);
end;

{ TNiStringExtraData }

procedure TNiStringExtraData.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
  FString := ABR.ReadStringLen;  
end;

{ TNiAVObject }

procedure TNiAVObject.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;

  FName := ABR.ReadStringLen;
  ABR.SeekOff(4);
end;

procedure TNiAVObject.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;

  FName := ABR.ReadStringLen;
  ABR.ReadInt;  // FID := always -1 in version 4
  ABR.SeekOff(4);
end;

{ TNiExtraData }

procedure TNiExtraData.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;

  FNext := ABR.ReadInt;
  FUISize := ABR.ReadInt;
end;

{ TNiLODNode }

procedure TNiLODNode.LoadFromBinReader3(ABR: TBinaryReader);
var
  I:          integer;
  iLODCount:  integer;
begin
  inherited;

  ABR.SeekOff(4);
//  for I := 0 to 2 do
//    FCenter[I] := ABR.ReadSingle;

  iLODCount := ABR.ReadInt;
  for I := 0 to iLODCount - 1 do begin
    FN := ABR.ReadSingle;
    FF := ABR.ReadSingle;

    ABR.SeekOff(12);   // (x,y,z)
  end;
end;

procedure TNiLODNode.LoadFromBinReader4(ABR: TBinaryReader);
var
  I:          integer;
  iLODCount:  integer;
begin
  inherited;

  for I := 0 to 2 do
    FCenter[I] := ABR.ReadSingle;

  iLODCount := ABR.ReadInt;
  for I := 0 to iLODCount - 1 do begin
    FN := ABR.ReadSingle;  { BRY: Need to make an array of these }
    FF := ABR.ReadSingle;
  end;
end;

{ TNiSwitchNode }

procedure TNiSwitchNode.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;

  FIndex := ABR.ReadInt;
end;

{ TNiTriShape }

procedure TNiTriShape.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;

  FMeshID := ABR.ReadInt;
end;

procedure TNiTriShape.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;

  FMeshID := ABR.ReadInt;
  ABR.SeekOff(4);
end;

{ TNiTextureModeProperty }

procedure TNiTextureModeProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
  ABR.SeekOff(4);
  { seen C2 00 00 00 here }
  ABR.ReadInt;
  { B5 FF here }
  ABR.ReadWord;
end;

procedure TNiTextureModeProperty.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
end;

{ TTransformableObject }

constructor TTransformableObject.Create;
begin
  inherited;
  FProperties := TObjectList.Create(false);
//  FExtraData := TObjectList.Create;
end;

destructor TTransformableObject.Destroy;
begin
//  FExtraData.Free;
  FProperties.Free;
  inherited;
end;

function TTransformableObject.GetProperties(I: integer): TNIFNode;
begin
  if (I < 0) or (I >= Length(FPropIDs)) then
    raise Exception.CreateFmt('Property index %d out of range', [I]);
  Result := TNIFNode(FProperties[I]);
end;

function TTransformableObject.GetPropertyCount: integer;
begin
  Result := Length(FPropIDs);
end;

function TTransformableObject.GetTranslationX: single;
begin
  Result := FTranslation[0];
end;

function TTransformableObject.GetTranslationY: single;
begin
  Result := FTranslation[1];
end;

function TTransformableObject.GetTranslationZ: single;
begin
  Result := FTranslation[2];
end;

procedure TTransformableObject.LoadFromBinReader3(ABR: TBinaryReader);
var
  I:    integer;
  iPropCount:   integer;
begin
  inherited;

  ABR.SeekOff(6);
  for I := 0 to 2 do
    FTranslation[I] := ABR.ReadSingle;
  for I := 0 to 8 do
    FRotation[I] := ABR.ReadSingle;
  FScale := ABR.ReadSingle;

  ABR.SeekOff(12);
  iPropCount := ABR.ReadInt;
  if iPropCount > 0 then
    ABR.SeekOff(iPropCount * 4);

  ABR.SeekOff(4);  // another list length?
end;

procedure TTransformableObject.LoadFromBinReader4(ABR: TBinaryReader);
var
  I:          integer;
  iPropCount: integer;
//  sNodeType:  string;
//  pProp:      TNiProperty;
//  bHasExtraData:  boolean;
//  pExtraData: TNiExtraData;
  bFlags:   BYTE;
begin
  inherited;

  bFlags := ABR.ReadByte;
  FSelectiveUpdateEnabled := (bFlags and $10) <> 0;
  FSelectiveUpdatePropControllers := (bFlags and $20) <> 0;
  FSelectiveUpdateTranforms := (bFlags and $40) <> 0;
  FSelectiveUpdateRigid := (bFlags and $80) <> 0;

    { seen 0 and 1 }
  ABR.SeekOff(1);

  for I := 0 to 2 do
    FTranslation[I] := ABR.ReadSingle;
  for I := 0 to 8 do
    FRotation[I] := ABR.ReadSingle;
  FScale := ABR.ReadSingle;

  ABR.SeekOff(12);
  iPropCount := ABR.ReadInt;
  if iPropCount > 0 then begin
    SetLength(FPropIDs, iPropCount);
    for I := 0 to iPropCount - 1 do
      FPropIDs[I] := ABR.ReadInt;
  end;
(**
    bHasExtraData := (ABR.ReadInt and $08) <> 0;
    if (ABR.ReadInt and $0100) <> 0 then
      ABR.SeekOff(4);
  end
  else
    bHasExtraData := false;
**)
  ABR.SeekOff(1);
end;

{ TNiMaterialProperty }

procedure TNiMaterialProperty.LoadFromBinReader3(ABR: TBinaryReader);
var
  I:    integer;
begin
  inherited;

  ABR.SeekOff(6);

  for I := 0 to 2 do
    FAmbientColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FDiffuseColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FSpecularColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FEmissiveColor[I] := ABR.ReadSingle;

  FShininess := ABR.ReadSingle;
  FAlpha := ABR.ReadSingle;
end;

{ TNiTriStripData }

function TNiTriShapeData.GetColors(I: integer): PNIFColor;
begin
  if I >= FVertexCount then
    raise Exception.CreateFmt('Vertex color %d out of bounds', [I]);
  Result := @FColors[I];
end;

function TNiTriShapeData.GetNormals(I: integer): PNIFVertex;
begin
  if I >= FVertexCount then
    raise Exception.CreateFmt('Vertex normal %d out of bounds', [I]);
  Result := @FNormals[I];
end;

function TNiTriShapeData.GetTexCoords(I: integer): PNIFTexCoord;
begin
  if I >= FVertexCount then
    raise Exception.CreateFmt('Vertex texturecoord %d out of bounds', [I]);
  Result := @FTexCoords[I];
end;

function TNiTriShapeData.GetTriangleIndicies(I: integer): WORD;
begin
  if I >= FTriangleIndicieCount then
    raise Exception.CreateFmt('Triangle indicie %d out of bounds', [I]);
  Result := FTriangleIndicies[I];
end;

function TNiTriShapeData.GetVerticies(I: integer): PNIFVertex;
begin
  if I >= FVertexCount then
    raise Exception.CreateFmt('Vertex %d out of bounds', [I]);
  Result := @FVerticies[I];
end;

procedure TNiTriShapeData.LoadFromBinReader3(ABR: TBinaryReader);
var
  I:  integer;
  a:  integer;
  iNumTextures:  integer;
begin
  inherited;

  FVertexCount := ABR.ReadWord;
  ABR.SeekOff(4);
  ReadVerticies(ABR);

  if ABR.ReadInt <> 0 then
    ReadNormals(ABR);
  if ABR.ReadInt <> 0 then
    ReadColors(ABR);
  iNumTextures := ABR.ReadWord;
  if ABR.ReadInt <> 0 then
    ReadTexCoords(ABR, iNumTextures);

  FTriangleCount := ABR.ReadWord; // triangle count
  FTriangleIndicieCount := ABR.ReadWord; // index count
  ABR.SeekOff(2);   // texture sets?

  SetLength(FTriangleIndicies, FTriangleIndicieCount);
  for I := 0 to FTriangleIndicieCount - 1 do
    FTriangleIndicies[I] := ABR.ReadWord;

  a := ABR.ReadWord;
  if a <> 0 then
    ReadUnkIndexes(ABR, a);
end;

procedure TNiMaterialProperty.LoadFromBinReader4(ABR: TBinaryReader);
var
  I:    integer;
begin
  inherited;

 ABR.SeekOff(2);

  for I := 0 to 2 do
    FAmbientColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FDiffuseColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FSpecularColor[I] := ABR.ReadSingle;
  for I := 0 to 2 do
    FEmissiveColor[I] := ABR.ReadSingle;

  FShininess := ABR.ReadSingle;
  FAlpha := ABR.ReadSingle;
end;

{ TNiMultiTextureProperty }

procedure TNiMultiTextureProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
  ABR.SeekOff(10);
  FTextureID1 := ABR.ReadInt;
  FTextureID2 := ABR.ReadInt;
  ABR.SeekOff(34);
end;

{ TNiImage }

procedure TNiImage.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
  ABR.SeekOff(1);  // property count

  FTextureName := ABR.ReadStringLen;

  { Size, mip map, pixellayout, alpha format, clamp mode, filter mode,
    apply mode, etc?  Might also be in NiTextureProperty }
  ABR.SeekOff(8);
end;

procedure TNiImage.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
  ABR.SeekOff(12);
  ABR.SeekOff(1);  // property count

  FTextureName := ABR.ReadStringLen;

  { Size, mip map, pixellayout, alpha format, clamp mode, filter mode,
    apply mode, etc?  Might also be in NiTexturingProperty }
  ABR.SeekOff(13);
end;

{ TNiTextureProperty }

procedure TNiTextureProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
  { Size, mip map, pixellayout, alpha format, clamp mode, filter mode,
    apply mode, etc?  Might also be in NiImage }
  ABR.SeekOff(10);
end;

procedure TNiTriShapeData.LoadFromBinReader4(ABR: TBinaryReader);
var
  I:  integer;
  a:  integer;
  iNumTextures:  integer;
begin
  inherited;

  FVertexCount := ABR.ReadWord;
  ABR.SeekOff(1);
  ReadVerticies(ABR);

  if ABR.ReadByte <> 0 then
    ReadNormals(ABR);
  if ABR.ReadByte <> 0 then
    ReadColors(ABR);

  iNumTextures := ABR.ReadByte;
  ABR.ReadByte;
  if iNumTextures <> 0 then
    ReadTexCoords(ABR, iNumTextures);

  FTriangleCount := ABR.ReadWord; // triangle count
  FTriangleIndicieCount := ABR.ReadWord; // index count
  ABR.SeekOff(2);  // texture sets?

  SetLength(FTriangleIndicies, FTriangleIndicieCount);
  for I := 0 to FTriangleIndicieCount - 1 do
    FTriangleIndicies[I] := ABR.ReadWord;

  a := ABR.ReadWord;
  if a <> 0 then
    ReadUnkIndexes(ABR, a);
end;

procedure TNiTextureProperty.LoadFromBinReader4(ABR: TBinaryReader);
begin
  inherited;
  { Size, mip map, pixellayout, alpha format, clamp mode, filter mode,
    apply mode, etc?  Might also be in NiSourceTexture }
  ABR.SeekOff(39);
end;

procedure TNiTriShapeData.ReadColors(ABR: TBinaryReader);
var
  I:    integer;
begin
  FHasColors := true;
  SetLength(FColors, FVertexCount);
  for I := 0 to FVertexCount - 1 do begin
    FColors[I].r := ABR.ReadSingle;
    FColors[I].g := ABR.ReadSingle;
    FColors[I].b := ABR.ReadSingle;
    FColors[I].a := ABR.ReadSingle;
    // Log(Format('%d r%f, g%f, b%f, a%f', [I, FColors[I].r, FColors[I].g, FColors[I].b, FColors[I].a]));
  end;
end;

procedure TNiTriShapeData.ReadNormals(ABR: TBinaryReader);
var
  I:    integer;
begin
  FHasNormals := true;
  SetLength(FNormals, FVertexCount);
  for I := 0 to FVertexCount - 1 do begin
    FNormals[I].x := ABR.ReadSingle;
    FNormals[I].y := ABR.ReadSingle;
    FNormals[I].z := ABR.ReadSingle;
    // Log(Format('%d n%f, n%f, n%f', [I, FNormals[I].x, FNormals[I].y, FNormals[I].z]));
  end;

  ABR.SeekOff(16);
end;

procedure TNiTriShapeData.ReadTexCoords(ABR: TBinaryReader; iNumTextures: integer);
var
  I:    integer;
begin
  FHasTexCoords := true;
  SetLength(FTexCoords, FVertexCount);
  for I := 0 to (FVertexCount * iNumTextures) - 1 do
    if I < FVertexCount then begin
      FTexCoords[I].s := ABR.ReadSingle;
      FTexCoords[I].t := ABR.ReadSingle;
      // Log(Format('%d s%f, t%f', [I, FTexCoords[I].s, FTexCoords[I].t]));
    end
    else
      ABR.SeekOff(8);
end;

procedure TNiTriShapeData.ReadUnkIndexes(ABR: TBinaryReader;
  iCount: integer);
var
  I:    integer;
  a:    WORD;
begin
  for I := 0 to iCount - 1 do begin
    a := ABR.ReadWord;
    if a > 0 then
      ABR.SeekOff(a * 2);
  end;
end;

procedure TNiTriShapeData.ReadVerticies(ABR: TBinaryReader);
var
  I:    integer;
begin
  SetLength(FVerticies, FVertexCount);
  for I := 0 to FVertexCount - 1 do begin
    FVerticies[I].x := ABR.ReadSingle;
    FVerticies[I].y := ABR.ReadSingle;
    FVerticies[I].z := ABR.ReadSingle;
    // Log(Format('%d %f,%f,%f', [I, FVerticies[I].x, FVerticies[I].y, FVerticies[I].z]));
  end;
end;

{ TNiTexturingProperty }

function TNiTexturingProperty.GetTextureProps(I: integer): PNIFTexturingPropertyRec;
begin
  Result := @FTexPropRecs[I];
end;

procedure TNiTexturingProperty.LoadFromBinReader3(ABR: TBinaryReader);
begin
  inherited;
end;

procedure TNiTexturingProperty.LoadFromBinReader4(ABR: TBinaryReader);
var
  I:    integer;
  iCnt: integer;
  bContinue:  BYTE;
begin
  // 0 = Base Map
  // 1 = Dark Map
  // 5 = Bump Map
  inherited;
  ABR.SeekOff(2); // 00 00
  ABR.ReadInt;  // 02 00 00 00
  ABR.ReadInt;  // 07 00 00 00
  FNumTextures := 0;
  SetLength(FTexPropRecs, 7);

  for I := 0 to 6 do begin
      { first byte is 1 if this type is there, else 0 }  
    if ABR.ReadByte = 1 then begin
      FTexPropRecs[I].TexID := ABR.ReadInt;
      FTexPropRecs[I].ClampMode := ABR.ReadInt;  // 3 = WRAP_S_WRAP_T
      FTexPropRecs[I].FilterMode := ABR.ReadInt; // 2 = FILTER_TRILERP
      FTexPropRecs[I].TextureSet := ABR.ReadInt;
      FTexPropRecs[I].PS2LSettings := ABR.ReadWord;
      FTexPropRecs[I].PS2KSettings := ABR.ReadWord;

      if I = 5 then
        ABR.SeekOff(6 * 4);  // bumpmap has Luma Offset, Luma Scale, and a 2x2 matrix (all single)

      inc(FNumTextures);
    end;
  end;  { for I to numtextures }
end;

end.

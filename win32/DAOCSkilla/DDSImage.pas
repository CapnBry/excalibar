unit DDSImage;

(***
  Adapted by Bryan Mayland from DDS loader written by Jon Watte 2002
  Currently only works with DXT1
**)

interface

uses
  SysUtils, Classes, VCLMemStrms;
  
const
  DDS_HEADER_MAGIC = $20534444;  // 'DDS '

    { DDS_header.dwFlags }
  DDSD_CAPS                  = $00000001;
  DDSD_HEIGHT                = $00000002;
  DDSD_WIDTH                 = $00000004;
  DDSD_PITCH                 = $00000008;
  DDSD_PIXELFORMAT           = $00001000;
  DDSD_MIPMAPCOUNT           = $00020000;
  DDSD_LINEARSIZE            = $00080000;
  DDSD_DEPTH                 = $00800000;

    { DDS_header.sPixelFormat.dwFlags }
  DDPF_ALPHAPIXELS           = $00000001;
  DDPF_FOURCC                = $00000004;
  DDPF_INDEXED               = $00000020;
  DDPF_RGB                   = $00000040;

    { DDS_header.sCaps.dwCaps1 }
  DDSCAPS_COMPLEX            = $00000008;
  DDSCAPS_TEXTURE            = $00001000;
  DDSCAPS_MIPMAP             = $00400000;

    { DDS_header.sCaps.dwCaps2 }
  DDSCAPS2_CUBEMAP           = $00000200;
  DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
  DDSCAPS2_VOLUME            = $00200000;

  D3DFMT_DXT1     = $31545844;    //  DXT1 compression texture format
  D3DFMT_DXT2     = $32545844;    //  DXT2 compression texture format
  D3DFMT_DXT3     = $33545844;    //  DXT3 compression texture format
  D3DFMT_DXT4     = $34545844;    //  DXT4 compression texture format
  D3DFMT_DXT5     = $35545844;    //  DXT5 compression texture format

type
  TDDPixelFormat = packed record
    dwSize:   Cardinal;
    dwFlags:  Cardinal;
    dwFourCC: Cardinal;
    dwRGBBitCount:  Cardinal;
    dwRBitMask: Cardinal;
    dwGBitMask: Cardinal;
    dwBBitMask: Cardinal;
    dwAlphaBitMask: Cardinal;
  end;

  TDDCaps2 = packed record
    dwCaps1:    Cardinal;
    dwCaps2:    Cardinal;
    dwDDSX:     Cardinal;
    dwReserved: Cardinal;
  end;

  TDDSurfaceDesc2 = packed record
    dwSize:     Cardinal;
    dwFlags:    Cardinal;
    dwHeight:   Cardinal;
    dwWidth:    Cardinal;
    dwLinearSize:       Cardinal;
    dwBackBufferCount:  Cardinal;
    dwMipMapCount:      Cardinal;
    dwReserved1:    array[0..10] of Cardinal;
    sPixelFormat:   TDDPixelFormat;
    sCaps:      TDDCaps2;
    dwTextureStage: Cardinal;
  end;

  TDDSFileHeader = packed record
    dwMagic:      Cardinal;
    sSurfaceDesc: TDDSurfaceDesc2;
  end;

type
  TDDSImagePixelsChunk = class(TObject)
  private
    FPixelData: TMemoryStream;
    FInternalFormat: Cardinal;
    FHeight: integer;
    FWidth: integer;
    function GetPixels: Pointer;
    function GetPixelsSize: integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Pixels: Pointer read GetPixels;
    property PixelsSize: integer read GetPixelsSize;
    property Height: integer read FHeight;
    property Width:  integer read FWidth;
    property InternalFormat: Cardinal read FInternalFormat;
  end;

  TDDSImage = class(TObject)
  private
    FSwap: boolean;
    FCompressed: boolean;
    FPalettized: boolean;
    FexternalFormat: Cardinal;
    FdivSize: Cardinal;
    FByteType: Cardinal;
    FblockBytes: Cardinal;
    FInternalFormat: Cardinal;
    FHeight: integer;
    FWidth: integer;
    FPixelData: TMemoryStream;
    FMipMapCount: Cardinal;

    function GetPixels: Pointer;
    function GetPixelsSize: integer;
  private
    procedure InitializeDXT1;
    procedure InitializeDXT3;
    procedure InitializeDXT5;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    function CopyChunk(X, Y, H, W: Cardinal) : TDDSImagePixelsChunk;

    property MipMapCount: Cardinal read FMipMapCount;
    property Compressed: boolean read FCompressed;
    property Swap: boolean read FSwap;
    property Palettized: boolean read FPalettized;
    property divSize: Cardinal read FdivSize;
    property blockBytes: Cardinal read FblockBytes;
    property internalFormat: Cardinal read FInternalFormat;
    property externalFormat: Cardinal read FexternalFormat;
    property ByteType: Cardinal read FByteType;
    property Height: integer read FHeight;
    property Width: integer read FWidth;
    property Pixels: Pointer read GetPixels;
    property PixelsSize: integer read GetPixelsSize;
  end;

function PF_IS_DXT1(const pf: TDDPixelFormat) : boolean;
function PF_IS_DXT3(const pf: TDDPixelFormat) : boolean;
function PF_IS_DXT5(const pf: TDDPixelFormat) : boolean;
function PF_IS_BGRA8(const pf: TDDPixelFormat) : boolean;
function PF_IS_BGR8(const pf: TDDPixelFormat) : boolean;
function PF_IS_BGR5A1(const pf: TDDPixelFormat) : boolean;
function PF_IS_BGR565(const pf: TDDPixelFormat) : boolean;
function PF_IS_INDEX8(const pf: TDDPixelFormat) : boolean;

const
  // GL_EXT_texture_compression_s3tc
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT                   = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                  = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                  = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                  = $83F3;

implementation

function PF_IS_DXT1(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_FOURCC) <> 0) and (pf.dwFourCC = D3DFMT_DXT1);
end;

function PF_IS_DXT3(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_FOURCC) <> 0) and (pf.dwFourCC = D3DFMT_DXT3);
end;

function PF_IS_DXT5(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_FOURCC) <> 0) and (pf.dwFourCC = D3DFMT_DXT5);
end;

function PF_IS_BGRA8(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_RGB) <> 0) and
    ((pf.dwFlags and DDPF_ALPHAPIXELS) <> 0) and
    (pf.dwRGBBitCount = 32) and
    (pf.dwRBitMask = $ff0000) and
    (pf.dwGBitMask = $00ff00) and
    (pf.dwBBitMask = $0000ff) and
    (pf.dwAlphaBitMask = $ff000000);
end;

function PF_IS_BGR8(const pf: TDDPixelFormat) : boolean;
begin
//  ((pf.dwFlags & DDPF_ALPHAPIXELS) && \  ???
//  !(pf.dwFlags & DDPF_ALPHAPIXELS) && \
  Result := ((pf.dwFlags and DDPF_RGB) <> 0) and
    ((pf.dwFlags and DDPF_ALPHAPIXELS) = 0) and
    (pf.dwRGBBitCount = 32) and
    (pf.dwRBitMask = $ff0000) and
    (pf.dwGBitMask = $00ff00) and
    (pf.dwBBitMask = $0000ff) and
    (pf.dwAlphaBitMask = $ff000000);
end;

function PF_IS_BGR5A1(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_RGB) <> 0) and
    ((pf.dwFlags and DDPF_ALPHAPIXELS) <> 0) and
    (pf.dwRGBBitCount = 16) and
    (pf.dwRBitMask = $7c00) and
    (pf.dwGBitMask = $03e0) and
    (pf.dwBBitMask = $001f) and
    (pf.dwAlphaBitMask = $8000);
end;

function PF_IS_BGR565(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_RGB) <> 0) and
    ((pf.dwFlags and DDPF_ALPHAPIXELS) = 0) and
    (pf.dwRGBBitCount = 16) and
    (pf.dwRBitMask = $f800) and
    (pf.dwGBitMask = $07e0) and
    (pf.dwBBitMask = $001f) and
    (pf.dwAlphaBitMask = $ff000000);
end;

function PF_IS_INDEX8(const pf: TDDPixelFormat) : boolean;
begin
  Result := ((pf.dwFlags and DDPF_INDEXED) <> 0) and
    (pf.dwRGBBitCount = 8);
end;

{ TDDSImage }

function TDDSImage.CopyChunk(X, Y, H, W: Cardinal): TDDSImagePixelsChunk;
var
  dwLine:     Cardinal;
  dwLineSize: Cardinal;
  dwNewLineSize: Cardinal;
begin
  if (W <= 0) or (H <= 0) then
    raise Exception.Create('Height and Width must be greater than 0');
  if (X mod FdivSize <> 0) or (Y mod FdivSize <> 0) then
    raise Exception.Create('X and Y must be a multiple of ' + IntToStr(FdivSize));
  if (W mod FdivSize <> 0) or (H mod FdivSize <> 0) then
    raise Exception.Create('Height and Width must be a multiple of ' + IntToStr(FdivSize));

    { linesize is actually for a block of data width x divsize }
  dwLineSize := (Cardinal(FWidth) div FdivSize) * FblockBytes;
  dwNewLineSize := (Cardinal(W) div FdivSize) * FblockBytes;

  Result := TDDSImagePixelsChunk.Create;
  Result.FHeight := H;
  Result.FWidth := W;
  Result.FInternalFormat := FInternalFormat;

  dwLine := Y;
  while dwLine < (Y + H) do begin
    FPixelData.Seek(dwLineSize * (dwLine div FdivSize) +
      (X div FdivSize) * FblockBytes, soFromBeginning);
    Result.FPixelData.CopyFrom(FPixelData, dwNewLineSize);
    inc(dwLine, FdivSize);
  end;

  Result.FPixelData.Seek(0, soFromBeginning);
end;

constructor TDDSImage.Create;
begin
  inherited Create;
  FPixelData := TVCLMemoryStream.Create;
end;

destructor TDDSImage.Destroy;
begin
  FPixelData.Free;
  inherited Destroy;
end;

function TDDSImage.GetPixels: Pointer;
begin
  Result := FPixelData.Memory;
end;

function TDDSImage.GetPixelsSize: integer;
begin
  Result := FPixelData.Size;
end;

procedure TDDSImage.InitializeDXT1;
begin
  FCompressed := true;
  FSwap := false;
  FPalettized := false;
  FdivSize := 4;
  FblockBytes := 8;
  FInternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
  FexternalFormat := 0;
  FByteType := 0;
end;

procedure TDDSImage.InitializeDXT3;
begin
  FCompressed := true;
  FblockBytes := 16;
  FInternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;

  FSwap := false;
  FPalettized := false;
  FdivSize := 4;
  FexternalFormat := 0;
  FByteType := 0;
end;

procedure TDDSImage.InitializeDXT5;
begin
  FCompressed := true;
  FblockBytes := 16;
  FInternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;

  FSwap := false;
  FPalettized := false;
  FdivSize := 4;
  FexternalFormat := 0;
  FByteType := 0;
end;

procedure TDDSImage.LoadFromFile(const AFileName: string);
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

procedure TDDSImage.LoadFromStream(AStream: TStream);
var
  hdr:  TDDSFileHeader;
  iPixDataSize:  integer;
  function max(a, b: Cardinal) : Cardinal;
    begin if a > b then Result := a else Result := b; end;
begin
  FPixelData.Clear;
  if AStream.Size < sizeof(hdr) then
    exit;

  AStream.Read(hdr, sizeof(hdr));
  if hdr.dwMagic <> DDS_HEADER_MAGIC then
    raise Exception.Create('DDS header magic not found');
  if hdr.sSurfaceDesc.dwSize <> 124 then
    raise Exception.Create('DDS header size mismatch');
  if (hdr.sSurfaceDesc.dwFlags and DDSD_PIXELFORMAT) = 0 then
    raise Exception.Create('File does not contain DDSD_PIXELFORMAT');
  if (hdr.sSurfaceDesc.dwFlags and DDSD_CAPS) = 0 then
    raise Exception.Create('File does not contain DDSD_CAPS');

  FHeight := hdr.sSurfaceDesc.dwHeight;
  FWidth := hdr.sSurfaceDesc.dwWidth;
  FMipMapCount := hdr.sSurfaceDesc.dwMipMapCount;

  if PF_IS_DXT1(hdr.sSurfaceDesc.sPixelFormat) then
    InitializeDXT1
  else if PF_IS_DXT3(hdr.sSurfaceDesc.sPixelFormat) then
    InitializeDXT3
  else if PF_IS_DXT5(hdr.sSurfaceDesc.sPixelFormat) then
    InitializeDXT5
//  else if PF_IS_BGRA8(hdr.sPixelFormat) then
//    InitializeBGRA8
//  else if PF_IS_BGRA8(hdr.sPixelFormat) then
//    InitializeBGR8
//  else if PF_IS_BGR5A1(hdr.sPixelFormat) then
//    InitializeBGR5A1
//  else if PF_IS_BGR565(hdr.sPixelFormat) then
//    InitializeBGR565
//  else if PF_IS_INDEX8(hdr.sPixelFormat) then
//    InitializeIndex8
  else
    raise Exception.Create('Currently unsupported DDS pixel format');

  if FCompressed then begin
    if FdivSize = 0 then
      raise Exception.Create('divSize is 0 loading DDS.  Format must be DXT1');

    iPixDataSize := (max(FdivSize, Cardinal(FWidth)) div FdivSize) *
      (max(FdivSize, Cardinal(FHeight)) div FdivSize) * FblockBytes;
//    if FMipMapCount > 1 then
//      iPixDataSize := hdr.sSurfaceDesc.dwLinearSize * 2
//    else
//      iPixDataSize := hdr.sSurfaceDesc.dwLinearSize;
    FPixelData.Size := iPixDataSize;
    FPixelData.CopyFrom(AStream, iPixDataSize);
  end;  { if compressed }
end;

{ TDDSImagePixelsChunk }

constructor TDDSImagePixelsChunk.Create;
begin
  FPixelData := TVCLMemoryStream.Create;
end;

destructor TDDSImagePixelsChunk.Destroy;
begin
  FPixelData.Free;
  inherited;
end;

function TDDSImagePixelsChunk.GetPixels: Pointer;
begin
  Result := FPixelData.Memory;
end;

function TDDSImagePixelsChunk.GetPixelsSize: integer;
begin
  Result := FPixelData.Size;
end;

end.

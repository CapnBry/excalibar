unit TexFont;

(* Copyright (c) Mark J. Kilgard, 1997. *)

(* This program is freely distributable without licensing fees and is
   provided without guarantee or warrantee expressed or implied. This
   program is -not- in the public domain. *)

(* Delphi interpretation by Bryan Mayland, 2003 *)

interface

uses
  Classes, SysUtils, GL, GLU;

const
  TXF_FORMAT_BYTE	=	0;
  TXF_FORMAT_BITMAP	= 1;

type
  TXFException = class(Exception);

  PTexGlyphInfo = ^TexGlyphInfo;
  TexGlyphInfo = packed record
    c:      WORD;  { Potentially support 16-bit glyphs. }
    width:  BYTE;
    height: BYTE;
    xoffset:  shortint;
    yoffset:  shortint;
    advance:  shortint;
    dummy:  BYTE;  { Space holder for alignment reasons. }
    x:      smallint;
    y:      smallint;
  end;

  PTexGlyphVertexInfo = ^TexGlyphVertexInfo;
  TexGlyphVertexInfo = packed record
    t0: array[0..1] of GLfloat;
    v0: array[0..1] of GLshort;
    t1: array[0..1] of GLfloat;
    v1: array[0..1] of GLshort;
    t2: array[0..1] of GLfloat;
    v2: array[0..1] of GLshort;
    t3: array[0..1] of GLfloat;
    v3: array[0..1] of GLshort;
    advance:  GLfloat;
  end;

  TTexFont = class(TObject)
  private
    Ftexobj:  GLuint;
    Ftex_width:  integer;
    Ftex_height: integer;
    Fmax_ascent: integer;
    Fmax_descent:integer;
    Fnum_glyphs: integer;
    Fmin_glyph:  integer;
    Frange:    integer;
    Fteximage: PBYTEARRAY;
    Ftgi:      array of TexGlyphInfo;
    Ftgvi:     array of TexGlyphVertexInfo;
    Flut:      array of PTexGlyphVertexInfo;
  protected
    function getTCVI(c: integer) : PTexGlyphVertexInfo;
  public
    procedure LoadFont(const AFileName: string);
    procedure UnloadFont;
    procedure EstablishTexture;
    function EstablishTextureEx(texobj: GLuint; setupMipmaps: boolean) : GLuint;
    procedure CleanupTexture;
    procedure BindFontTexture;
    procedure GetStringMetrics(const s: string; var width, max_ascent, max_descent: integer);
    function RenderGlyph(c: integer) : integer;
    function RenderString(const s: string) : integer;
    procedure RenderStringXY(X, Y: integer; const s: string);
    procedure RenderStringXYBind(X, Y: integer; const s: string);
    procedure RenderFancyString(const s: string);
  end;


implementation

procedure SWAPL(var l: Cardinal); overload;
begin
  l :=
    (l shl 24) or
    ((l and $ff00) shl 8) or
    ((l and $ff0000) shr 8) or
    (l shr 24);
end;

procedure SWAPL(var l: integer); overload;
begin
  l :=
    (l shl 24) or
    ((l and $ff00) shl 8) or
    ((l and $ff0000) shr 8) or
    (l shr 24);
end;

procedure SWAPS(var l: WORD); overload;
begin
  l := (l shl 8) or (l shr 8);
end;

procedure SWAPS(var l: SmallInt); overload;
begin
  l := (l shl 8) or (l shr 8);
end;

{ TTexFont }

procedure TTexFont.BindFontTexture;
begin
  glBindTexture(GL_TEXTURE_2D, Ftexobj);
end;

procedure TTexFont.CleanupTexture;
begin
  glDeleteTextures(1, @Ftexobj);
  Ftexobj := 0;
end;

procedure TTexFont.EstablishTexture;
begin
  EstablishTextureEx(0, false);
end;

function TTexFont.EstablishTextureEx(texobj: GLuint; setupMipmaps: boolean) : GLuint;
begin
  if Ftexobj = 0 then
    if texobj = 0 then
      glGenTextures(1, @Ftexobj)
    else
      Ftexobj := texobj;

  glBindTexture(GL_TEXTURE_2D, Ftexobj);

  if setupMipmaps then begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_INTENSITY4, Ftex_width, Ftex_height,
      GL_LUMINANCE, GL_UNSIGNED_BYTE, Fteximage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST_MIPMAP_NEAREST);
  end
  else begin
    glTexImage2D(GL_TEXTURE_2D, 0, GL_INTENSITY4, Ftex_width, Ftex_height, 0,
      GL_LUMINANCE, GL_UNSIGNED_BYTE, Fteximage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  end;

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  Result := Ftexobj;
end;

procedure TTexFont.GetStringMetrics(const s: string; var width, max_ascent,
  max_descent: integer);
var
  tgvi:   PTexGlyphVertexInfo;
  I:      integer;
begin
  width := 0;
  for I := 1 to Length(s) do begin
    { NOTE:  ESC M,T,L,F not supported here for fancy string }
    tgvi := getTCVI(ord(s[I]));
    inc(width, round(tgvi^.advance));
  end;

  max_ascent := Fmax_ascent;
  max_descent := Fmax_descent;
end;

function TTexFont.getTCVI(c: integer): PTexGlyphVertexInfo;
begin
  Result := nil;

  { Automatically substitute uppercase letters with lowercase if not
     uppercase available (and vice versa). }
  if (c >= Fmin_glyph) and (c < Fmin_glyph + Frange) then begin
    Result := Flut[c - Fmin_glyph];
    if Assigned(Result) then
      exit;

      { BRY: is this right?  shouldn't this be outside the top if? }
    if (c >= 97) and (c <= 122) then begin  // n ['a'..'z']
      dec(c, 32);
      if (c >= Fmin_glyph) and (c < Fmin_glyph + Frange) then
        Result := Flut[c - Fmin_glyph];
    end
    else if (c >= 65) and (c <= 90) then begin //  in ['A'..'Z'] 
      inc(c, 32);
      if (c >= Fmin_glyph) and (c < Fmin_glyph + Frange) then
        Result := Flut[c - Fmin_glyph];
    end;
  end;

  if not Assigned(Result) then
    raise TXFException.CreateFmt(
      'texfont: tried to access unavailable font character "%c" (%d)', [char(c), ord(c)]);
end;

procedure TTexFont.LoadFont(const AFileName: string);
var
  FS:     TFileStream;
  fileid: integer;
  endianness: Cardinal;
  swap:   boolean;
  texformat:  integer;
  w, h:   GLfloat;
  xstep, ystep: GLfloat;
  I, J:   integer;
  tgi:    PTexGlyphInfo;
  max_glyph:  integer;
  stride:  integer;
  texbitmap:  PByteArray;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Fteximage := nil;

    FS.Read(fileid, sizeof(fileid));
    if fileid <> $667874ff then
      raise TXFException.Create('Not a texture font file.');

    FS.Read(endianness, sizeof(endianness));
    if endianness = $12345678 then
      swap := false
    else if endianness = $78563412 then
      swap := true
    else
      raise TXFException.Create('Not a texture font file.');

    FS.Read(texformat, sizeof(texformat));
    FS.Read(Ftex_width, sizeof(Ftex_width));
    FS.Read(Ftex_height, sizeof(Ftex_height));
    FS.Read(Fmax_ascent, sizeof(Fmax_ascent));
    FS.Read(Fmax_descent, sizeof(Fmax_descent));
    FS.Read(Fnum_glyphs, sizeof(Fnum_glyphs));

    if swap then begin
      SWAPL(texformat);
      SWAPL(Ftex_width);
      SWAPL(Ftex_height);
      SWAPL(Fmax_ascent);
      SWAPL(Fmax_descent);
      SWAPL(Fnum_glyphs);
    end;

    SetLength(Ftgi, Fnum_glyphs);
    FS.Read(Ftgi[0], Fnum_glyphs * sizeof(TexGlyphInfo));

    if swap then
      for I := 0 to Fnum_glyphs - 1 do begin
        SWAPS(Ftgi[i].c);
        SWAPS(Ftgi[i].x);
        SWAPS(Ftgi[i].y);
      end;

    SetLength(Ftgvi, Fnum_glyphs);
    w := Ftex_width;
    h := Ftex_height;
    xstep := 0.5 / w;
    ystep := 0.5 / h;
    for I := 0 to Fnum_glyphs - 1 do begin
      tgi := @Ftgi[i];
      with Ftgvi[i] do begin
        t0[0] := tgi^.x / w + xstep;
        t0[1] := tgi^.y / h + ystep;
        v0[0] := tgi^.xoffset;
        v0[1] := tgi^.yoffset;

        t1[0] := (tgi^.x + tgi^.width) / w + xstep;
        t1[1] := tgi^.y / h + ystep;
        v1[0] := tgi^.xoffset + tgi^.width + 1;
        v1[1] := tgi^.yoffset;

        t2[0] := (tgi^.x + tgi^.width) / w + xstep;
        t2[1] := (tgi^.y + tgi^.height) / h + ystep;
        v2[0] := tgi^.xoffset + tgi^.width + 1;
        v2[1] := tgi^.yoffset + tgi^.height + 1;

        t3[0] := tgi^.x / w + xstep;
        t3[1] := (tgi^.y + tgi^.height) / h + ystep;
        v3[0] := tgi^.xoffset;
        v3[1] := tgi^.yoffset + tgi^.height + 1;
        advance := tgi^.advance;
      end;  { with Ftvgi }
    end;  { for I to num_glyphs }

    Fmin_glyph := Ftgi[0].c;
    max_glyph := Ftgi[0].c;
    for I := 1 to Fnum_glyphs - 1 do
      if Ftgi[i].c < Fmin_glyph then
        Fmin_glyph := Ftgi[i].c
      else if Ftgi[i].c > max_glyph then
        max_glyph := Ftgi[i].c;
    Frange := max_glyph - Fmin_glyph + 1;

    SetLength(Flut, Frange);
    for I := 0 to Fnum_glyphs - 1 do
      Flut[Ftgi[i].c - Fmin_glyph] := @Ftgvi[i];

    case texformat of
      TXF_FORMAT_BYTE:
        begin
          GetMem(Fteximage, Ftex_width * Ftex_height);
          FS.Read(Fteximage^, Ftex_width * Ftex_height);
        end;  { FORMAT_BYTE }

      TXF_FORMAT_BITMAP:
        begin
          stride := (Ftex_width + 7) shr 3;
          GetMem(texbitmap, stride * Ftex_height);
          FS.Read(texbitmap^, stride * Ftex_height);

          GetMem(Fteximage, Ftex_width * Ftex_height);
          for I := 0 to Ftex_height - 1 do
            for J := 0 to Ftex_width - 1 do
              if (texbitmap[i * stride + (j shr 3)] and (1 shl (j and 7))) <> 0 then
                Fteximage[i * Ftex_width + j] := 255;
          FreeMem(texbitmap);
        end;  { FORMAT_BITMAP }

        else
          raise TXFException.Create('Unsupported TXF image format: ' + IntToStr(texformat));
      end;  { case texformat }
  finally
    FS.Free;
  end;
end;

procedure TTexFont.RenderFancyString(const s: string);
begin
  raise Exception.Create('RenderFancyString not implemented');
end;

function TTexFont.RenderGlyph(c: integer) : integer;
var
  tgvi: PTexGlyphVertexInfo;
begin
  tgvi := getTCVI(c);
  glBegin(GL_QUADS);
    glTexCoord2fv(@tgvi^.t0);
    glVertex2sv(@tgvi^.v0);
    glTexCoord2fv(@tgvi^.t1);
    glVertex2sv(@tgvi^.v1);
    glTexCoord2fv(@tgvi^.t2);
    glVertex2sv(@tgvi^.v2);
    glTexCoord2fv(@tgvi^.t3);
    glVertex2sv(@tgvi^.v3);
  glEnd();

  glTranslatef(tgvi^.advance, 0.0, 0.0);
  Result := round(tgvi^.advance);
end;

function TTexFont.RenderString(const s: string) : integer;
var
  I:    integer;
begin
  Result := 0;
  for I := 1 to Length(s) do
    inc(Result, RenderGlyph(ord(s[I])));
end;

procedure TTexFont.RenderStringXY(X, Y: integer; const s: string);
var
  iAdvance: integer;
begin
  glTranslatef(X, Y, 0);
  iAdvance := RenderString(s);
  glTranslatef(-(X + iAdvance), -Y, 0);
end;

procedure TTexFont.RenderStringXYBind(X, Y: integer; const s: string);
begin
  BindFontTexture;
  RenderStringXY(X, Y, s);
end;

procedure TTexFont.UnloadFont;
begin
  if Assigned(Fteximage) then begin
    FreeMem(Fteximage);
    Fteximage := nil;
  end;
  SetLength(Ftgi, 0);
  SetLength(Ftgvi, 0);
  SetLength(Flut, 0);
end;

end.

unit glWindow;

(***
  Adapted from glWindows.pas from:
    Author: Jason Allen
    Email: jra101@home.com
    website: delphigl.cfxweb.net

  Modified by Bryan Mayland for Kylix
***)

interface

uses
{$IFDEF LINUX}
  XLib, QGraphics, QControls, QForms, Qt, GLX, QDialogs,
{$ELSE}
  Windows, Graphics, Messages, Controls,
{$ENDIF !LINUX}
  SysUtils, Classes, GL, GLu;

type
  // Types for OpenGL window settings
  TWindowOption = (wfDrawToWindow, wfDrawToBitmap, wfSupportGDI,
                   wfSupportOpenGL, wfGenericAccelerated, wfDoubleBuffer);
  TWindowOptions = set of TWindowOption;

  TDepthBits = (c16bits, c32bits);

  TglWindow = class(TCustomControl)
  private
    hDC : THandle;                  // Device context
    hRC : THandle;                  // Rendering context
    FInitialized : Boolean;         // Whether panel has been initalized yet
    FColorDepth : Integer;          // Color depth (ignored in Linux)
    FDepthBits : Integer;           // Depth buffer depth (ignored in Linux)
    FDepthEnabled : Boolean;        // Enables the depth buffer (ignored in Linux)
    FWindowFlags : TWindowOptions;  // OpenGL window properties
    FOnResize : TNotifyEvent;       // OpenGL resize function
    FOnDraw : TNotifyEvent;         // OpenGL draw function
    FOnInit : TNotifyEvent;         // OpenGL initialization function
    flags : Word;                   // OpenGL window flags

    function GetColorDepth : TDepthBits;
    procedure SetColorDepth(depth : TDepthBits);
    function GetDepthBufferDepth : TDepthBits;
    procedure SetDepthBufferDepth(depth : TDepthBits);
    procedure defResize(Sender: TObject);
    procedure defDraw(Sender: TObject);
    procedure defInit(Sender: TObject);
    procedure Cleanup;

    procedure CreateRC;
    function MakeCurrent : boolean;
    procedure DestroyRC;
    procedure SwapBuffers;
{$IFDEF MSWINDOWS}
    procedure SetWindowFlags();
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
{$ENDIF}
  protected
{$IFDEF MSWINDOWS}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}
{$IFDEF LINUX}
    function WidgetFlags: integer; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Redraw;
    procedure Initialize;
  published
    property Align;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property ColorDepth : TDepthBits read GetColorDepth write SetColorDepth;
    property DepthBits : TDepthBits read GetDepthBufferDepth write SetDepthBufferDepth;
    property DepthBufferEnabled : Boolean read FDepthEnabled write FDepthEnabled;
    property WindowFlags : TWindowOptions read FWindowFlags write FWindowFlags;
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
    property OnDraw : TNotifyEvent read FOnDraw write FOnDraw;
    property OnInit : TNotifyEvent read FOnInit write FOnInit;
  end;

procedure Register;

implementation

procedure Register;
begin
  // Register glWindow component so its displayed in the Tool Palette
  RegisterComponents('OpenGL', [TglWindow]);
end;

{ TGLWindow }

// Name       : Create
// Purpose    : Creates the glWindow component
// Parameters : Owner of the component
constructor TGLWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  Color := clBlack;
  FInitialized := False;

  if csDesigning in ComponentState then begin
    // Set default width and height
    Width := 65;
    Height := 65;

    // Set up initial rendering settings
    // FDoubleBuffered := True;
    FColorDepth := 16;
    FDepthBits := 16;
    FDepthEnabled := True;
    FWindowFlags := [wfDrawToWindow, wfSupportOpenGL, wfDoubleBuffer];
  end;
end;

// Name       : defInit
// Purpose    : default OpenGL initialization
procedure TGLWindow.defInit(Sender: TObject);
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
end;

// Name       : defResize
// Purpose    : Default resize routine
procedure TGLWindow.defResize(Sender: TOBject);
begin
  // Set the viewport for the OpenGL window
  glViewport(0, 0, Width, Height);

  // Go to the projection matrix, this gets modified by the perspective
  // calulations
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  // Do the perspective calculations
  glOrtho(0.0, 10.0, 0.0, 10.0, 1, -1);

  // Return to the modelview matrix
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

// Name       : defDraw
// Purpose    : Default drawing routine, draws spinning cube
procedure TGLWindow.defDraw(Sender: TObject);
begin
  // Clear the color and depth buffers
  glClear(GL_COLOR_BUFFER_BIT);

  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);
    glVertex3f(0, 0, 0);
    glColor3f(0, 1, 0);
    glVertex3f(10, 0, 0);
    glColor3f(0, 0, 1);
    glVertex3f(5, 10, 0);
  glEnd();
end;

destructor TGLWindow.Destroy;
begin
  Cleanup;
  inherited Destroy;
end;

// Name       : GetColorDepth
// Purpose    : Gets the current OpenGL color depth
// Returns    : Current OpenGL color depth
function TGLWindow.GetColorDepth() : TDepthBits;
begin
  case FColorDepth of
    16 : Result := c16bits;
    32 : Result := c32bits;
    else Result := c16bits;
  end;
end;

// Name       : GetDepthBufferDepth
// Purpose    : Gets the current depth of the depth buffer
// Returns    : The current depth of the depth buffer
function TGLWindow.GetDepthBufferDepth() : TDepthBits;
begin
  case FDepthBits of
    16 : Result := c16bits;
    32 : Result := c32bits;
    else Result := c16bits;
  end;
end;

// Name       : Paint
// Purpose    : Paints the OpenGL component black when its first put on the form
procedure TGLWindow.Paint;
begin
  if csDesigning in ComponentState then
    Canvas.Rectangle(0, 0, Width, Height)
  else
    Redraw;
end;

// Name       : SetColorDepth
// Purpose    : Sets the window color depth
// Parameters :
//   depth - new color depth for the window
procedure TGLWindow.SetColorDepth(depth : TDepthBits);
begin
  case depth of
    c16bits : FColorDepth := 16;
    c32bits : FColorDepth := 32;
  end;
end;

// Name       : SetDepthBufferDepth
// Purpose    : Sets the depth of the depth buffer
// Parameters :
//   depth - new depth for the depth buffer
procedure TGLWindow.SetDepthBufferDepth(depth : TDepthBits);
begin
  case depth of
    c16bits : FDepthBits := 16;
    c32bits : FDepthBits := 32;
  end;
end;

// Name       : Resize
// Purpose    : Called when the width or height of the component changes, in
//              turn it calls the resize procedure assigned to the component
procedure TGLWindow.Resize;
begin
  if not FInitialized then
    exit;

  // Make this components rendering context current
  MakeCurrent;

  // Call assigned resize event if component has been initialized
  if Assigned(OnResize) then
    OnResize(Self)
  else
    defResize(Self);
end;

// Name       : ReDraw
// Purpose    : Called when owner want's opengl window to be updated, in turn
//              it calls the draw function assigned to the component
procedure TGLWindow.Redraw;
begin
  // Make this components rendering context current
  MakeCurrent;

  // Call assigned drawing routine, updating the OpenGL scene
  if Assigned(OnDraw) then
    OnDraw(Self)
  else
    defDraw(Self);

  // Swap the buffers if needed
  if wfDoubleBuffer in FWindowFlags then
    SwapBuffers;
end;

// Name       : Loaded
// Purpose    : Sets the pixel format and attaches a rendering context to the
//              components device context
{$IFDEF MSWINDOWS}
// Name       : SetWindowFlags
// Purpose    : Sets the OpenGL window flags depending on the values specified
//              in the property editor for the component
procedure TglWindow.SetWindowFlags;
begin
  if wfDrawToWindow in FWindowFlags then flags := flags or PFD_DRAW_TO_WINDOW;
  if wfDrawToBitmap in FWindowFlags then flags := flags or PFD_DRAW_TO_BITMAP;
  if wfSupportOpenGL in FWindowFlags then flags := flags or PFD_SUPPORT_OPENGL;
  if wfGenericAccelerated in FWindowFlags then flags := flags or PFD_GENERIC_ACCELERATED;

  // A window can't have both double buffering and gdi support in Microsoft's
  // OpenGL implementation. So support double buffering if requested and only
  // if it isn't do we support GDI operations
  if wfDoubleBuffer in FWindowFlags then
    flags := flags or PFD_DOUBLEBUFFER
  else if wfSupportGDI in FWindowFlags then
    flags := flags or PFD_SUPPORT_GDI;
end;

procedure TglWindow.CreateRC;
var
  PixelFormat : Integer;
  pfd : PIXELFORMATDESCRIPTOR;
begin
  // Update the window flags
  SetWindowFlags();

  // Set all fields in the pixelformatdescriptor to zero
  ZeroMemory(@pfd, SizeOf(pfd));

  // Initialize only the fields we need
  pfd.nSize       := SizeOf(PIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
  pfd.nVersion    := 1;                         // The version of this data structure
  pfd.dwFlags     := flags;                     // Set the window flags
                                                // (set in property editor)
  pfd.iPixelType  := PFD_TYPE_RGBA;             // Set OpenGL pixel data type
  pfd.cColorBits  := FColorDepth;               // OpenGL color depth
  pfd.cDepthBits  := FDepthBits;                // Specifies the depth of the depth buffer

  hDC := Canvas.Handle;

  // Attempts to find the pixel format supported by a device context that is the
  // best match to a given pixel format specification.
  PixelFormat := ChoosePixelFormat(hDC, @pfd);
  if (PixelFormat = 0) then begin
    Cleanup;
    MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Sets the specified device context's pixel format to the format specified by
  // the PixelFormat.
  if (not SetPixelFormat(hDC, PixelFormat, @pfd)) then begin
    Cleanup;
    MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Create a OpenGL rendering context
  hRC := wglCreateContext(hDC);
  if (hRC = 0) then begin
    Cleanup;
    MessageBox(0, 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Makes the specified OpenGL rendering context the calling thread's current
  // rendering context
  if not MakeCurrent then begin
    Cleanup;
    MessageBox(0, 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;
end;

function TglWindow.MakeCurrent : boolean;
begin
  Result := wglMakeCurrent(hDC, hRC);
end;

procedure TglWindow.DestroyRC;
begin
  if hRC = 0 then
    Exit;
  wglMakeCurrent(hDC, 0);
  wglMakeCurrent(0, 0);
  wglDeleteContext(hRC);
  hRC := 0;
  hDC := 0;
end;

procedure TglWindow.SwapBuffers;
begin
  Windows.SwapBuffers(hDC);
end;

procedure TglWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := CS_VREDRAW or CS_HREDRAW or CS_OWNDC or CS_DBLCLKS;
  end;
end;

procedure TglWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
procedure TglWindow.CreateRC;
var
  display: PDisplay;
  vinfo: PXVisualInfo;
  attr: array[0..23] of integer;
  idx: integer;
  procedure AddAttr(v: Cardinal);
  begin
    attr[idx] := v;
    inc(idx);
  end;
begin
  hDC := QWidget_winId(ChildHandle);

  display := Application.Display;
  FillChar(attr, sizeof(attr), 0);
  idx := 0;
  AddAttr(GLX_USE_GL);
  AddAttr(GLX_RGBA);
  if wfDoubleBuffer in FWindowFlags then
    AddAttr(GLX_DOUBLEBUFFER);

  AddAttr(GLX_RED_SIZE); AddAttr(1);
  AddAttr(GLX_GREEN_SIZE); AddAttr(1);
  AddAttr(GLX_BLUE_SIZE); AddAttr(1);
  AddAttr(GLX_DEPTH_SIZE); AddAttr(1);
//  AddAttr(GLX_ALPHA_SIZE); AddAttr(1);

//  if FDepthEnabled then begin
//    AddAttr(GLX_DEPTH_SIZE);
//    AddAttr(FDepthBits);
//  end;

  vinfo := glXChooseVisual(display, XDefaultScreen(display), @attr[0]);
  if not Assigned(vinfo) then begin
    Cleanup;
    MessageDlg('Error', 'Unable to find a suitable visual', mtError, [mbOk], 0);
    Exit;
  end;

  hRC := glXCreateContext(display, vinfo, 0, GL_TRUE);
  XFree(vinfo);
  if hRC = 0 then begin
    Cleanup;
    MessageDlg('Error', 'Unable to create an OpenGL rendering context', mtError, [mbOk], 0);
    Exit;
  end;

  if not MakeCurrent then begin
    Cleanup;
    MessageDlg('Error', 'Unable to activate OpenGL rendering context', mtError, [mbOk], 0);
    Exit;
  end;
end;

function TglWindow.MakeCurrent : boolean;
begin
  Result := glXMakeCurrent(Application.Display, hDC, hRC) <> GL_FALSE;
end;

procedure TglWindow.SwapBuffers;
begin
  glXSwapBuffers(Application.Display, hDC);
end;

procedure TglWindow.DestroyRC;
begin
  if hRC = 0 then
    exit;
  glXMakeCurrent(Application.Display, 0, 0);
  glXDestroyContext(Application.Display, hRC);
  hRC := 0;
  hDC := 0;
end;

function TglWindow.WidgetFlags: integer;
begin
  Result := inherited WidgetFlags;
  Result := Result or integer(WidgetFlags_WRepaintNoErase) or
    integer(WidgetFlags_WResizeNoErase);
end;
{$ENDIF}

procedure TglWindow.Cleanup;
begin
  // Makes current rendering context not current, and releases the device
  // Attempts to delete the rendering context
  DestroyRC;
  FInitialized := false;
end;

procedure TglWindow.Initialize;
begin
  if FInitialized then
    exit;

  CreateRC;

  // Component has a rendering context
  FInitialized := true;

  // Call assinged initialization routine and resize routine
  if Assigned(OnInit) then
    OnInit(Self)
  else
    defInit(Self);
end;

end.

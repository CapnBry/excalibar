unit glWindow;

interface

uses
  Windows, Classes, Graphics, Forms, OpenGL, extctrls;

type
  // Types for OpenGL window settings
  TWindowOption = (wfDrawToWindow, wfDrawToBitmap, wfSupportGDI,
                   wfSupportOpenGL, wfGenericAccelerated, wfDoubleBuffer);
  TWindowOptions = set of TWindowOption;

  TDepthBits = (c16bits, c32bits);

  TResizeEvent = procedure(Sender : TObject) of object;
  TDrawEvent = procedure(Sender : TObject) of object;
  TInitEvent = procedure(Sender : TObject) of object;

  TglWindow = class(TCustomPanel)
  private
    h_RC : HGLRC;                   // Rendering context
    Init : Boolean;                 // Whether panel has been initalized yet
    FColorDepth : Integer;          // Color depth
    FDepthBits : Integer;           // Depth buffer depth
    FDepthEnabled : Boolean;        // Enables the depth buffer
    FStencBits : Integer;           // Stencil buffer depth
    FStencEnabled : Boolean;        // Enables the stencil buffer
    FAccumBits : Integer;           // Accumulation buffer depth
    FAccumBufferEnabled : Boolean;  // Enables the accumulation buffer
    FWindowFlags : TWindowOptions;  // OpenGL window properties
    FOnResize : TResizeEvent;       // OpenGL resize function
    FOnDraw : TDrawEvent;           // OpenGL draw function
    FOnInit : TInitEvent;           // OpenGL initialization function
    OldClose : TNotifyEvent;        // Saves old close event from form
    flags : Word;                   // OpenGL window flags
    rot : Integer;                  // rotation amount for cube in default
                                    // drawing code

    procedure Close(Sender: TObject);
    function GetColorDepth() : TDepthBits;
    procedure SetColorDepth(depth : TDepthBits);
    function GetDepthBufferDepth() : TDepthBits;
    procedure SetDepthBufferDepth(depth : TDepthBits);
    function GetStencBufferDepth() : TDepthBits;
    procedure SetStencBufferDepth(depth : TDepthBits);
    function GetAccumBufferDepth() : TDepthBits;
    procedure SetAccumBufferDepth(depth : TDepthBits);
    procedure defResize(Sender : TOBject);
    procedure defDraw(Sender : TObject);
    procedure defInit(Sender : TObject);
    procedure SetWindowFlags();
  public
    h_DC : HDC;                     // Device context
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ReDraw();
    procedure Swap;               // Swaps the buffer's if display is double buffered 
    procedure Resize; override;
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
    property StencBits : TDepthBits read GetStencBufferDepth write SetStencBufferDepth;
    property StencBufferEnabled : Boolean read FStencEnabled write FStencEnabled;
    property AccumBits : TDepthBits read GetAccumBufferDepth write SetAccumBufferDepth;
    property AccumBufferEnabled : Boolean read FAccumBufferEnabled write FAccumBufferEnabled;
    property WindowFlags : TWindowOptions read FWindowFlags write FWindowFlags;
    property OnResize : TResizeEvent read FOnResize write FOnResize;
    property OnDraw : TDrawEvent read FOnDraw write FOnDraw;
    property OnInit : TInitEvent read FOnInit write FOnInit;
  end;

procedure Register;

implementation

procedure Register;
begin
  // Register glWindow component so its displayed in the Tool Palette 
  RegisterComponents('OpenGL', [TGLWindow]);
end;

{ TGLWindow }

// Name       : Create
// Purpose    : Creates the glWindow component
// Parameters : Owner of the component
constructor TGLWindow.Create(AOwner: TComponent);
var
  frm : TForm;
begin
  inherited Create(AOwner);

  // Set background color and pen and brush colors
  Color := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;

  if csDesigning in ComponentState then begin
    // Set default width and height
    Width := 65;
    Height := 65;

    // Set up initial rendering settings
    FDoubleBuffered := True;
    FColorDepth := 16;
    FDepthBits := 16;
    FDepthEnabled := True;
    FStencBits := 16;
    FStencEnabled := False;
    FAccumBits := 16;
    FAccumBufferEnabled := False;
    FWindowFlags := [wfDrawToWindow, wfSupportOpenGL, wfDoubleBuffer];
  end else begin

    // component hasn't been initialized yet (no rendering context, etc...)
    Init := False;

    // Get a handle on the form that the component is on
    frm := TForm(AOwner);

    // Set this component to be destroyed when the form is destroyed
    if Assigned(frm.OnDestroy) then
      OldClose := frm.OnDestroy;
    frm.OnDestroy := Close;

    // Set default OpenGL routines, if no draw/resize/or init routines are
    // specified then the default ones are used, the default drawing routine
    // simply draws a rotating cube
    OnResize := defResize;
    OnDraw := defDraw;
    OnInit := defInit;
  end;
end;

// Name       : defInit
// Purpose    : default OpenGL initialization
procedure TGLWindow.defInit(Sender: TObject);
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
end;

// Name       : defResize
// Purpose    : Default resize routine
procedure TGLWindow.defResize(Sender: TOBject);
var
  fAspect : GLfloat;
begin
  // Make sure that we don't get a divide by zero exception
  if (Height = 0) then
    Height := 1;

  // Set the viewport for the OpenGL window
  glViewport(0, 0, Width, Height);

  // Calculate the aspect ratio of the window
  fAspect := Width/Height;

  // Go to the projection matrix, this gets modified by the perspective
  // calulations
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  // Do the perspective calculations
  gluPerspective(45.0, fAspect, 1.0, 1000.0);

  // Return to the modelview matrix
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

// Name       : defDraw
// Purpose    : Default drawing routine, draws spinning cube
procedure TGLWindow.defDraw(Sender: TObject);
begin
  // Clear the color and depth buffers
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  // Replaces the current matrix with the identity matrix
  glLoadIdentity();

  // Go to the model view matrix mode
  glMatrixMode(GL_MODELVIEW);

  // Sample drawing code, draw's a rotating cube onscreen
  glTranslatef(0.0, 0.0, -5.0);
  glRotatef(rot, 1.0, 1.0, 0.0);  // Rotates around the x and y axis

	glBegin(GL_QUADS);
    glColor3f(0.0, 1.0, 0.0);
    glVertex3f( 1.0, 1.0,-1.0);
    glVertex3f(-1.0, 1.0,-1.0);
    glVertex3f(-1.0, 1.0, 1.0);
	  glVertex3f( 1.0, 1.0, 1.0);

    glColor3f(1.0, 0.5, 0.0);
		glVertex3f( 1.0,-1.0, 1.0);
		glVertex3f(-1.0,-1.0, 1.0);
		glVertex3f(-1.0,-1.0,-1.0);
		glVertex3f( 1.0,-1.0,-1.0);

    glColor3f(1.0, 0.0, 0.0);
	  glVertex3f( 1.0, 1.0, 1.0);
  	glVertex3f(-1.0, 1.0, 1.0);
		glVertex3f(-1.0,-1.0, 1.0);
	  glVertex3f( 1.0,-1.0, 1.0);

    glColor3f(1.0, 1.0, 0.0);
  	glVertex3f( 1.0,-1.0,-1.0);
		glVertex3f(-1.0,-1.0,-1.0);
	  glVertex3f(-1.0, 1.0,-1.0);
  	glVertex3f( 1.0, 1.0,-1.0);

    glColor3f(0.0, 0.0, 1.0);
	  glVertex3f(-1.0, 1.0, 1.0);
  	glVertex3f(-1.0, 1.0,-1.0);
		glVertex3f(-1.0,-1.0,-1.0);
	  glVertex3f(-1.0,-1.0, 1.0);

    glColor3f(1.0, 0.0, 1.0);
  	glVertex3f( 1.0, 1.0,-1.0);
		glVertex3f( 1.0, 1.0, 1.0);
	  glVertex3f( 1.0,-1.0, 1.0);
  	glVertex3f( 1.0,-1.0,-1.0);
  glEnd();

  Inc(rot); // Increment the current cube rotation amount
end;

destructor TGLWindow.Destroy;
begin
  inherited Destroy;
end;

// Name       : GetAccumBufferDepth
// Purpose    : Gets the current accumulation buffer depth
// Returns    : Current accumulation buffer depth
function TGLWindow.GetAccumBufferDepth() : TDepthBits;
begin
  case FAccumBits of
    16 : Result := c16bits;
    32 : Result := c32bits;
    else Result := c16bits;
  end;
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

// Name       : GetStencBufferDepth
// Purpose    : Gets the stencil buffer depth
// Returns    : Current stencil buffer depth
function TGLWindow.GetStencBufferDepth() : TDepthBits;
begin
  case FStencBits of
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
    ReDraw;
end;

// Name       : SetAccumBufferDepth
// Purpose    : Sets the accumulation buffer depth
// Parameters :
//   depth - new depth for accumulation buffer
procedure TGLWindow.SetAccumBufferDepth(depth : TDepthBits);
begin
  case depth of
    c16bits : FAccumBits := 16;
    c32bits : FAccumBits := 32;
  end;
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

// Name       : SetStencBufferDepth
// Purpose    : Sets the stencil buffer depth
// Parameters :
//   depth - New depth for the stencil buffer
procedure TGLWindow.SetStencBufferDepth(depth : TDepthBits);
begin
  case depth of
    c16bits : FStencBits := 16;
    c32bits : FStencBits := 32;
  end;
end;

// Name       : Resize
// Purpose    : Called when the width or height of the component changes, in
//              turn it calls the resize procedure assigned to the component
procedure TGLWindow.Resize;
begin
  // Make this components rendering context current
  wglMakeCurrent(h_DC, h_RC);

  // Call assigned resize event if component has been initialized
  if (Assigned(OnResize) and (Init))
    then OnResize(self);
end;

// Name       : ReDraw
// Purpose    : Called when owner want's opengl window to be updated, in turn
//              it calls the draw function assigned to the component
procedure TGLWindow.ReDraw;
begin
  // Make this components rendering context current
  wglMakeCurrent(h_DC, h_RC);

  // Call assigned drawing routine, updating the OpenGL scene
  if Assigned(OnDraw) then OnDraw(self);

  // Swap the buffers if needed
  Swap;
end;

// Name       : Swap
// Purpose    : Swaps the OpenGL buffers if display is double buffered
procedure TGLWindow.Swap;
begin
  if wfDoubleBuffer in FWindowFlags then SwapBuffers(h_DC);
end;

// Name       : Loaded
// Purpose    : Sets the pixel format and attaches a rendering context to the
//              components device context
procedure TGLWindow.Loaded;
var
  PixelFormat : Integer;
  pfd : PIXELFORMATDESCRIPTOR;
begin
  inherited;

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

  // If component's settings specifies that the accumulation buffer is enabled
  // then set its depth (which enables it)
  if FAccumBufferEnabled then
    pfd.cAccumBits := FAccumBits;

  // If component's settings specifies that the stencil buffer is enabled
  // then set its depth (which enables it)
  if FStencEnabled then
    pfd.cStencilBits := FStencBits;

  h_DC := GetDC(Handle);

  // Attempts to find the pixel format supported by a device context that is the
  // best match to a given pixel format specification.
  PixelFormat := ChoosePixelFormat(h_DC, @pfd);
  if (PixelFormat = 0) then begin
    Close(self);
    MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Sets the specified device context's pixel format to the format specified by
  // the PixelFormat.
  if (not SetPixelFormat(h_DC, PixelFormat, @pfd)) then begin
    Close(self);
    MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Create a OpenGL rendering context
  h_RC := wglCreateContext(h_DC);
  if (h_RC = 0) then begin
    Close(self);
    MessageBox(0, 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Makes the specified OpenGL rendering context the calling thread's current
  // rendering context
  if (not wglMakeCurrent(h_DC, h_RC)) then begin
    Close(self);
    MessageBox(0, 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  // Component has a rendering context
  Init := True;

  // Call assinged initialization routine and resize routine
  if Assigned(OnInit) then OnInit(self);
  if Assigned(OnResize) then OnResize(self);

  // Updates the OpenGL scene
  ReDraw;
end;

// Name       : Close
// Purpose    : Removes and deletes the components rendering context
procedure TglWindow.Close(Sender : TObject);
begin
  // Makes current rendering context not current, and releases the device
  // context that is used by the rendering context.
  wglMakeCurrent(h_DC, 0);

  // Attempts to delete the rendering context
  wglDeleteContext(h_RC);

  // Release the device context (return the memory)
  ReleaseDC(Handle, h_DC);

  Init := False;

  // If old close event was saved, call it
  if Assigned(OldClose) then OldClose(Sender);
end;


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

end.

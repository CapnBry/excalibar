/******************************************************************************
Cheyenne: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the Cheyenne Developers

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
******************************************************************************/

#include "glppi.h"
#include <stdexcept>
#include <sstream> // for the stringstream defs
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\CodeUtils.h" // for handy-dandy utility functions

extern logger_t Logger; // the logger
extern MapInfo Zones; // zone info
extern std::string InitialDir; // initial working directory

GLPPI::GLPPI() :
    ActorXScale(0.01f),ActorYScale(0.0149f),
    XLimit(2000000.0f),YLimit(2000000.0f)
{
    Wrapped=false;
    RenderContext=0;
    WrappedDC=0;
    WrappedWindow=0;
    CircleList=0;
    DiscList=0;

    ProjectionX=0.0f;
    ProjectionY=0.0f;
    ProjectionWidthX=10000.0f;
    ProjectionWidthY=10000.0f;
    ZoomIncrement=1.0f;
    PanIncrement=1.0f;
    
    LastBoundTexture=GLPPI::invalid_texture_id;

    // calcualte the initial increments
    // we do this before we set the actor
    // vertices so that the first set of
    // vertices is correct
    RecalcIncrements();

    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;
    
} // end GLPPI

GLPPI::~GLPPI()
{
    // make sure we are unwrapped
    if(Wrapped)
        {
        Unwrap();
        }
} // end ~GLPPI

void GLPPI::InitPixelFormat(void)
{
    // set pixel format
    // get the current pixel format index 
    // obtain a detailed description of that pixel format 
    PIXELFORMATDESCRIPTOR pfd;
    ZeroMemory(&pfd,sizeof(pfd));
    pfd.nSize=sizeof(pfd);
    pfd.nVersion=1;
    
    SetLastError(0);
    int iPF=GetPixelFormat(WrappedDC);
    if(iPF==0)
        {
        Logger << "[GLPPI::InitPixelFormat] GetPixelFormat() returned error " << GetLastError() << "\n";
        }

    DescribePixelFormat(WrappedDC,iPF,sizeof(PIXELFORMATDESCRIPTOR),&pfd);

    pfd.dwFlags |= PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_GENERIC_ACCELERATED|PFD_DOUBLEBUFFER;
    pfd.iPixelType=PFD_TYPE_RGBA; // use RGBA
    pfd.cColorBits=16; // 16 bit color
    pfd.cAlphaBits=8; // 8 bit alpha blending
    pfd.cDepthBits=16; // 16 bit z buffer
    pfd.iLayerType=PFD_MAIN_PLANE;

    iPF=ChoosePixelFormat(WrappedDC,&pfd);
    
    DescribePixelFormat(WrappedDC,iPF,sizeof(PIXELFORMATDESCRIPTOR),&pfd);
    
    Logger << "[GLPPI::InitPixelFormat] closest match pixel format:\n"
           << "\tpfd.dwFlags=" << pfd.dwFlags << "\n"
           << "\tpfd.iPixelType=" << (int)pfd.iPixelType << "\n"
           << "\tpfd.cColorBits=" << (int)pfd.cColorBits << "\n"
           << "\tpfd.cAlphaBits=" << (int)pfd.cAlphaBits << "\n"
           << "\tpfd.cDepthBits=" << (int)pfd.cDepthBits << "\n"
           << "\tpfd.iLayerType=" << (int)pfd.iLayerType << "\n";

    SetPixelFormat(WrappedDC,iPF,&pfd);

    Logger << "[GLPPI::InitPixelFormat] using pixel format:\n" 
           << (pfd.dwFlags&PFD_GENERIC_ACCELERATED ? "PFD_GENERIC_ACCELERATED" : "no PFD_GENERIC_ACCELERATED") << "\n"
           << (pfd.dwFlags&PFD_GENERIC_FORMAT ? "PFD_GENERIC_FORMAT" : "no PFD_GENERIC_FORMAT") << "\n";

    // done
    return;
}// end InitPixelFormat

void GLPPI::InitRenderContext(void)
{
    if(RenderContext!=0)
        {
        // remove glrc
        wglMakeCurrent(0,0);
        
        // free the render context
        wglDeleteContext(RenderContext);
        }
    
    // create context
    RenderContext=wglCreateContext(WrappedDC);
    
    // make current
    wglMakeCurrent(WrappedDC,RenderContext);
    
    // clear color is dark blue
    glClearColor(0.0f,0.0f,0.5f,1.0f);

    // clear all buffers
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_ACCUM_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

    // select buffer
    glDrawBuffer(GL_BACK);

    // cull
    glCullFace(GL_BACK);
    
    // this is WIERD because <0,0> is in the upper-left corner in DAoC
    glFrontFace(GL_CW);
    //glFrontFace(GL_CCW);
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
    glPolygonMode(GL_FRONT,GL_FILL);
    glShadeModel(GL_SMOOTH);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glDepthFunc(GL_ALWAYS);

    // make stored render state current
    RenderState.MakeAllCurrent();
    
    // set the way we want
    RenderState.Enable(GL_CULL_FACE);
    RenderState.Disable(GL_DEPTH_TEST);
    RenderState.Enable(GL_TEXTURE_2D);
    RenderState.Disable(GL_BLEND);
    
    // done
    return;
} // end InitRenderContext

void GLPPI::InitTextures(void)
{
    LOG_FUNC << "Initializing textures\n";

    std::ostringstream oss;

    // go through each zone and load a texture for it
    for(unsigned int zone=0;zone<256;++zone)
        {
        // empty it
        oss.str("");
        oss.seekp(0);
        oss.clear();

        // put filename in
        oss << "maps\\zone";
        oss.width(3);
        oss.fill('0');
        oss << zone << ".png";
        
        // make a full path out of it
        // and bind the texture
        unsigned int id=PngBindContainer(ZoneTextureMap,zone,AppendFileToPath(InitialDir,oss.str()));

        if(id != 0)
            {
            Logger << "[GLPPI::InitTextures] loaded texture \"" << oss.str().c_str() << "\""
                   << " with id " << id << "\n";
            }
        } // end for each zone

    // load the actor con textures
    PngBindContainer(TextureMap,GLPPI::alb_gray,AppendFileToPath(InitialDir,"skins\\alb_gray.png"));
    PngBindContainer(TextureMap,GLPPI::alb_green,AppendFileToPath(InitialDir,"skins\\alb_green.png"));
    PngBindContainer(TextureMap,GLPPI::alb_blue,AppendFileToPath(InitialDir,"skins\\alb_blue.png"));
    PngBindContainer(TextureMap,GLPPI::alb_yellow,AppendFileToPath(InitialDir,"skins\\alb_yellow.png"));
    PngBindContainer(TextureMap,GLPPI::alb_orange,AppendFileToPath(InitialDir,"skins\\alb_orange.png"));
    PngBindContainer(TextureMap,GLPPI::alb_red,AppendFileToPath(InitialDir,"skins\\alb_red.png"));
    PngBindContainer(TextureMap,GLPPI::alb_purple,AppendFileToPath(InitialDir,"skins\\alb_purple.png"));

    PngBindContainer(TextureMap,GLPPI::hib_gray,AppendFileToPath(InitialDir,"skins\\hib_gray.png"));
    PngBindContainer(TextureMap,GLPPI::hib_green,AppendFileToPath(InitialDir,"skins\\hib_green.png"));
    PngBindContainer(TextureMap,GLPPI::hib_blue,AppendFileToPath(InitialDir,"skins\\hib_blue.png"));
    PngBindContainer(TextureMap,GLPPI::hib_yellow,AppendFileToPath(InitialDir,"skins\\hib_yellow.png"));
    PngBindContainer(TextureMap,GLPPI::hib_orange,AppendFileToPath(InitialDir,"skins\\hib_orange.png"));
    PngBindContainer(TextureMap,GLPPI::hib_red,AppendFileToPath(InitialDir,"skins\\hib_red.png"));
    PngBindContainer(TextureMap,GLPPI::hib_purple,AppendFileToPath(InitialDir,"skins\\hib_purple.png"));

    PngBindContainer(TextureMap,GLPPI::mid_gray,AppendFileToPath(InitialDir,"skins\\mid_gray.png"));
    PngBindContainer(TextureMap,GLPPI::mid_green,AppendFileToPath(InitialDir,"skins\\mid_green.png"));
    PngBindContainer(TextureMap,GLPPI::mid_blue,AppendFileToPath(InitialDir,"skins\\mid_blue.png"));
    PngBindContainer(TextureMap,GLPPI::mid_yellow,AppendFileToPath(InitialDir,"skins\\mid_yellow.png"));
    PngBindContainer(TextureMap,GLPPI::mid_orange,AppendFileToPath(InitialDir,"skins\\mid_orange.png"));
    PngBindContainer(TextureMap,GLPPI::mid_red,AppendFileToPath(InitialDir,"skins\\mid_red.png"));
    PngBindContainer(TextureMap,GLPPI::mid_purple,AppendFileToPath(InitialDir,"skins\\mid_purple.png"));

    PngBindContainer(TextureMap,GLPPI::mob_gray,AppendFileToPath(InitialDir,"skins\\mob_gray.png"));
    PngBindContainer(TextureMap,GLPPI::mob_green,AppendFileToPath(InitialDir,"skins\\mob_green.png"));
    PngBindContainer(TextureMap,GLPPI::mob_blue,AppendFileToPath(InitialDir,"skins\\mob_blue.png"));
    PngBindContainer(TextureMap,GLPPI::mob_yellow,AppendFileToPath(InitialDir,"skins\\mob_yellow.png"));
    PngBindContainer(TextureMap,GLPPI::mob_orange,AppendFileToPath(InitialDir,"skins\\mob_orange.png"));
    PngBindContainer(TextureMap,GLPPI::mob_red,AppendFileToPath(InitialDir,"skins\\mob_red.png"));
    PngBindContainer(TextureMap,GLPPI::mob_purple,AppendFileToPath(InitialDir,"skins\\mob_purple.png"));

    PngBindContainer(TextureMap,GLPPI::generic_alb,AppendFileToPath(InitialDir,"skins\\generic_alb.png"));
    PngBindContainer(TextureMap,GLPPI::generic_hib,AppendFileToPath(InitialDir,"skins\\generic_hib.png"));
    PngBindContainer(TextureMap,GLPPI::generic_mid,AppendFileToPath(InitialDir,"skins\\generic_mid.png"));
    PngBindContainer(TextureMap,GLPPI::generic_mob,AppendFileToPath(InitialDir,"skins\\generic_mob.png"));
    
    // load the general textures
    PngBindContainer(TextureMap,GLPPI::ground_target,AppendFileToPath(InitialDir,"skins\\ground_target.png"));
    
    // done
    return;
} // end InitTextures

void GLPPI::DestroyTextures(void)
{
    LOG_FUNC << "Cleaning up textures\n";
    
    // cleanup all textures
    while(ZoneTextureMap.begin() != ZoneTextureMap.end())
        {
        ZoneTextureMapIteratorType it=ZoneTextureMap.begin();

        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        ZoneTextureMap.erase(it);
        }

    while(TextureMap.begin() != TextureMap.end())
        {
        TextureMapIteratorType it=TextureMap.begin();
        
        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        TextureMap.erase(it);
        }

    // done
    return;
} // end DestroyTextures

void GLPPI::InitDisplayLists(void)
{
    LOG_FUNC << "Initializing display lists\n";

    // make circle radius 100, 36 slices (every 10°)
    CircleList=glGenLists(1);
    const float slice=10.0f*0.017453292519943295769236907684886f; // 10° in radians
    
    glNewList(CircleList,GL_COMPILE);
    glColor4f(1.0f,1.0f,1.0f,1.0f);
    glBegin(GL_LINE_LOOP);
    for(int i=0;i<36;++i)
        {
        glVertex3d
            (
            100.0*cos(float(i)*slice),
            100.0*sin(float(i)*slice),
            0.0
            );
        }

    glEnd();
    glEndList();

    // make disc radius 100, 36 slices (every 10°)
    DiscList=glGenLists(1);
    glNewList(DiscList,GL_COMPILE);
    glBegin(GL_TRIANGLE_FAN);

    glVertex3d
        (
        0.0,
        0.0,
        0.0
        );

    // use <= to complete the disc
    for(int i=0;i<=36;++i)
        {
        glVertex3d
            (
            100.0*cos(float(i)*slice),
            100.0*sin(float(i)*slice),
            0.0
            );
        }

    glEnd();
    glEndList();
    // done
    return;
} // end InitDisplayLists

void GLPPI::DestroyDisplayLists(void)
{
    LOG_FUNC << "Cleaning up display lists\n";

    // destroy the circle list
    glDeleteLists(CircleList,1);
    
    // destroy the disc list
    glDeleteLists(DiscList,1);

    // done
    return;
} // end DestroyDisplayLists

void GLPPI::InitFonts(void)
{
    LOG_FUNC << "Loading fonts\n";

    // load our font and make it active
    TextEngine.LoadFont("CheyenneFont",AppendFileToPath(InitialDir,"fonts\\arial.fnt"));
    TextEngine.SetActiveFont("CheyenneFont");
    TextEngine.SetHeight(14);
    TextEngine.SetWidthScale(1.0f);
    
    // done
    return;
} // end InitFonts

void GLPPI::DestroyFonts(void)
{
    LOG_FUNC << "Unloading fonts\n";

    // cleanup
    TextEngine.Clear();
    
    // done
    return;
} // end DestroyFonts

void GLPPI::InitDisplayMatrices(void)const
{
    // change to projection matrix, and clear it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // make orthographic
    // this is WIERD because <0,0> is in the upper-left corner in DAoC
    glOrtho
        (
        ProjectionX, // left
        ProjectionX+ProjectionWidthX, // right
        ProjectionY+ProjectionWidthY, // top
        ProjectionY, // bottom
        1.0f, // near
        5.0f // far
        );
    
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // set initial eye point
    gluLookAt
        (
        0,                        // eyex
        0,                        // eyey
        3,                        // eyez (we make this a 
                                  // reasonable value to ease 
                                  // rounding error)
        0,                        // centerx
        0,                        // centery
        0.0,                      // centerz
        0.0,                      // upx
        1.0,                      // upy
        0.0                       // upz
        );

    return;
} // end InitDisplayMatrices

void GLPPI::Wrap(HWND Window)
{
    if(Wrapped)
        {
        throw(std::logic_error("attempt to wrap when already wrapped, call Unwrap() first!"));
        }
    
    // set flag
    Wrapped=true;
    
    // save window
    WrappedWindow=Window;
    
    // get client rect
    GetClientRect(WrappedWindow,&rClient);
    
    // get device context
    WrappedDC=GetDC(WrappedWindow);
    
    // init the pixel format
    InitPixelFormat();
    
    // init the render context
    InitRenderContext();
    
    // init display lists
    InitDisplayLists();
    
    // init textures
    InitTextures();
    
    // init fonts
    InitFonts();
    
    // init vector maps
    VmLoader.Go();
    
    // call resize to initialize the viewport
    Resize();
    
    // initialize the projection matrix
    InitDisplayMatrices();
    
    // done
    return;
} // end Wrap

void GLPPI::Unwrap(void)
{
    if(!Wrapped)
        {
        // done
        return;
        }
    
    // destroy texures
    DestroyTextures();
    
    // destroy display lists
    DestroyDisplayLists();
    
    // destroy fonts
    DestroyFonts();
        
    // clean up vector maps: first
    // make sure its not still loading
    // then cleanup
    VmLoader.Stop();
    VmLoader.MakeEmpty();

    // remove glrc
    wglMakeCurrent(0,0);
    
    // free the render context
    wglDeleteContext(RenderContext);
    
    // release DC
    ReleaseDC(WrappedWindow,WrappedDC);
    
    // clear flag
    Wrapped=false;
} // end Unwrap

void GLPPI::Resize(void)
{
    if(!Wrapped)
        {
        throw(std::logic_error("attempt to resize before Wrap(), call Wrap() first!"));
        }
    
    // recreate viewport
    GetClientRect(WrappedWindow,&rClient);
    glViewport(0,0,rClient.right-rClient.left,rClient.bottom-rClient.top);
    
    // regenerate viewport in the text renderer
    TextEngine.RegenerateViewport();
    
    // initialize the projection matrix
    InitDisplayMatrices();
} // end Resize

void GLPPI::RecalcIncrements(void)
{
    const float ProjWidth=0.5f*(ProjectionWidthX + ProjectionWidthY);

    float NewInc=0.10202040816326530612244897959184f*ProjWidth -101.0f;
    // y = mx + b =)

    // clamp at 1.0
    // and at 5000.0
    if(NewInc < 1.0f)
        {
        ZoomIncrement=1.0f;
        PanIncrement=1.0f;
        }
    else if(NewInc > 5000.0f)
        {
        ZoomIncrement=5000.0f;
        PanIncrement=5000.0f;
        }
    else
        {
        ZoomIncrement=NewInc;
        PanIncrement=NewInc;
        }
    
    // done
    return;
} // end RecalcIncrements

void GLPPI::ZoomIn(void)
{
    if(ProjectionWidthX-ZoomIncrement < 100.0f)
        {
        return;
        }
    
    if(ProjectionWidthY-ZoomIncrement < 100.0f)
        {
        return;
        }

    ProjectionWidthX-=ZoomIncrement;
    ProjectionWidthY-=ZoomIncrement;

    ProjectionX+=ZoomIncrement*0.5f;
    ProjectionY+=ZoomIncrement*0.5f;

    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;

    // init display matrices
    InitDisplayMatrices();

    // recalculate increments
    RecalcIncrements();

    // done
    return;
} // end ZoomIn

void GLPPI::ZoomOut(void)
{
    if(ProjectionWidthX > XLimit)
        {
        return;
        }
    
    if(ProjectionWidthY > YLimit)
        {
        return;
        }
    
    ProjectionWidthX+=ZoomIncrement;
    ProjectionWidthY+=ZoomIncrement;
    ProjectionX-=ZoomIncrement*0.5f;
    ProjectionY-=ZoomIncrement*0.5f;

    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;

    // init display matrices
    InitDisplayMatrices();

    // recalculate increments
    RecalcIncrements();

    // done
    return;
} // end ZoomOut

void GLPPI::PanRight(void)
{
    ProjectionX+=PanIncrement;

    if(ProjectionX > XLimit)
        {
        ProjectionX=XLimit;
        }
    
    InitDisplayMatrices();
} // end PanRight

void GLPPI::PanLeft(void)
{
    ProjectionX-=PanIncrement;

    if(ProjectionX < -XLimit)
        {
        ProjectionX=-XLimit;
        }
    
    InitDisplayMatrices();
} // end PanLeft

// UP AND DOWN ARE BACKWARDS INTENTIONALLY
// this is WIERD because <0,0> is in the upper-left corner in DAoC
void GLPPI::PanDown(void)
{
    ProjectionY+=PanIncrement;

    if(ProjectionY > YLimit)
        {
        ProjectionY=YLimit;
        }
    
    InitDisplayMatrices();
} // end PanDown

void GLPPI::PanUp(void)
{
    ProjectionY-=PanIncrement;

    if(ProjectionY < -YLimit)
        {
        ProjectionY=-YLimit;
        }
    
    InitDisplayMatrices();
} // end PanUp

bool GLPPI::GetScreenPosition
    (
    GLdouble ObjX,
    GLdouble ObjY,
    GLdouble ObjZ,
    GLdouble* ScreenX,
    GLdouble* ScreenY,
    GLdouble* ScreenZ
    ) const
{
    GLdouble ModelView[16];
    glGetDoublev(GL_MODELVIEW_MATRIX,&ModelView[0]);

    GLdouble Projection[16];
    glGetDoublev(GL_PROJECTION_MATRIX,&Projection[0]);

    GLint Viewport[4];
    glGetIntegerv(GL_VIEWPORT,&Viewport[0]);
    
    bool success=gluProject
        (
        ObjX,
        ObjY,
        ObjZ,
        ModelView,
        Projection,
        Viewport,
        ScreenX,
        ScreenY,
        ScreenZ
        )==GL_TRUE ? true:false;
        
    // adjust for daoc wierdness again
    GLdouble ht=(GLdouble)(rClient.bottom-rClient.top);
    *ScreenY=ht-*ScreenY;
    
    return(success);
} // end GetScreenPosition

void GLPPI::AdjustPositionByRegion(Motion& Position,const unsigned char Region)const
{
    // offset position by region offsets (loaded from regionoffsets.txt)
    Position.SetXPos(Position.GetXPos() + float(Zones.GetLimitsFromRegion(Region).XOffset));
    Position.SetYPos(Position.GetYPos() + float(Zones.GetLimitsFromRegion(Region).YOffset));
} // end AdjustPositionByRegion

void GLPPI::GetRenderPosition(const Actor& ThisActor,Motion& Position)const
{
    // store current position in parameter for Adjust function
    Position=ThisActor.GetMotion();
    // let adjust do the rest
    AdjustPositionByRegion(Position,ThisActor.GetRegion());
} // end GetRenderPosition

void GLPPI::RenderBegin(void)
{
    #ifdef _DEBUG
    if(!Wrapped)
        {
        throw(std::logic_error("attempt to begin rendering before Wrap(), call Wrap() first!"));
        }
    #endif

    // clear all buffers
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

    // switch to modelview matrix
    glMatrixMode(GL_MODELVIEW);
    
    // init modelview matrix to identity
    glLoadIdentity();

    // move back a bit
    glTranslatef(0.0f,0.0f,-3.0f);
    
    // make all states current
    RenderState.MakeAllCurrent();

    // done
    return;
} // end RenderBegin

void GLPPI::RenderActor(const Actor& ThisActor,const GLPPI::TextureId& TexId)
{
    // push matrix stack
    glPushMatrix();
    
    // get position to render this actor
    Motion RenderPosition;
    GetRenderPosition(ThisActor,RenderPosition);
    
    glTranslatef(RenderPosition.GetXPos(),RenderPosition.GetYPos(),0.0f);

    // bind the texture
    BindTexture(TexId);

    // rotate for heading (all heading in daoc are 180° off)
    glRotatef(180.0f+RenderPosition.GetHeading()*57.295779513082320876798154814105f,0.0f,0.0f,1.0f);
    //                                     convert to degrees for glRotatef

    // render differently if it is old data
    if(ThisActor.GetOld())
        {
        // draw with color coding instead of texture
        // for old actors (those that have not been 
        // updated for a while: the data is stale)
        
        // set color to a dark reddish purple color cause
        // its different from everything else ;)
        glColor4f(0.5f,0.0f,0.25f,1.0f);
        
        // bind to invalid texture
        BindTexture(GLPPI::invalid_texture_id);
        }

    // store current color
    GLfloat PreAlphaColors[4];
    glGetFloatv(GL_CURRENT_COLOR,&PreAlphaColors[0]);

    if(ThisActor.GetStealth())
        {
        // enable alpha blending
        RenderState.Enable(GL_BLEND);
        }
    
    // draw actor symbol
    glBegin(GL_TRIANGLES);

    glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleB());
    glTexCoord2f(0.5f,1.0f);
    glVertex3f(0.0f,ActorVertexY,0.0f);

    glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleA());
    glTexCoord2f(0.0f,0.0f);
    glVertex3f(-ActorVertexX,-ActorVertexY,0.0f);

    glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleC());
    glTexCoord2f(1.0f,0.0f);
    glVertex3f(ActorVertexX,-ActorVertexY,0.0f);

    // done with this actor
    glEnd();

    // disable alpha blending if necessary
    if(ThisActor.GetStealth())
        {
        // disable alpha blending
        RenderState.Disable(GL_BLEND);
        }
        
    // restore colors
    glColor4fv(&PreAlphaColors[0]);

    // pop matrix stack
    glPopMatrix();
    
    // done
    return;
} // end RenderActor

void GLPPI::RenderVectorMap(const VectorMap& Map)
{
    // done
    return;
} // end RenderVectorMap

void GLPPI::RenderZone
    (
    const MapInfo::ZoneInfo& zone,
    const int BaseX,
    const int BaseY,
    const int MaxX,
    const int MaxY
    )
{
    glPushMatrix();
    glTranslatef(float(BaseX),float(BaseY),0);
    
    // set texture
    BindTexture(unsigned char(zone.ZoneNum));

    // draw this zone
    glBegin(GL_QUADS);

    glColor4f(0.5f,0.5f,0.5f,1.0f);

    glTexCoord2i(0,1);
    glVertex3i(0,MaxY-BaseY,0);

    glTexCoord2i(0,0);
    glVertex3i(0,0,0);

    glTexCoord2i(1,0);
    glVertex3i(MaxX-BaseX,0,0);

    glTexCoord2i(1,1);
    glVertex3i(MaxX-BaseX,MaxY-BaseY,0);

    glEnd();
    
    // draw vector map -- use null texture
    BindTexture(GLPPI::invalid_texture_id);
    VmLoader.Draw(zone.ZoneNum);
    
    GLdouble x,y,z;
    GetScreenPosition(0,0,0,&x,&y,&z);
    
    // draw zone name
    TextEngine.SetColor(1.0f,1.0f,1.0f);
    TextEngine.StringOut(float(x),float(y),zone.ZoneFile);
    //TextEngine.StringOut(0.0f,0.0f,zone.ZoneFile);

    glPopMatrix();
    
    // done
    return;
} // end RenderZone

void GLPPI::RenderAllZones(void)
{
    MapInfo::ZoneIndexType ndx;
    
    for(ndx=0;ndx<::Zones.MaxZone;++ndx)
        {
        const MapInfo::ZoneInfo& zone=::Zones.GetZone(ndx);
        
        int BaseX,BaseY,MaxX,MaxY;
        GetBoundingRectangle(zone,BaseX,BaseY,MaxX,MaxY);
        
        // see if its visible
        if(IsVisible(BaseX,BaseY,MaxX,MaxY) && zone.bValid)
            {
            // render it
            RenderZone(zone,BaseX,BaseY,MaxX,MaxY);
            } // end if zone is vi-sible
        } // end for all zones
        
    // done
    return;
} // end RenderAllZones

void GLPPI::RenderEnd(void)
{
    #ifdef _DEBUG
    if(!Wrapped)
        {
        throw(std::logic_error("attempt to end rendering before Wrap(), call Wrap() first!"));
        }
    #endif

    // flush font rendering, the text is cached up to this point
    TextEngine.Draw();
    
    // lfontrenderer sets the texture environment and doesn't put it 
    // back -- evil!
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);

    // lfontrenderer enables depth testing and doesn't put it
    // back -- evil!
    RenderState.Disable(GL_DEPTH_TEST);

    // finish and swap
    glFlush();
    glFinish();
    SwapBuffers(WrappedDC);

    // done
    return;
} // end RenderEnd

bool GLPPI::IsVisible(int BaseX, int BaseY, int MaxX, int MaxY)const
{
    // check to see if overlapping with screen
    // we can use the Windows GDI call IntersectRect to
    // determine this
    
    RECT r={BaseX,BaseY,MaxX,MaxY};
    RECT rDisplay={(LONG)ProjectionX,(LONG)ProjectionY,(LONG)(ProjectionX+ProjectionWidthX),(LONG)(ProjectionY+ProjectionWidthY)};
    RECT rIntersect;

    return(IntersectRect(&rIntersect,&r,&rDisplay) ? true:false);
} // end IsVisible

void GLPPI::GetBoundingRectangle(const MapInfo::ZoneInfo& zone,int& BaseX,int& BaseY,int& MaxX,int& MaxY)const
{
    BaseX=int(zone.BaseX) + ::Zones.GetLimitsFromRegion(zone.Region).XOffset;
    BaseY=int(zone.BaseY) + ::Zones.GetLimitsFromRegion(zone.Region).YOffset;

    MaxX=int(zone.MaxX) + ::Zones.GetLimitsFromRegion(zone.Region).XOffset;
    MaxY=int(zone.MaxY) + ::Zones.GetLimitsFromRegion(zone.Region).YOffset;

    // done
    return;
} // end GetBoundingRectangle

bool GLPPI::IsZoneVisible(const MapInfo::ZoneInfo& zone)const
{
    // get display coordinates
    // for this zone
    int BaseX,BaseY;
    int MaxX,MaxY;

    // get bounding rectangle (object coordinates)
    GetBoundingRectangle(zone,BaseX,BaseY,MaxX,MaxY);

    // check to make sure zone is visible on screen
    return(IsVisible(BaseX,BaseY,MaxX,MaxY));
} // end IsZoneVisible

/* ----------------------------------------------------------------------------
Copyright (c) 2002, Lev Povalahev
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, 
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
      this list of conditions and the following disclaimer in the documentation 
      and/or other materials provided with the distribution.
    * The name of the author may not be used to endorse or promote products 
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
THE POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------*/

/******************************************************************************

2/1/2004, ces:  1. changed windows include to winsock2.h instead of windows.h
                2. added no min max for including windows headers this is 
                because for my application, I need access to min and
                max stl functions (as well as my own) but the windows header
                files default to making MACROS out of min/max so I have to 
                override that in every file that includes a windows header :(
                3. put in lfont namespace to avoid name collisions
                4. made DEFAULT_HEIGHT into a const uint instead of a macro
                5. added regenerate viewport function

******************************************************************************/


#pragma once
//--------------------------------------------------

#include <string>
#include <vector>

#ifdef _WIN32
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#endif // _WIN32

#include <GL/gl.h>
//--------------------------------------------------

namespace lfont
{
typedef unsigned int uint;
typedef unsigned char byte;

const uint DEFAULT_HEIGHT=99999;

struct LCharCoords
{
    GLfloat left;
    GLfloat top;
    GLfloat right;
    GLfloat bottom;
    bool enabled;
    float widthFactor;
};

struct LFont
{
    std::string name;
    GLuint textureId;
    uint defaultHeight;
    uint height;
    float widthScale;
    bool italic;
    GLfloat angle;
    GLfloat r, g, b;
    LCharCoords chars[256];
};

struct LStringData
{
    std::string str;
    uint font;  // the font to be used
    uint height; // the height of the font
    GLfloat r, g, b; // the color
    GLfloat angle; // the rotation angle
    GLfloat x, y;  // the position
    float widthScale; // width scale
    bool italic; // true if the font is italic
};

//--------------------------------------------------
class LFontRenderer
{
public:
    // default constructor, initializes the object
    LFontRenderer();
    // the destructor, automatically called by delete
    virtual ~LFontRenderer();
    // returns the number of fonts loaded
    uint GetFontCount();
    // returns the name of the given font, index _must_ be smaller than GetFontCount()
    const std::string& GetFontName(uint index);
    // sets the active font, this function is also available in a version that takes the index as parameter
    void SetActiveFont(const std::string& name);
    // sets the active font, this function is also available in a version that takes the name
    // of the font as parameter
    void SetActiveFont(uint index);
    // returns the index of the active font, throws an exception if no fonts are loaded
    uint GetActiveFont();
    // this function loads a font from a file and adds it to the list of fonts it returns the index of
    // the new font. The font name paramater is a name for the font that can be used with
    // SetActiveFont() etc.
    uint LoadFont(const std::string& fontname, const std::string& filename);
    // removes all fonts and clears any memory allocated
    void Clear();
    // sets the height of the current font
    void SetHeight(uint height);
    // sets the color of the active font
    void SetColor(GLfloat red, GLfloat green, GLfloat blue);
    // returns teh height of the active font
    uint GetHeight();
    // call this to get the color of the active font
    void GetColor(GLfloat &red, GLfloat &green, GLfloat &blue);
    // call thos method to output a string
    void StringOut(float x, float y, const std::string &string);
    // this method regenerates the stored viewport for when the 
    // drawing area changes
    void RegenerateViewport(void);
    // this method draws all the strings
    void Draw();
    // this methods sets the width scale of the active font
    void SetWidthScale(float scale);
    // this method returns the width scale of the active font
    float GetWidthScale();
    // this method sets the rotation angle for the active font
    void SetRotation(float angle);
    // this method returns the rotation angle for the active font
    float GetRotation();
    // this function is used to set either the active font should be italic or not
    void SetItalic(bool value);
    // this function returns true if the active font is italic
    bool GetItalic();
    // this function returns the width of the string. font metrics of the active font
    // are used
    uint GetStringWidth(const std::string &string);
protected:
    // used internally 
    uint m_strCount;
    // the index of the active font
    uint m_activeFont;
    // the array of fonts
    std::vector<LFont> m_fonts;
    // the strings to be rendered
    std::vector<LStringData> m_strings;
    // the size of the viewport
    uint m_viewTop;
    uint m_viewLeft;
    uint m_viewWidth;
    uint m_viewHeight;
#ifdef GL_VERSION_1_3    
    // used internally, the number of supported texture units;
    uint m_textureUnits;
#endif // GL_VERSION_1_3
};

} // end namespace lfont

//--------------------------------------------------

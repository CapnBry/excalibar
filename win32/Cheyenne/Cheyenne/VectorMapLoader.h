#pragma once

#include "global.h"
#include <gl\gl.h>
#include <gl\glu.h>
#include <gl\glut.h>
#include <map>

class VectorMapTriplet
{
    public:
    VectorMapTriplet(float x1=0.0f,float y1=0.0f,float z1=0.0f){x=x1;y=y1;z=z1;};
    VectorMapTriplet(const VectorMapTriplet& s){x=s.x;y=s.y;z=s.z;};
    ~VectorMapTriplet(){};

    VectorMapTriplet& operator=(const VectorMapTriplet& s)
    {
        if(this!=&s)
            {
            x=s.x;
            y=s.y;
            z=s.z;
            }
        return(*this);
    }

    float x,y,z;
}; 

class VectorMapColor
{
public:
    VectorMapColor(float r1=0.0f,float g1=0.0f,float b1=0.0f,float a1=1.0f){r=r1;g=g1;b=b1;a=a1;};
    VectorMapColor(const VectorMapColor& s){r=s.r;g=s.g;b=s.b;a=s.a;};
    ~VectorMapColor(){};

    VectorMapColor& operator=(const VectorMapColor& s)
    {
        if(this!=&s)
            {
            r=s.r;
            g=s.g;
            b=s.b;
            a=s.a;
            }
        return(*this);
    }

    float r,g,b,a;
};

class VectorMapItem
{
public:
    VectorMapItem():Type('M'),Color("black"),Name("none")
    {
        // add color map values
        BuildColorMap();

    };
    VectorMapItem(const VectorMapItem& s){set(s);};
    ~VectorMapItem(){};

    VectorMapItem& operator=(const VectorMapItem& s)
        {
        if(this!=&s)
            {
            set(s);
            }

        return(*this);
        }

    void AddTriplet(const VectorMapTriplet& s)
    {
        Triplets.insert(Triplets.end(),s);
    }

    void SetType(const char& t){Type=t;};
    char GetType(void)const{return(Type);};
    void SetName(const std::string& s){Name=s;};
    void SetColor(const std::string& s){Color=s;};

    void Draw(void)const
    {
        // draw

        switch(GetType())
            {
            case 'P':
                glBegin(GL_POINTS);
                break;

            default:
                glBegin(GL_LINE_STRIP);
                break;
            }
        
        // get color
        std::map<std::string,VectorMapColor>::const_iterator cit;
        cit=ColorMap.find(Color);

        if(cit==ColorMap.end())
            {
            // not defined, make black
            glColor4f(0.0f,0.0f,0.0f,1.0f);
            }
        else
            {
            // use predefined color
            glColor4f(cit->second.r,cit->second.g,cit->second.b,cit->second.a);
            }

        std::list<VectorMapTriplet>::const_iterator it;
        for(it=Triplets.begin();it!=Triplets.end();++it)
            {
            glVertex3f(it->x,it->y,1.0f);//it->z); render it flat for now

            /*
            if(GetType() == 'P')
                {
                // draw name too
                glRasterPos3f(it->x+10.0f,it->y,1.0f);
                glColor4f(1.0f,1.0f,1.0f,1.0f);
                DrawGLFontString(Name);

                }
            */
            }

        if(GetType() == 'P')
            {
            // end current drawing operation
            glEnd();

            // draw name too
            it=Triplets.begin();
            glRasterPos3f(it->x+10.0f,it->y,1.0f);
            glColor4f(1.0f,1.0f,1.0f,1.0f);
            DrawGLFontString(Name);
            }
        else
            {
            // end here
            glEnd();
            }
    }

protected:
private:
    void BuildColorMap(void)
    {
        // add color map values
        ColorMap.insert(std::make_pair(std::string("black"),VectorMapColor(0.0f,0.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("white"),VectorMapColor(1.0f,1.0f,1.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("gray"),VectorMapColor(0.5f,0.5f,0.5f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("green"),VectorMapColor(0.0f,1.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("red"),VectorMapColor(1.0f,0.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("blue"),VectorMapColor(0.0f,0.0f,1.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("orange"),VectorMapColor(250.0f/255.0f,226.0f/255.0f,133.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("pink"),VectorMapColor(255.0f/255.0f,226.0f/255.0f,219.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("magenta"),VectorMapColor(230.0f/255.0f,113.0f/255.0f,242.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("brown"),VectorMapColor(193.0f/255.0f,183.0f/255.0f,162.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("yellow"),VectorMapColor(248.0f/255.0f,240.0f/255.0f,105.0f/255.0f,1.0f)));
    }

    void set(const VectorMapItem& s)
    {
        Type=s.Type;
        Name=s.Name;
        Color=s.Color;
        Triplets=s.Triplets;
        ColorMap=s.ColorMap;
    }

    char Type;
    std::string Color;
    std::string Name;
    std::map<std::string,VectorMapColor> ColorMap;

    std::list<VectorMapTriplet> Triplets;
}; // end VectorMapItem

class VectorMap
{
public:
    VectorMap(){ListNum=0;bUsed=false;};
    VectorMap(const VectorMap& s){set(s);};
    explicit VectorMap(const std::string& name):Name(name){};
    ~VectorMap(){};

    VectorMap& operator=(const VectorMap& s)
    {
        if(this != &s)
            {
            set(s);
            }

        return(*this);
    }

    void MakeEmpty(void)
    {
        Name="";
        Vectors.erase(Vectors.begin(),Vectors.end());
    }

    void SetName(const std::string& s)
    {
        Name=s;
    }

    void AddItem(const VectorMapItem& s)
    {
        bUsed=true;
        Vectors.insert(Vectors.end(),s);
    }

    void SetListNum(unsigned int num)
    {
        ListNum=num;
    }

    void Draw(void)const
    {
        // if we are not used, do nothing
        if(!IsUsed())
            {
            return;
            }

        // draw all items
        std::list<VectorMapItem>::const_iterator it;

        for(it=Vectors.begin();it!=Vectors.end();++it)
            {
            it->Draw();
            }
    }

    void MakeDisplayList(void)const
    {
        // if we are not used, do nothing
        if(!IsUsed())
            {
            return;
            }

        // make a new display list
        glNewList(ListNum,GL_COMPILE);

        // draw
        Draw();

        // done
        glEndList();
    }

    bool IsUsed(void)const{return(bUsed);};

protected:
private:
    void set(const VectorMap& s)
    {
        Name=s.Name;
        Vectors=s.Vectors;
        ListNum=s.ListNum;
        bUsed=s.bUsed;
    }
    
    std::list<VectorMapItem> Vectors;
    std::string Name;
    unsigned int ListNum;
    bool bUsed;
}; // end class VectorMap

class VectorMapLoader : public Thread
{
public:
    VectorMapLoader();
    virtual ~VectorMapLoader();

    bool IsDone(void)const{return(bIsDone);};
    void SetListBase(unsigned int base){ListBase=base;};

    bool MakeDisplayLists(void);
    void Draw(const unsigned char MapNumber)const;

protected:
private:
    VectorMapLoader(const VectorMapLoader& s); // disallow
    VectorMapLoader& operator=(const VectorMapLoader& s); // disallow


    virtual DWORD Run(const bool& bContinue);


    VectorMap Maps[256];
    bool bIsDone;
    unsigned int ListBase;
};

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
#pragma once

#include <map>
#include <list>
#include <gl\gl.h>
#include <gl\glu.h>
#include "..\Utils\threads.h"

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

    void Draw(void)const;

    void MakeEmpty(void)
    {
        Name="none";
        Color="black";
        Type='M';
        Triplets.erase(Triplets.begin(),Triplets.end());
    }
protected:
private:
    void BuildColorMap(void);
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
    void Draw(const unsigned int MapNumber)const;
    
    void MakeEmpty(void)
    {
        for(int i=0;i<256;++i)
            {
            Maps[i].MakeEmpty();
            }
    }

protected:
private:
    VectorMapLoader(const VectorMapLoader& s); // disallow
    VectorMapLoader& operator=(const VectorMapLoader& s); // disallow


    virtual DWORD Run(const bool& bContinue);

    bool LoadMap(const std::string& filename,const unsigned char map_cnt);
    
    VectorMap Maps[256];
    bool bIsDone;
    unsigned int ListBase;
};

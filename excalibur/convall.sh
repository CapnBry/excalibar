#! /bin/bash

ZONES="000 001 002 003 006 007 008 009 010 011 012 014 015 100 101 102 103 104 105 106 107 108 111 112 113 115 200 201 202 203 204 205 206 207 208 210 211 212 214";

if [ ! -f convert.jar ]; then
  echo "Missing convert.jar"
  exit
fi

if [ ! -d $1/zones ]; then
  echo "The first argument MUST be the base folder of the Camlot Client"
  exit
fi

if [ ! -d maps ]; then
  echo "There's no maps directory here. Is this the EX directory?"
  exit
fi

if [ "$2" == "" ]; then
  echo "Please specify division factor. 1 means full size images, 2 means half size images, 3 means quartersize images and so on"
  exit
fi

if [ "$3" == "" ]; then
  echo "Please specify light factor. 1.0 means full intensity, while 0.0  is pure black"
  exit
fi

if [ $2 -lt 0 ]; then
  echo "Division factor must be 1 or more"
  exit
fi

if ! tgatoppm $1/zones/textures/lava.tga | pnmtopng > $1/zones/textures/lava.png; then
  echo "Failed to convert lava image. Do you have the netpbm tools installed?"
  exit
fi

if ! tgatoppm $1/zones/textures/watery.tga | pnmtopng > $1/zones/textures/watery.png; then
  echo "Failed to convert watery image. Do you have the netpbm tools installed?"
  exit
fi

for h in $ZONES; do
  DIR=$(echo $1/zones/*$h)
  WDIR="$1/zones/zone$h"
  if [ "$DIR" == "$1/zones/*$h" ]; then
    echo "Directory for zone $h, which was expected in $DIR, isn't there"
    echo "It's $DIR"
    exit
  fi
  if [ "$DIR" != "$WDIR" ]; then
    echo "Renaming directory $DIR to $WDIR"
    mv $DIR $WDIR
  fi
done

java -Xmx256000000 -jar convert.jar $1 $2 $3 $ZONES

for h in $ZONES; do
  mv zone$h.png maps/
done

  

unit DAOCConSystem;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

type
  TDAOCConColor = (ccGray, ccGreen, ccBlue, ccYellow, ccOrange, ccRed, ccPurple);
  TDAOCConColors = set of TDAOCConColor;

  TDAOCConRangeDefinition = packed record
    GrayMax:    Shortint;
    GreenMax:   Shortint;
    BlueMax:    Shortint;
    YellowMax:  Shortint;
    OrangeMax:  Shortint;
    RedMax:     Shortint;
  end;

function GetConColor(AViewerLevel, ATargetLevel: integer) : TDAOCConColor;
function IntToConColors(AVal: integer) : TDAOCConColors;
function ConColorsToInt(AVal: TDAOCConColors) : integer;

implementation

const
  CON_RANGES: array[0..50] of TDAOCConRangeDefinition = (
  (GrayMax:  0; GreenMax:  0; BlueMax:  0; YellowMax:  0; OrangeMax:  0; RedMax:  0),  { Level 0 }
  (GrayMax: -1; GreenMax: -1; BlueMax:  0; YellowMax:  1; OrangeMax:  2; RedMax:  3),  { Level 1 }
  (GrayMax: -1; GreenMax:  0; BlueMax:  1; YellowMax:  2; OrangeMax:  3; RedMax:  4),  { Level 2 }
  (GrayMax:  0; GreenMax:  1; BlueMax:  2; YellowMax:  3; OrangeMax:  4; RedMax:  5),  { Level 3 }
  (GrayMax:  1; GreenMax:  2; BlueMax:  3; YellowMax:  4; OrangeMax:  5; RedMax:  6),  { Level 4 }
  (GrayMax:  2; GreenMax:  3; BlueMax:  4; YellowMax:  5; OrangeMax:  6; RedMax:  7),  { Level 5 }
  (GrayMax:  3; GreenMax:  4; BlueMax:  5; YellowMax:  6; OrangeMax:  7; RedMax:  8),  { Level 6 }
  (GrayMax:  4; GreenMax:  5; BlueMax:  6; YellowMax:  7; OrangeMax:  8; RedMax:  9),  { Level 7 }
  (GrayMax:  5; GreenMax:  6; BlueMax:  7; YellowMax:  8; OrangeMax:  9; RedMax: 10),  { Level 8 }
  (GrayMax:  6; GreenMax:  7; BlueMax:  8; YellowMax:  9; OrangeMax: 10; RedMax: 11),  { Level 9 }
  (GrayMax:  6; GreenMax:  7; BlueMax:  9; YellowMax: 10; OrangeMax: 11; RedMax: 13),  { Level 10 }
  (GrayMax:  6; GreenMax:  7; BlueMax:  9; YellowMax: 11; OrangeMax: 13; RedMax: 15),  { Level 11 }
  (GrayMax:  6; GreenMax:  8; BlueMax: 10; YellowMax: 12; OrangeMax: 14; RedMax: 16),  { Level 12 }
  (GrayMax:  7; GreenMax:  9; BlueMax: 11; YellowMax: 13; OrangeMax: 15; RedMax: 17),  { Level 13 }
  (GrayMax:  8; GreenMax: 10; BlueMax: 12; YellowMax: 14; OrangeMax: 16; RedMax: 18),  { Level 14 }
  (GrayMax:  9; GreenMax: 11; BlueMax: 13; YellowMax: 15; OrangeMax: 17; RedMax: 19),  { Level 15 }
  (GrayMax: 10; GreenMax: 12; BlueMax: 14; YellowMax: 16; OrangeMax: 18; RedMax: 20),  { Level 16 }
  (GrayMax: 11; GreenMax: 13; BlueMax: 15; YellowMax: 17; OrangeMax: 19; RedMax: 21),  { Level 17 }
  (GrayMax: 12; GreenMax: 14; BlueMax: 16; YellowMax: 18; OrangeMax: 20; RedMax: 22),  { Level 18 }
  (GrayMax: 13; GreenMax: 15; BlueMax: 17; YellowMax: 19; OrangeMax: 21; RedMax: 23),  { Level 19 }
  (GrayMax: 13; GreenMax: 15; BlueMax: 18; YellowMax: 20; OrangeMax: 22; RedMax: 25),  { Level 20 }
  (GrayMax: 13; GreenMax: 15; BlueMax: 18; YellowMax: 21; OrangeMax: 24; RedMax: 27),  { Level 21 }
  (GrayMax: 13; GreenMax: 16; BlueMax: 19; YellowMax: 22; OrangeMax: 25; RedMax: 28),  { Level 22 }
  (GrayMax: 14; GreenMax: 17; BlueMax: 20; YellowMax: 23; OrangeMax: 26; RedMax: 29),  { Level 23 }
  (GrayMax: 15; GreenMax: 18; BlueMax: 21; YellowMax: 24; OrangeMax: 27; RedMax: 30),  { Level 24 }
  (GrayMax: 16; GreenMax: 19; BlueMax: 22; YellowMax: 25; OrangeMax: 28; RedMax: 31),  { Level 25 }
  (GrayMax: 17; GreenMax: 20; BlueMax: 23; YellowMax: 26; OrangeMax: 29; RedMax: 32),  { Level 26 }
  (GrayMax: 18; GreenMax: 21; BlueMax: 24; YellowMax: 27; OrangeMax: 30; RedMax: 33),  { Level 27 }
  (GrayMax: 19; GreenMax: 22; BlueMax: 25; YellowMax: 28; OrangeMax: 31; RedMax: 34),  { Level 28 }
  (GrayMax: 20; GreenMax: 23; BlueMax: 26; YellowMax: 29; OrangeMax: 32; RedMax: 35),  { Level 29 }
  (GrayMax: 21; GreenMax: 24; BlueMax: 27; YellowMax: 30; OrangeMax: 33; RedMax: 36),  { Level 30 }
  (GrayMax: 22; GreenMax: 25; BlueMax: 28; YellowMax: 31; OrangeMax: 34; RedMax: 37),  { Level 31 }
  (GrayMax: 23; GreenMax: 26; BlueMax: 29; YellowMax: 32; OrangeMax: 35; RedMax: 38),  { Level 32 }
  (GrayMax: 24; GreenMax: 27; BlueMax: 30; YellowMax: 33; OrangeMax: 36; RedMax: 39),  { Level 33 }
  (GrayMax: 25; GreenMax: 28; BlueMax: 31; YellowMax: 34; OrangeMax: 37; RedMax: 40),  { Level 34 }
  (GrayMax: 25; GreenMax: 28; BlueMax: 31; YellowMax: 35; OrangeMax: 39; RedMax: 42),  { Level 35 }
  (GrayMax: 25; GreenMax: 28; BlueMax: 31; YellowMax: 36; OrangeMax: 41; RedMax: 45),  { Level 36 }
  (GrayMax: 25; GreenMax: 29; BlueMax: 32; YellowMax: 37; OrangeMax: 42; RedMax: 47),  { Level 37 }
  (GrayMax: 25; GreenMax: 29; BlueMax: 33; YellowMax: 38; OrangeMax: 43; RedMax: 48),  { Level 38 }
  (GrayMax: 25; GreenMax: 29; BlueMax: 34; YellowMax: 39; OrangeMax: 44; RedMax: 49),  { Level 39 }
  (GrayMax: 25; GreenMax: 30; BlueMax: 35; YellowMax: 40; OrangeMax: 45; RedMax: 50),  { Level 40 }
  (GrayMax: 26; GreenMax: 31; BlueMax: 36; YellowMax: 41; OrangeMax: 46; RedMax: 51),  { Level 41 }
  (GrayMax: 27; GreenMax: 32; BlueMax: 37; YellowMax: 42; OrangeMax: 47; RedMax: 52),  { Level 42 }
  (GrayMax: 28; GreenMax: 33; BlueMax: 38; YellowMax: 43; OrangeMax: 48; RedMax: 53),  { Level 43 }
  (GrayMax: 29; GreenMax: 34; BlueMax: 39; YellowMax: 44; OrangeMax: 49; RedMax: 54),  { Level 44 }
  (GrayMax: 30; GreenMax: 35; BlueMax: 40; YellowMax: 45; OrangeMax: 50; RedMax: 55),  { Level 45 }
  (GrayMax: 31; GreenMax: 36; BlueMax: 41; YellowMax: 46; OrangeMax: 51; RedMax: 56),  { Level 46 }
  (GrayMax: 32; GreenMax: 37; BlueMax: 42; YellowMax: 47; OrangeMax: 52; RedMax: 57),  { Level 47 }
  (GrayMax: 33; GreenMax: 38; BlueMax: 43; YellowMax: 48; OrangeMax: 53; RedMax: 58),  { Level 48 }
  (GrayMax: 34; GreenMax: 39; BlueMax: 44; YellowMax: 49; OrangeMax: 54; RedMax: 59),  { Level 49 }
  (GrayMax: 35; GreenMax: 40; BlueMax: 45; YellowMax: 50; OrangeMax: 55; RedMax: 60)   { Level 50 }
);

function GetConColor(AViewerLevel, ATargetLevel: integer) : TDAOCConColor;
begin
  if (AViewerLevel < low(CON_RANGES)) or (AViewerLevel > high(CON_RANGES)) then
    Result := ccGray

  else begin
    with CON_RANGES[AViewerLevel] do
      if ATargetLevel <= GrayMax then
        Result := ccGray
      else if ATargetLevel <= GreenMax then
        Result := ccGreen
      else if ATargetLevel <= BlueMax then
        Result := ccBlue
      else if ATargetLevel <= YellowMax then
        Result := ccYellow
      else if ATargetLevel <= OrangeMax then
        Result := ccOrange
      else if ATargetLevel <= RedMax then
        Result := ccRed
      else
        Result := ccPurple;
  end;
end;

function IntToConColors(AVal: integer) : TDAOCConColors;
var
  I:    TDAOCConColor;
begin
  Result := [];
  for I := low(TDAOCConColor) to high(TDAOCConColor) do
    if (AVal and (1 shl ord(I))) <> 0 then
      Include(Result, I);
end;

function ConColorsToInt(AVal: TDAOCConColors) : integer;
var
  I:    TDAOCConColor;
begin
  Result := 0;
  for I := low(TDAOCConColor) to high(TDAOCConColor) do
    if I in AVal then
      Result := Result or (1 shl ord(I));
end;

end.

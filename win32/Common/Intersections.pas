unit Intersections;

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

uses
  Types;
  
function PointInRect(const ARect: TRect; AX, AY: integer) : boolean;
function RectsIntersect(const A, B: TRect) : boolean;

implementation

function PointInRect(const ARect: TRect; AX, AY: integer) : boolean;
{ point in rect assumes a top down rectangle (that the Top is less than the Bottom }
begin
  Result := (AX >= ARect.Left) and (AX <= ARect.Right) and
    (AY >= ARect.Top) and (AY <= ARect.Bottom);
end;

function RectsIntersect(const A, B: TRect) : boolean;
begin
    { a rect intersects if any one of its points is inside the other
      dude's rect.  We need to check both against each other because
      one might completely contain the other, in which case only
      one of the two checks will be true }
  Result := PointInRect(A, B.Left, B.Top) or
    PointInRect(A, B.Right, B.Top) or
    PointInRect(A, B.Left, B.Bottom) or
    PointInRect(A, B.Right, B.Bottom)
      or
    PointInRect(B, A.Left, A.Top) or
    PointInRect(B, A.Right, A.Top) or
    PointInRect(B, A.Left, A.Bottom) or
    PointInRect(B, A.Right, A.Bottom);
end;

end.

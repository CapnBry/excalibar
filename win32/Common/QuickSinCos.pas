unit QuickSinCos;

interface

{ NOTE: angle is in degrees! }
function cos_quick(a: integer) : single;
function sin_quick(a: integer) : single;
procedure sincos_quick(a: integer; var s, c: single);

implementation

const
  TRIG_TABLE_SIZE = 128;
  TRIG_TABLE_LERP: single = (TRIG_TABLE_SIZE / 180.0);

var
  COS_TABLE: array[0..TRIG_TABLE_SIZE] of single;
  SIN_TABLE: array[0..TRIG_TABLE_SIZE] of single;

procedure BuildSinCosTables;
var
  I:    integer;
begin
  for I := 0 to TRIG_TABLE_SIZE do begin
    COS_TABLE[I] := cos(PI * I / TRIG_TABLE_SIZE);
    SIN_TABLE[I] := sin(PI * I / TRIG_TABLE_SIZE);
  end;
end;

function cos_quick(a: integer) : single;
var
  iQuantized:   integer;
begin
  iQuantized := round(a * TRIG_TABLE_LERP);

  if iQuantized < 0 then
    iQuantized := -iQuantized;

  if iQuantized > TRIG_TABLE_SIZE then begin
    iQuantized := iQuantized mod (2 * TRIG_TABLE_SIZE);
    if iQuantized > TRIG_TABLE_SIZE then
      iQuantized := (2 * TRIG_TABLE_SIZE) - iQuantized;
  end;

  Result := COS_TABLE[iQuantized];
end;

function sin_quick(a: integer) : single;
var
  iQuantized:   integer;
  bNegative:    boolean;
begin
  iQuantized := round(a * TRIG_TABLE_LERP);

  if iQuantized < 0 then begin
    iQuantized := -iQuantized;
    bNegative := true;
  end
  else
    bNegative := false;

  if iQuantized > TRIG_TABLE_SIZE then begin
    iQuantized := iQuantized mod (2 * TRIG_TABLE_SIZE);
    if iQuantized > TRIG_TABLE_SIZE then begin
      iQuantized := (2 * TRIG_TABLE_SIZE) - iQuantized;
      bNegative := not bNegative;
    end;
  end;

  if bNegative then
    Result := -SIN_TABLE[iQuantized]
  else
    Result := SIN_TABLE[iQuantized];
end;

procedure sincos_quick(a: integer; var s, c: single);
var
  iQuantized:   integer;
  bNegativeS:   boolean;
begin
  iQuantized := round(a * TRIG_TABLE_LERP);

  if iQuantized < 0 then begin
    iQuantized := -iQuantized;
    bNegativeS := true;
  end
  else
    bNegativeS := false;

  if iQuantized > TRIG_TABLE_SIZE then begin
    iQuantized := iQuantized mod (2 * TRIG_TABLE_SIZE);
    if iQuantized > TRIG_TABLE_SIZE then begin
      iQuantized := (2 * TRIG_TABLE_SIZE) - iQuantized;
      bNegativeS := not bNegativeS;
    end;
  end;

  c := COS_TABLE[iQuantized];
  if bNegativeS then
    s := -SIN_TABLE[iQuantized]
  else
    s := SIN_TABLE[iQuantized];
end;

initialization
  BuildSinCosTables;
  
end.


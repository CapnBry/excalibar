unit DAOCObjs;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Graphics,
  DAOCRegion, DAOCInventory, DAOCPlayerAttributes, DAOCConSystem,
  DAOCClasses, QuickSinCos;

type
  TDAOCObjectClass = (ocUnknown, ocObject, ocMob, ocPlayer, ocLocalPlayer);
  TDAOCObjectClasses = set of TDAOCObjectClass;

  TDAOCObject = class(TObject)
  private
    procedure SetX(const Value: DWORD);
    procedure SetY(const Value: DWORD);
    procedure SetZ(const Value: DWORD);
    procedure SetDestinationX(const Value: DWORD);
    procedure SetDestinationY(const Value: DWORD);
    procedure SetDestinationZ(const Value: WORD);
    procedure SetLevel(const Value: integer);
    procedure SetRealm(const Value: TDAOCRealm);
    procedure SetHeadWord(const Value: WORD);
    function GetHead: integer;
    procedure SetStealthed(const Value: boolean);
  protected
    FInfoID:WORD;
    FPlayerID: WORD;
    FX:     DWORD;
    FY:     DWORD;
    FZ:     DWORD;
    FDestinationX:  DWORD;
    FDestinationY:  DWORD;
    FDestinationZ:  WORD;
    FName:  string;
    FLastUpdate:  DWORD;
    FLevel: integer;
    FRealm: TDAOCRealm;
    FHeadWord:    WORD;
    FStealthed:   boolean;
    FStale:       boolean;
    FIsInUpdateRange: boolean;

    function HeadRad: double;
    procedure SetName(const Value: string); virtual;
    function GetObjectClass : TDAOCObjectClass; virtual;
    function GetName : string; virtual;
    function TicksSinceUpdate : DWORD;
  public
    constructor Create; virtual;

    procedure Assign(ASrc: TDAOCObject);
    function AsString: string;
    procedure CheckStale; virtual;
    procedure Clear; virtual;
    function Distance2D(AObject: TDAOCObject) : double; overload;
    function Distance2D(X, Y: DWORD) : double; overload;
    function Distance3D(AObject: TDAOCObject) : double; overload;
    function Distance3D(X, Y, Z: DWORD) : double; overload;
    function DistanceSqr2D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr2D(X, Y: DWORD) : double; overload;
    function DistanceSqr3D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr3D(X, Y, Z: DWORD) : double; overload;
    function GetConColor(AToLevel: integer) : TColor;
    procedure LoadFromReader(AReader: TReader); virtual;
    procedure MarkStaleAtDestination; virtual;
    function SameLoc(AObject: TDAOCObject) : boolean;
    procedure SaveToWriter(AWriter: TWriter); virtual;
    procedure Touch;

    property InfoID: WORD read FInfoID write FInfoID;
    property PlayerID: WORD read FPlayerID write FPlayerID;
    property LastUpdate: DWORD read FLastUpdate;
    property X: DWORD read FX write SetX;
    property Y: DWORD read FY write SetY;
    property Z: DWORD read FZ write SetZ;
    property DestinationX: DWORD read FDestinationX write SetDestinationX;
    property DestinationY: DWORD read FDestinationY write SetDestinationY;
    property DestinationZ: WORD read FDestinationZ write SetDestinationZ;
    property Head: integer read GetHead;
    property HeadWord: WORD read FHeadWord write SetHeadWord;
    property Level: integer read FLevel write SetLevel;
    property Realm: TDAOCRealm read FRealm write SetRealm;
    property Stale: boolean read FStale;
    property Stealthed: boolean read FStealthed write SetStealthed;
    property ObjectClass: TDAOCObjectClass read GetObjectClass;
    property Name: string read GetName write SetName;
    property IsInUpdateRange: boolean read FIsInUpdateRange write FIsInUpdateRange;
  end;

  TDAOCObjectNotify = procedure (ASender: TObject; ADAOCObject: TDAOCObject) of Object;

  TDAOCObjectList = class(TObjectList)
  private
    function GetItems(I: integer): TDAOCObject;
  public
    procedure AddOrReplace(AObject: TDAOCObject);
    function IndexOfInfoID(AInfoID: integer) : integer;
    function FindByInfoID(AInfoID: integer) : TDAOCObject;
    function FindByPlayerID(APlayerID: integer) : TDAOCObject;
    function FindNearest3D(X, Y, Z: DWORD) : TDAOCObject;
    function FindNearest2D(X, Y: DWORD) : TDAOCObject;

    property Items[I: integer]: TDAOCObject read GetItems; default;
  end;

  TDAOCMovingObject = class(TDAOCObject)
  private
    function GetSpeed: integer;
    procedure SetSpeedWord(const Value: WORD);
    function GetIsSwimming: boolean;
    function GetXProjected: DWORD;
    function GetYProjected: DWORD;
    function GetIsDead: boolean;
    function GetIsAlive: boolean;
    procedure SetHitPoints(const Value: BYTE);
    function GetSpeedString: string;
  protected
    FHitPoints:  BYTE;
    FSpeedWord:  WORD;
    FProjectedX:  DWORD;
    FProjectedY:  DWORD;
    FProjectedLastUpdate: DWORD;
    FInventory: TDAOCInventory;

    procedure UpdateLastProjected;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(ASrc: TDAOCMovingObject);
    procedure Clear; override;
    procedure CheckStale; override;
    function DestinationAhead : boolean;
    procedure InventoryChanged; virtual;
    procedure MarkStaleAtDestination; override;

    property XProjected: DWORD read GetXProjected;
    property YProjected: DWORD read GetYProjected;
    property Speed: integer read GetSpeed;
    property SpeedWord: WORD read FSpeedWord write SetSpeedWord;
    property SpeedString: string read GetSpeedString;
    property IsAlive: boolean read GetIsAlive;
    property IsDead: boolean read GetIsDead;
    property IsSwimming: boolean read GetIsSwimming;
    property HitPoints: BYTE read FHitPoints write SetHitPoints;
    property Inventory: TDAOCInventory read FInventory;
  end;

  TDAOCUnknownMovingObject = class(TDAOCMovingObject)
  protected
    function GetName : string; override;
    function GetObjectClass : TDAOCObjectClass; override;
  end;

  TDAOCMob = class(TDAOCMovingObject)
  private
  protected
    FTypeTag: string;
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCMob);

    property TypeTag: string read FTypeTag write FTypeTag;
  end;

  TDAOCPlayer = class(TDAOCMovingObject)
  protected
    FGuild: string;
    FLastName:  string;
    FFullName:  string;
    FCharacterClass:  TDAOCCharacterClass;
    procedure UpdateFullName;
    procedure SetName(const Value: string); override;
    procedure SetLastName(const Value: string);
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCPlayer);
    procedure Clear; override;

    procedure CheckStale; override;
    procedure InventoryChanged; override;

    property Guild: string read FGuild write FGuild;
    property LastName: string read FLastName write SetLastName;
    property FullName: string read FFullName;
    property CharacterClass: TDAOCCharacterClass read FCharacterClass;
  end;

  TDAOCCurrency = class(TObject)
  private
    FPlatinum: integer;
    FSilver: integer;
    FMithril: integer;
    FGold: integer;
    FCopper: integer;
  public
    procedure Clear;
    function AsText : string;

    property Copper: integer read FCopper write FCopper;
    property Silver: integer read FSilver write FSilver;
    property Gold: integer read FGold write FGold;
    property Platinum: integer read FPlatinum write FPlatinum;
    property Mithril: integer read FMithril write FMithril;
  end;

  TDAOCLocalPlayer = class(TDAOCMovingObject)
  private
  protected
    FSkills:    TDAOCNameValueList;
    FSpecializations: TDAOCNameValueList;
    FAbilities: TDAOCNameValueList;
    FSpells:    TDAOCNameValueList;
    FStyles:    TDAOCNameValueList;
    FCurrency:  TDAOCCurrency;
    function GetObjectClass : TDAOCObjectClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    property Abilities: TDAOCNameValueList read FAbilities;
    property Currency: TDAOCCurrency read FCurrency;
    property Skills: TDAOCNameValueList read FSkills;
    property Specializations: TDAOCNameValueList read FSpecializations;
    property Spells: TDAOCNameValueList read FSpells;
    property Styles: TDAOCNameValueList read FStyles;
  end;

function DAOCObjectClassToStr(AClass: TDAOCObjectClass) : string;
function CopperToStr(ACopper: integer) : string;
function DWORDDelta(A, B: DWORD) : DWORD;

implementation

{$IFDEF GLOBAL_TICK_COUNTER}
uses
  GlobalTickCounter;
{$ENDIF GLOBAL_TICK_COUNTER}

const
  SPEED_1X = 191;
  
function DWORDDelta(A, B: DWORD) : DWORD;
begin
  if A > B then
    Result := A - B
  else
    Result := B - A;
end;

function DAOCObjectClassToStr(AClass: TDAOCObjectClass) : string;
begin
  case AClass of
    ocUnknown:    Result := 'Unknown';
    ocObject:     Result := 'Object';
    ocMob:        Result := 'MOB';
    ocPlayer:     Result := 'Player';
    ocLocalPlayer:  Result := 'Local Player';
    else
      Result := 'Unknown' + IntToStr(ord(AClass));
  end;  { case AClass }
end;

function CopperToStr(ACopper: integer) : string;
  procedure PrependStr(const s: string);
  begin
    if Result <> '' then
      Result := s + ' ' + Result
    else
      Result := s;
  end;
begin
  Result := '';
  if (ACopper mod 100) <> 0 then
    PrependStr(IntToStr(ACopper mod 100) + 'c');
  ACopper := ACopper div 100;

  if (ACopper mod 100) <> 0 then
    PrependStr(IntToStr(ACopper mod 100) + 's');
  ACopper := ACopper div 100;

  if (ACopper mod 1000) <> 0 then
    PrependStr(IntToStr(ACopper mod 1000) + 'g');
  ACopper := ACopper div 1000;

  if (ACopper mod 1000) <> 0 then
    PrependStr(IntToStr(ACopper mod 1000) + 'p');
  ACopper := ACopper div 1000;

  if (ACopper mod 1000) <> 0 then
    PrependStr(IntToStr(ACopper mod 1000) + 'm');
end;

function LocalTickCount : DWORD;
begin
{$IFDEF GLOBAL_TICK_COUNTER}
  Result := GlobalTickCount;
{$ELSE}
  Result := GetTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}
end;

{ TDAOCMovingObject }

procedure TDAOCMovingObject.Assign(ASrc: TDAOCMovingObject);
begin
  inherited Assign(ASrc);

  FSpeedWord := ASrc.SpeedWord;
end;

procedure TDAOCMovingObject.CheckStale;
begin
  if FStale then
    exit;

  if not DestinationAhead then
    MarkStaleAtDestination;
end;

procedure TDAOCMovingObject.Clear;
begin
  inherited;
  FSpeedWord := 0;
end;

constructor TDAOCMovingObject.Create;
begin
    { create objects before inherited because inherited calls clear }
  FInventory := TDAOCInventory.Create;
  inherited;
  FHitPoints := 100;
end;

function TDAOCMovingObject.DestinationAhead: boolean;
  { return true if the destination is in the direction of travel,
    or if the destination is unset }
type
  TQuadrant = (q1, q2, q3, q4);
  TQuadrants = set of TQuadrant;
var
  AcceptableQuads:  TQuadrants;
  vx:   integer;
  vy:   integer;
  HeadQuad:   TQuadrant;
begin
  if (FDestinationX = 0) or (FDestinationY = 0) or (Speed = 0) then
    Result := true
  else begin
    AcceptableQuads := [q1, q2, q3, q4];
      { compute vector to destination }
    vx := FDestinationX - XProjected;
    vy := FDestinationY - YProjected;

    if vx > 0 then
      AcceptableQuads := AcceptableQuads - [q2, q3]
    else if vx < 0 then
      AcceptableQuads := AcceptableQuads - [q1, q4];
    if vy < 0 then
      AcceptableQuads := AcceptableQuads - [q3, q4]
    else if vy > 0 then
      AcceptableQuads := AcceptableQuads - [q1, q2];

      { head should be between 0..359 }
    case (Head div 90) mod 4 of
      0:  HeadQuad := q1;
      1:  HeadQuad := q4;
      2:  HeadQuad := q3;
      3:  HeadQuad := q2;
      else
        HeadQuad := q1;
    end;
    Result := HeadQuad in AcceptableQuads;
  end;
end;

destructor TDAOCMovingObject.Destroy;
begin
  FInventory.Free;
  inherited;
end;

function TDAOCMovingObject.GetIsAlive: boolean;
begin
  REsult := FHitPoints <> 0;
end;

function TDAOCMovingObject.GetIsDead: boolean;
begin
  Result := FHitPoints = 0;
end;

function TDAOCMovingObject.GetIsSwimming: boolean;
begin
    { Bit 10 = swimming }
  Result := (FSpeedWord and (1 shl 10)) <> 0;
end;

function TDAOCMovingObject.GetSpeed: integer;
begin
    { bit  9: sign }
    { bit 10: swimming? }
  Result := FSpeedWord and $01ff;
  if (Result and $0200) <> 0 then
    Result := -Result;
end;

function TDAOCMovingObject.GetSpeedString: string;
var
  iSpeed:   integer;
begin
  iSpeed := Speed;
  iSpeed := (iSpeed * 100) div SPEED_1X;
  Result := IntToStr(iSpeed) + '%';
end;

function TDAOCMovingObject.GetXProjected: DWORD;
begin
  UpdateLastProjected;
  Result := FProjectedX;
end;

function TDAOCMovingObject.GetYProjected: DWORD;
begin
  UpdateLastProjected;
  Result := FProjectedY;
end;

procedure TDAOCMovingObject.InventoryChanged;
begin
end;

procedure TDAOCMovingObject.MarkStaleAtDestination;
begin
  inherited;
  FSpeedWord := 0;
end;

procedure TDAOCMovingObject.SetHitPoints(const Value: BYTE);
begin
  FHitPoints := Value and $7f;  // bit $80 means *something*
end;

procedure TDAOCMovingObject.SetSpeedWord(const Value: WORD);
begin
  FSpeedWord := Value;
  Touch;
end;

procedure TDAOCMovingObject.UpdateLastProjected;
var
  iSpeed:   integer;
  dHyp:     double;
  s, c:     single;
begin
  if FProjectedLastUpdate = LocalTickCount then
    exit;

  FProjectedLastUpdate := LocalTickCount;

  iSpeed := Speed;
  if iSpeed = 0 then begin
    FProjectedX := FX;
    FProjectedY := FY;
  end
  else begin
    dHyp := iSpeed * integer(TicksSinceUpdate) * (1 / 1000);
    sincos_quick(GetHead, s, c);
    FProjectedX := FX + round(s * dHyp);
    FProjectedY := FY - round(c * dHyp);
  end;
end;

{ TDAOCObject }

procedure TDAOCObject.Assign(ASrc: TDAOCObject);
begin
  FInfoID := ASrc.InfoID;
  FPlayerID := ASrc.PlayerID;
  FX := ASrc.X;
  FY := ASrc.Y;
  FZ := ASrc.Z;
  FDestinationX := ASrc.DestinationX;
  FDestinationY := ASrc.DestinationY;
  FLastUpdate := ASrc.FLastUpdate;
  FName := ASrc.Name;
  FRealm := ASrc.Realm;
  FLevel := ASrc.Level;
  FHeadWord := ASrc.HeadWord;
end;

function TDAOCObject.AsString: string;
begin
  Result := Format('Level %d %s %s (Info%4.4x,Player%4.4x) (%d,%d,%d) head %d',
    [FLevel, RealmToStr(FRealm), FName, FInfoID, FPlayerID, FX, FY, FZ, GetHead]);
  if (FDestinationX <> 0) and (FDestinationY <> 0) then
    Result := Result + Format(#13'  Headed to: (%d,%d)', [FDestinationX, FDestinationY]); 
end;

procedure TDAOCObject.Clear;
begin
  FPlayerID := 0;
  FX := 0;
  FY := 0;
  FZ := 0;
  FDestinationX := 0;
  FDestinationY := 0;
  FLastUpdate := 0;
  FLevel := 0;
  FRealm := drNeutral;
  FName := '';
  FHeadWord := 0;
  FIsInUpdateRange := true;
end;

constructor TDAOCObject.Create;
begin
  inherited Create;
  Clear;
end;

function TDAOCObject.Distance2D(AObject: TDAOCObject): double;
begin
  Result := Distance2D(AObject.X, AObject.Y);
end;

function TDAOCObject.Distance3D(AObject: TDAOCObject): double;
begin
  Result := sqrt(DistanceSqr3D(AObject.X, AObject.Y, AObject.Z));
end;

function TDAOCObject.Distance3D(X, Y, Z: DWORD): double;
begin
  Result := sqrt(DistanceSqr3D(X, Y, Z));
end;

function TDAOCObject.GetConColor(AToLevel: integer): TColor;
begin
  case DAOCConSystem.GetConColor(AToLevel, FLevel) of
    ccGray:   Result := clSilver;
    ccGreen:  Result := clLime;
    ccBlue:   Result := $ff3300;  // blue
    ccYellow: Result := clYellow;
    ccOrange: Result := $007fff;  // orange
    ccRed:    Result := clRed;
    ccPurple: Result := $ff00cc;  // purple
    else
      Result := clBlack;
  end;  { cas l_steps_taken }
end;

function TDAOCObject.GetHead: integer;
begin
  Result := FHeadWord and $0fff;
  Result := Result * 360 div 4096;
    { Since the daoc coordinate system is top->bottom, we need to 'adjust' 180 degrees }
  dec(Result, 180);
  if Result < 0 then
    inc(Result, 360);
end;

function TDAOCObject.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocObject;
end;

function TDAOCObject.HeadRad: double;
begin
  Result := GetHead * (PI / 180);
end;

procedure TDAOCObject.LoadFromReader(AReader: TReader);
begin
  Clear;
  // FID :=
  FX := AReader.ReadInteger;
  FY := AReader.ReadInteger;
  FZ := AReader.ReadInteger;
  // FLastUpdate :=
  FName := AReader.ReadString;
  FRealm := TDAOCRealm(AReader.ReadInteger);
  FLevel := AReader.ReadInteger;
  FHeadWord := AReader.ReadInteger;
end;

function TDAOCObject.SameLoc(AObject: TDAOCObject): boolean;
begin
  Result := (AObject.X = FX) and (AObject.Y = FY) and (AObject.Z = FZ);
end;

procedure TDAOCObject.SaveToWriter(AWriter: TWriter);
begin
  // FID :=
  AWriter.WriteInteger(FX);
  AWriter.WriteInteger(FY);
  AWriter.WriteInteger(FZ);
  // FLastUpdate :=
  AWriter.WriteString(FName);
  AWriter.WriteInteger(ord(FRealm));
  AWriter.WriteInteger(FLevel);
  AWriter.WriteInteger(FHeadWord);
end;

procedure TDAOCObject.SetHeadWord(const Value: WORD);
begin
  FHeadWord := Value;
  Touch;
end;

procedure TDAOCObject.SetLevel(const Value: integer);
begin
  FLevel := Value;
  Touch;
end;

procedure TDAOCObject.SetName(const Value: string);
begin
  FName := Value;
  Touch;
end;

procedure TDAOCObject.SetRealm(const Value: TDAOCRealm);
begin
  FRealm := Value;
  Touch;
end;

procedure TDAOCObject.SetX(const Value: DWORD);
begin
  FX := Value;
  Touch;
end;

procedure TDAOCObject.SetY(const Value: DWORD);
begin
  FY := Value;
  Touch;
end;

procedure TDAOCObject.SetZ(const Value: DWORD);
begin
  FZ := Value;
  Touch;
end;

procedure TDAOCObject.Touch;
begin
  FLastUpdate := LocalTickCount;
  FStale := false;
end;

function TDAOCObject.Distance2D(X, Y: DWORD): double;
begin
  Result := Sqrt(DistanceSqr2D(X, Y));
end;

procedure TDAOCObject.SetDestinationX(const Value: DWORD);
begin
  FDestinationX := Value;
  Touch;
end;

procedure TDAOCObject.SetDestinationY(const Value: DWORD);
begin
  FDestinationY := Value;
  Touch;
end;

function TDAOCObject.GetName: string;
begin
  Result := FName;
end;

procedure TDAOCObject.SetStealthed(const Value: boolean);
begin
  FStealthed := Value;
  Touch;
end;

procedure TDAOCObject.SetDestinationZ(const Value: WORD);
begin
  FDestinationZ := Value;
  Touch;
end;

function TDAOCObject.DistanceSqr2D(AObject: TDAOCObject): double;
begin
  Result := DistanceSqr2D(AObject.X, AObject.Y);
end;

function TDAOCObject.DistanceSqr2D(X, Y: DWORD): double;
begin
  X := DWORDDelta(X, Self.FX);
  Y := DWORDDelta(Y, Self.FY);

  Result := X * X + Y * Y;
end;

function TDAOCObject.DistanceSqr3D(AObject: TDAOCObject): double;
begin
  Result := DistanceSqr3D(AObject.X, AObject.Y, AObject.Z);
end;

function TDAOCObject.DistanceSqr3D(X, Y, Z: DWORD): double;
begin
  X := DWORDDelta(X, Self.FX);
  Y := DWORDDelta(Y, Self.FY);
  Z := DWORDDelta(Z, Self.FZ);

  Result := X * X + Y * Y + Z * Z;
end;

procedure TDAOCObject.MarkStaleAtDestination;
begin
  FStale := true;
  if (FDestinationX <> 0) and (FDestinationY <> 0) then begin
    FX := FDestinationX;
    FY := FDestinationY;
    FZ := FDestinationZ;
  end;
end;

procedure TDAOCObject.CheckStale;
begin
;
end;

function TDAOCObject.TicksSinceUpdate: DWORD;
begin
  Result := LocalTickCount - FLastUpdate;
end;

{ TDAOCLocalPlayer }

procedure TDAOCLocalPlayer.Clear;
begin
  inherited Clear;
  FSkills.Clear;
  FSpecializations.Clear;
  FAbilities.Clear;
  FSpells.Clear;
  FStyles.Clear;
  FCurrency.Clear;
end;

constructor TDAOCLocalPlayer.Create;
begin
    { must create all objects before we call inherited because the inherited
      will call clear and we need objects for the clear }
  FSkills := TDAOCNameValueList.Create;
  FSpecializations := TDAOCNameValueList.Create;
  FAbilities := TDAOCNameValueList.Create;
  FSpells := TDAOCNameValueList.Create;
  FStyles := TDAOCNameValueList.Create;
  FCurrency := TDAOCCurrency.Create;

  inherited Create;
end;

destructor TDAOCLocalPlayer.Destroy;
begin
  FCurrency.Free;
  FStyles.Free;
  FSpells.Free;
  FAbilities.Free;
  FSkills.Free;
  FSpecializations.Free;

  inherited Destroy;
end;

function TDAOCLocalPlayer.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocLocalPlayer;
end;

{ TDAOCPlayer }

procedure TDAOCPlayer.Assign(ASrc: TDAOCPlayer);
begin
  inherited Assign(ASrc);
  
  FGuild := ASrc.Guild;
  FLastName := ASrc.FLastName;
end;

procedure TDAOCPlayer.CheckStale;
var
  dwLastUpdateDelta:  DWORD;
begin
  if FStale then
    exit;

  dwLastUpdateDelta := TicksSinceUpdate;
  FStale := (Speed <> 0) and (dwLastUpdateDelta > 15000);

  if FStale then
    FSpeedWord := 0;
end;

procedure TDAOCPlayer.Clear;
begin
  inherited Clear;
  FGuild := '';
  FLastName := '';
  FInventory.Clear;
end;

function TDAOCPlayer.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocPlayer;
end;

procedure TDAOCPlayer.InventoryChanged;
var
  I:    integer;
begin
  inherited;
  if FCharacterClass <> ccUnknown then
    exit;

  for I := 0 to FInventory.Count - 1 do begin
    FCharacterClass := FInventory[I].ClassRestriction;
    if FCharacterClass <> ccUnknown then
      exit;
  end;
end;

procedure TDAOCPlayer.SetLastName(const Value: string);
begin
  FLastName := Value;
  UpdateFullName;
end;

procedure TDAOCPlayer.SetName(const Value: string);
begin
  inherited;
  UpdateFullName;
end;

procedure TDAOCPlayer.UpdateFullName;
begin
  if FLastName <> '' then
    FFullName := FName + ' ' + FLastName
  else
    FFullName := FName;
end;

{ TDAOCMob }

procedure TDAOCMob.Assign(ASrc: TDAOCMob);
begin
  inherited Assign(ASrc);
  FTypeTag := ASrc.TypeTag;
end;

function TDAOCMob.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocMob;
end;

{ TDAOCCurrency }

function TDAOCCurrency.AsText: string;
begin
  Result := '';
  if FCopper > 0 then
    Result := Result + IntToStr(FCopper) + ' copper';
  if FSilver > 0 then begin
    if Result <> '' then
      Result := ', ' + Result;
    Result := IntToStr(FSilver) + ' silver' + Result;
  end;
  if FGold > 0 then begin
    if Result <> '' then
      Result := ', ' + Result;
    Result := IntToStr(FGold) + ' gold' + Result;
  end;
  if FPlatinum > 0 then begin
    if Result <> '' then
      Result := ', ' + Result;
    Result := IntToStr(FPlatinum) + ' platinum' + Result;
  end;
  if FMithril > 0 then begin
    if Result <> '' then
      Result := ', ' + Result;
    Result := IntToStr(FMithril) + ' mithril' + Result;
  end;
end;

procedure TDAOCCurrency.Clear;
begin
  FPlatinum := 0;
  FSilver := 0;
  FMithril := 0;
  FGold := 0;
  FCopper := 0;
end;

{ TDAOCObjectList }

procedure TDAOCObjectList.AddOrReplace(AObject: TDAOCObject);
var
  iPos: integer;
begin
  iPos := IndexOfInfoID(AObject.InfoID);
  if iPos <> -1 then
    Delete(iPos);

  Add(AObject);
end;

function TDAOCObjectList.FindByInfoID(AInfoID: integer): TDAOCObject;
var
  iPos: integer;
begin
  iPos := IndexOfInfoID(AInfoID);
  if iPos <> -1 then
    Result := Items[iPos]
  else
    Result := nil;
end;

function TDAOCObjectList.FindByPlayerID(APlayerID: integer): TDAOCObject;
var
  I:  integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I] is TDAOCPlayer) and (TDAOCPlayer(Items[I]).PlayerID = APlayerID) then begin
      Result := Items[I];
      exit;
    end;
    
  Result := nil;
end;

function TDAOCObjectList.FindNearest2D(X, Y: DWORD): TDAOCObject;
var
  dDist:    double;
  dMinDist: double;
  I:    integer;
begin
  Result := nil;
  dMinDist := 0;

  for I := 0 to Count - 1 do begin
    dDist := Items[I].DistanceSqr2D(X, Y);
    if not Assigned(Result) or (dDist < dMinDist) then begin
      Result := Items[I];
      dMinDist := dDist;
    end;
  end;  { for I .. count }
end;

function TDAOCObjectList.FindNearest3D(X, Y, Z: DWORD): TDAOCObject;
var
  dDist:    double;
  dMinDist: double;
  I:    integer;
begin
  Result := nil;
  dMinDist := 0;

  for I := 0 to Count - 1 do begin
    dDist := Items[I].DistanceSqr3D(X, Y, Z);
    if not Assigned(Result) or (dDist < dMinDist) then begin
      Result := Items[I];
      dMinDist := dDist;
    end;
  end;  { for I .. count }
end;

function TDAOCObjectList.GetItems(I: integer): TDAOCObject;
begin
  Result := TDAOCObject(inherited Items[I]);
end;

function TDAOCObjectList.IndexOfInfoID(AInfoID: integer): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].InfoID = AInfoID then
      exit;
  Result := -1;
end;

{ TDAOCUnknownMovingObject }

function TDAOCUnknownMovingObject.GetName: string;
begin
  Result := Format('Unknown NFO%4.4x 0x%4.4x', [FInfoID, FPlayerID]);
end;

function TDAOCUnknownMovingObject.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocUnknown;
end;

end.

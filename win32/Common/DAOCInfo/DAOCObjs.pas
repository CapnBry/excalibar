unit DAOCObjs;

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
{$IFDEF LINUX}
  QGraphics,
{$ELSE}
  Graphics,
{$ENDIF !LINUX}
  SysUtils, Classes, Contnrs,
  DAOCRegion, DAOCInventory, DAOCPlayerAttributes, DAOCConSystem, DAOCClasses,
  QuickSinCos;

type
  TDAOCObjectClass = (ocUnknown, ocObject, ocMob, ocPlayer, ocLocalPlayer,
    ocVehicle);
  TDAOCObjectClasses = set of TDAOCObjectClass;

  TDAOCObject = class(TObject)
  private
    procedure SetX(const Value: Cardinal);
    procedure SetY(const Value: Cardinal);
    procedure SetZ(const Value: Cardinal);
    procedure SetDestinationX(const Value: Cardinal);
    procedure SetDestinationY(const Value: Cardinal);
    procedure SetDestinationZ(const Value: WORD);
    procedure SetLevel(const Value: integer);
    procedure SetRealm(const Value: TDAOCRealm);
    procedure SetHeadWord(const Value: WORD);
    function GetHead: integer;
    procedure SetStealthed(const Value: boolean);
    function GetIsStale: boolean;
    function GetLiveDataConfidencePct: single;
    function GetIsDead: boolean;
    function GetIsAlive: boolean;
  protected
    FInfoID:WORD;
    FPlayerID: WORD;
    FX:     Cardinal;
    FY:     Cardinal;
    FZ:     Cardinal;
    FDestinationX:  Cardinal;
    FDestinationY:  Cardinal;
    FDestinationZ:  WORD;
    FName:  string;
    FLastUpdate:  Cardinal;
    FLevel: integer;
    FRealm: TDAOCRealm;
    FHeadWord:    WORD;
    FStealthed:   boolean;
    FLiveDataConfidence:  integer;
    FNext:        TDAOCObject;
    FPrev:        TDAOCObject;
    FHitPoints:   BYTE;
    FHitPointsLast: BYTE;

    function HeadRad: double;
    procedure SetName(const Value: string); virtual;
    function GetObjectClass : TDAOCObjectClass; virtual;
    function GetName : string; virtual;
    procedure SetHitPoints(const Value: BYTE);
  public
    LongestUpdateTime:    Cardinal;
    constructor Create; virtual;

    procedure Assign(ASrc: TDAOCObject);
    function AsString: string;
    procedure CheckStale; virtual;
    procedure Clear; virtual;
    function Distance2D(AObject: TDAOCObject) : double; overload;
    function Distance2D(X, Y: Cardinal) : double; overload;
    function Distance3D(AObject: TDAOCObject) : double; overload;
    function Distance3D(X, Y, Z: Cardinal) : double; overload;
    function DistanceSqr2D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr2D(X, Y: Cardinal) : double; overload; virtual;
    function DistanceSqr3D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr3D(X, Y, Z: Cardinal) : double; overload; virtual;
    function GetConColor(AToLevel: integer) : TColor;
    procedure LoadFromReader(AReader: TReader); virtual;
    procedure AssumeAtDestination; virtual;
    function SameLoc(AObject: TDAOCObject) : boolean;
    function SameLocAndHead(AObject: TDAOCObject) : boolean;
    procedure SaveToWriter(AWriter: TWriter); virtual;
    procedure Touch;
    function TicksSinceUpdate : Cardinal;

    property InfoID: WORD read FInfoID write FInfoID;
    property PlayerID: WORD read FPlayerID write FPlayerID;
    property LastUpdate: Cardinal read FLastUpdate;
    property X: Cardinal read FX write SetX;
    property Y: Cardinal read FY write SetY;
    property Z: Cardinal read FZ write SetZ;
    property DestinationX: Cardinal read FDestinationX write SetDestinationX;
    property DestinationY: Cardinal read FDestinationY write SetDestinationY;
    property DestinationZ: WORD read FDestinationZ write SetDestinationZ;
    property Head: integer read GetHead;
    property HeadWord: WORD read FHeadWord write SetHeadWord;
    property HitPoints: BYTE read FHitPoints write SetHitPoints;
    property HitPointsLast: BYTE read FHitPointsLast; 
    property IsAlive: boolean read GetIsAlive;
    property IsDead: boolean read GetIsDead;
    property IsStale: boolean read GetIsStale;
    property Level: integer read FLevel write SetLevel;
    property Realm: TDAOCRealm read FRealm write SetRealm;
    property LiveDataConfidence: integer read FLiveDataConfidence;
    property LiveDataConfidencePct: single read GetLiveDataConfidencePct;
    property Stealthed: boolean read FStealthed write SetStealthed;
    property ObjectClass: TDAOCObjectClass read GetObjectClass;
    property Name: string read GetName write SetName;
    property Next: TDAOCObject read FNext;
  end;

  TDAOCObjectNotify = procedure (ASender: TObject; ADAOCObject: TDAOCObject) of Object;

  TDAOCObjectList = class(TObjectList)
  private
    FTakingItem:  boolean;
    function GetItems(I: integer): TDAOCObject;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure AddOrReplace(AObject: TDAOCObject);
    function IndexOfInfoID(AInfoID: integer) : integer;
    function IndexOfPlayerID(APlayerID: integer) : integer;
    function FindByInfoID(AInfoID: integer) : TDAOCObject;
    function FindByPlayerID(APlayerID: integer) : TDAOCObject;
    function FindNearest3D(X, Y, Z: Cardinal) : TDAOCObject;
    function FindNearest2D(X, Y: Cardinal) : TDAOCObject;
    function Take(I: integer) : TDAOCObject;

    property Items[I: integer]: TDAOCObject read GetItems; default;
  end;

  TDAOCObjectLinkedList = class(TObject)
  private
    FCount: integer;
    FHead: TDAOCObject;
  public
    destructor Destroy; override;

    procedure Add(ADAOCObj: TDAOCObject);
    function Delete(ADAOCObj: TDAOCObject) : TDAOCObject;
    function Remove(ADAOCObj: TDAOCObject) : TDAOCObject;

    // procedure AddOrReplaceByInfoID(ADAOCObj: TDAOCObject);
    procedure Clear;
    function FindByInfoID(AInfoID: integer) : TDAOCObject;
    function FindByPlayerID(APlayerID: integer) : TDAOCObject;
    function FindNearest3D(X, Y, Z: Cardinal) : TDAOCObject;
    function FindNearest2D(X, Y: Cardinal) : TDAOCObject;
    function FindByName(const AName: string) : TDAOCObject;

    property Head: TDAOCObject read FHead;
    property Count: integer read FCount;
  end;

  TDAOCUnknownStealther = class(TDAOCObject)
  public
    procedure CheckStale; override;
  end;

  TDAOCMovingObject = class(TDAOCObject)
  private
    function GetSpeed: integer;
    procedure SetSpeedWord(const Value: WORD);
    function GetIsSwimming: boolean;
    function GetXProjected: Cardinal;
    function GetYProjected: Cardinal;
    function GetSpeedString: string;
  protected
    FSpeedWord:   WORD;
    FProjectedX:  Cardinal;
    FProjectedY:  Cardinal;
    FProjectedLastUpdate: Cardinal;
    FInventory: TDAOCInventory;

    procedure UpdateLastProjected;
  public
    constructor Create; override;
    destructor Destroy; override;

    function DistanceSqr2D(X, Y: Cardinal) : double; override; 
    function DistanceSqr3D(X, Y, Z: Cardinal) : double; override; 

    procedure Assign(ASrc: TDAOCMovingObject);
    procedure Clear; override;
    procedure CheckStale; override;
    function DestinationAhead : boolean;
    procedure InventoryChanged; virtual;
    procedure AssumeAtDestination; override;

    property XProjected: Cardinal read GetXProjected;
    property YProjected: Cardinal read GetYProjected;
    property Speed: integer read GetSpeed;
    property SpeedWord: WORD read FSpeedWord write SetSpeedWord;
    property SpeedString: string read GetSpeedString;
    property IsSwimming: boolean read GetIsSwimming;
    property Inventory: TDAOCInventory read FInventory;
  end;

  TDAOCUnknownMovingObject = class(TDAOCMovingObject)
  protected
    function GetName : string; override;
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure CheckStale; override;
  end;

  TDAOCVehicle = class(TDAOCMovingObject)
  protected
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure CheckStale; override;
  end;

  TDAOCMob = class(TDAOCMovingObject)
  private
  protected
    FTypeTag: string;
    FTarget:  TDAOCMovingObject;
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCMob);
    procedure CheckStale; override;

    property Target: TDAOCMovingObject read FTarget write FTarget;
    property TypeTag: string read FTypeTag write FTypeTag;
  end;

  TDAOCMobNotify = procedure (ASender: TObject; ADAOCMob: TDAOCMob) of Object;

  TRealmRank = (rrUnknown, rr1, rr2, rr3, rr4, rr5, rr6, rr7, rr8, rr9, rr10);

  TDAOCPlayer = class(TDAOCMovingObject)
  private
    procedure SetRealmRank(const Value: TRealmRank);
  protected
    FGuild: string;
    FLastName:  string;
    FFullName:  string;
    FIsInGuild: boolean;
    FIsInGroup: boolean;
    FCharacterClass:  TDAOCCharacterClass;
    FRealmRank: TRealmRank;
    FRealmRankStr:  string;
    procedure UpdateFullName;
    procedure UpdateRealmRankStr;
    procedure SetName(const Value: string); override;
    procedure SetLastName(const Value: string);
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCPlayer);
    procedure Clear; override;

    procedure CheckStale; override;
    procedure InventoryChanged; override;

    property IsInGuild: boolean read FIsInGuild write FIsInGuild;
    property IsInGroup: boolean read FIsInGroup write FIsInGroup;
    property Guild: string read FGuild write FGuild;
    property LastName: string read FLastName write SetLastName;
    property FullName: string read FFullName;
    property CharacterClass: TDAOCCharacterClass read FCharacterClass;
    property RealmRank: TRealmRank read FRealmRank write SetRealmRank;
    property RealmRankStr: string read FRealmRankStr;
  end;

  TDAOCCurrency = class(TObject)
  private
    FPlatinum: integer;
    FSilver: integer;
    FMithril: integer;
    FGold: integer;
    FCopper: integer;
    function GetAsCopper: Cardinal;
    procedure SetAsCopper(Value: Cardinal);
  public
    procedure Clear;
    function AsText : string;

    property AsCopper: Cardinal read GetAsCopper write SetAsCopper; 
    property Copper: integer read FCopper write FCopper;
    property Silver: integer read FSilver write FSilver;
    property Gold: integer read FGold write FGold;
    property Platinum: integer read FPlatinum write FPlatinum;
    property Mithril: integer read FMithril write FMithril;
  end;

  TDAOCLocalPlayer = class(TDAOCMovingObject)
  private
    function GetFullName: string;
  protected
    FSkills:    TDAOCNameValueList;
    FSpecializations: TDAOCNameValueList;
    FAbilities: TDAOCNameValueList;
    FSpells:    TDAOCNameValueList;
    FStyles:    TDAOCNameValueList;
    FCurrency:  TDAOCCurrency;
    FBaseClass: string;
    FRace:      string;
    FHouseTitle:  string;
    FGuild:     string;
    FLastName:  string;
    FRealmTitle:  string;
    FHouse:     string;
    FPlayerClassStr:  string;
    function GetObjectClass : TDAOCObjectClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    property Abilities: TDAOCNameValueList read FAbilities;
    property BaseClass: string read FBaseClass write FBaseClass;
    property Currency: TDAOCCurrency read FCurrency;
    property FullName: string read GetFullName;
    property Guild: string read FGuild write FGuild;
    property House: string read FHouse write FHouse;
    property HouseTitle: string read FHouseTitle write FHouseTitle;
    property LastName: string read FLastName write FLastName;
    property PlayerClassStr: string read FPlayerClassStr write FPlayerClassStr;
    property Race: string read FRace write FRace;
    property RealmTitle: string read FRealmTitle write FRealmTitle;
    property Skills: TDAOCNameValueList read FSkills;
    property Specializations: TDAOCNameValueList read FSpecializations;
    property Spells: TDAOCNameValueList read FSpells;
    property Styles: TDAOCNameValueList read FStyles;
  end;

function DAOCObjectClassToStr(AClass: TDAOCObjectClass) : string;
function IntToObjectClasses(AVal: integer) : TDAOCObjectClasses;
function ObjectClassesToInt(AVal: TDAOCObjectClasses) : integer;
function CopperToStr(ACopper: integer) : string;
function CardinalDelta(A, B: Cardinal) : Cardinal;

const
  LIVE_DATA_CONFIDENCE_MAX = 100;

implementation

uses
  GlobalTickCounter;

const
  SPEED_1X = 191;

  COPPER_PER_SILVER = 100;
  COPPER_PER_GOLD = 10000;
  COPPER_PER_PLATINUM = 10000000;
  COPPER_PER_MITHRIL = 10000000000;

function IntToObjectClasses(AVal: integer) : TDAOCObjectClasses;
var
  I:    TDAOCObjectClass;
begin
  Result := [];
  for I := low(TDAOCObjectClass) to high(TDAOCObjectClass) do
    if (AVal and (1 shl ord(I))) <> 0 then
      Include(Result, I);
end;

function ObjectClassesToInt(AVal: TDAOCObjectClasses) : integer;
var
  I:    TDAOCObjectClass;
begin
  Result := 0;
  for I := low(TDAOCObjectClass) to high(TDAOCObjectClass) do
    if I in AVal then
      Result := Result or (1 shl ord(I));
end;

function CardinalDelta(A, B: Cardinal) : Cardinal;
begin
  if A > B then
    Result := A - B
  else
    Result := B - A;
end;

function max(a, b: integer) : integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function DAOCObjectClassToStr(AClass: TDAOCObjectClass) : string;
begin
  case AClass of
    ocUnknown:    Result := 'Unknown';
    ocObject:     Result := 'Object';
    ocMob:        Result := 'MOB';
    ocPlayer:     Result := 'Player';
    ocLocalPlayer:  Result := 'Local Player';
    ocVehicle:    Result := 'Vehicle';
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

function LocalTickCount : Cardinal;
begin
  Result := GlobalTickCount;
end;

{ TDAOCMovingObject }

procedure TDAOCMovingObject.Assign(ASrc: TDAOCMovingObject);
begin
  inherited Assign(ASrc);

  FSpeedWord := ASrc.SpeedWord;
end;

procedure TDAOCMovingObject.CheckStale;
begin
  if IsStale then
    exit;

  if not DestinationAhead then
    AssumeAtDestination;
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
      0:  HeadQuad := q3;
      1:  HeadQuad := q2;
      2:  HeadQuad := q1;
      3:  HeadQuad := q4;
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

function TDAOCObject.GetIsAlive: boolean;
begin
  REsult := FHitPoints <> 0;
end;

function TDAOCObject.GetIsDead: boolean;
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

function TDAOCMovingObject.GetXProjected: Cardinal;
begin
  UpdateLastProjected;
  Result := FProjectedX;
end;

function TDAOCMovingObject.GetYProjected: Cardinal;
begin
  UpdateLastProjected;
  Result := FProjectedY;
end;

procedure TDAOCMovingObject.InventoryChanged;
begin
end;

procedure TDAOCMovingObject.AssumeAtDestination;
begin
  inherited;
  FSpeedWord := 0;
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
    FProjectedX := FX - round(s * dHyp);
    FProjectedY := FY + round(c * dHyp);
  end;
end;

function TDAOCMovingObject.DistanceSqr2D(X, Y: Cardinal): double;
var
  fx, fy:   double;
begin
  fx := CardinalDelta(X, Self.FProjectedX);
  fy := CardinalDelta(Y, Self.FProjectedY);

  Result := fx * fx + fy * fy;
end;

function TDAOCMovingObject.DistanceSqr3D(X, Y, Z: Cardinal): double;
var
  fx, fy, fz:   double;
begin
  fx := CardinalDelta(X, Self.FProjectedX);
  fy := CardinalDelta(Y, Self.FProjectedY);
  fz := CardinalDelta(Z, Self.FZ);

  Result := fx * fx + fy * fy + fz * fz;
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
end;

constructor TDAOCObject.Create;
begin
  inherited Create;
  Clear;
  Touch;
end;

function TDAOCObject.Distance2D(AObject: TDAOCObject): double;
begin
  Result := Distance2D(AObject.X, AObject.Y);
end;

function TDAOCObject.Distance3D(AObject: TDAOCObject): double;
begin
  Result := sqrt(DistanceSqr3D(AObject.X, AObject.Y, AObject.Z));
end;

function TDAOCObject.Distance3D(X, Y, Z: Cardinal): double;
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
//  dec(Result, 180);
//  if Result < 0 then
//    inc(Result, 360);
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
  FLevel := Value and $7f;  // $80 not targetable?
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

procedure TDAOCObject.SetX(const Value: Cardinal);
begin
  FX := Value;
  Touch;
end;

procedure TDAOCObject.SetY(const Value: Cardinal);
begin
  FY := Value;
  Touch;
end;

procedure TDAOCObject.SetZ(const Value: Cardinal);
begin
  FZ := Value;
  Touch;
end;

procedure TDAOCObject.Touch;
var
  dw:   Cardinal;
begin
  dw := TicksSinceUpdate;
  if (FLastUpdate > 0) and (dw > LongestUpdateTime) then
    LongestUpdateTime := dw;
    
  FLastUpdate := LocalTickCount;
  FLiveDataConfidence := LIVE_DATA_CONFIDENCE_MAX;
end;

function TDAOCObject.Distance2D(X, Y: Cardinal): double;
begin
  Result := Sqrt(DistanceSqr2D(X, Y));
end;

procedure TDAOCObject.SetDestinationX(const Value: Cardinal);
begin
  FDestinationX := Value;
  Touch;
end;

procedure TDAOCObject.SetDestinationY(const Value: Cardinal);
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

function TDAOCObject.DistanceSqr2D(X, Y: Cardinal): double;
var
  fx, fy:   double;
begin
  fx := CardinalDelta(X, Self.FX);
  fy := CardinalDelta(Y, Self.FY);

  Result := fx * fx + fy * fy;
end;

function TDAOCObject.DistanceSqr3D(AObject: TDAOCObject): double;
begin
  Result := DistanceSqr3D(AObject.X, AObject.Y, AObject.Z);
end;

function TDAOCObject.DistanceSqr3D(X, Y, Z: Cardinal): double;
var
  fx, fy, fz:   double;
begin
  fx := CardinalDelta(X, Self.FX);
  fy := CardinalDelta(Y, Self.FY);
  fz := CardinalDelta(Z, Self.FZ);

  Result := fx * fx + fy * fy + fz * fz;
end;

procedure TDAOCObject.AssumeAtDestination;
begin
//  FLiveDataConfidence := 75;
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

function TDAOCObject.TicksSinceUpdate: Cardinal;
begin
  Result := LocalTickCount - FLastUpdate;
end;

function TDAOCObject.GetIsStale: boolean;
begin
  Result := FLiveDataConfidence = 0;
end;

function TDAOCObject.GetLiveDataConfidencePct: single;
begin
  Result := FLiveDataConfidence * (1 / LIVE_DATA_CONFIDENCE_MAX);
end;

procedure TDAOCObject.SetHitPoints(const Value: BYTE);
begin
  FHitPointsLast := FHitPoints;
  FHitPoints := Value and $7f;  // bit $80 means *something*
end;

function TDAOCObject.SameLocAndHead(AObject: TDAOCObject): boolean;
begin
  Result := SameLoc(AObject) and (AObject.HeadWord = FHeadWord);
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
  FPlayerClassStr := '';
  FBaseClass := '';
  FRace := '';
  FHouseTitle := '';
  FGuild := '';
  FLastName := '';
  FRealmTitle := '';
  FHouse := '';
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

function TDAOCLocalPlayer.GetFullName: string;
begin
  Result := FName;
  if (FLastName <> '') and not AnsiSameText(FLastName, 'None') then
    Result := Result + ' ' + FLastName;
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
  dwLastUpdateDelta:  Cardinal;
begin
  if IsStale then
    exit;

  { I think players are supposed to be updated every 4s, regardless of their speed,
    but this code tries to be a little safer. }

  dwLastUpdateDelta := TicksSinceUpdate;
    { for players that aren't moving, they start going stale at
      10s and time out after 20s }
//  if Speed = 0 then
//    if dwLastUpdateDelta < 10000 then
//      FLiveDataConfidence := LIVE_DATA_CONFIDENCE_MAX
//    else
//      FLiveDataConfidence := max(LIVE_DATA_CONFIDENCE_MAX - (dwLastUpdateDelta - 10000)
//        div 100, 0)

//    { players who are moving, stale period 5s-10s }
//  else
        { all players should receive updates every give them 5 to 10s }
    if dwLastUpdateDelta < 5000 then
      FLiveDataConfidence := LIVE_DATA_CONFIDENCE_MAX
    else
      FLiveDataConfidence := max(LIVE_DATA_CONFIDENCE_MAX - (dwLastUpdateDelta - 5000)
        div 50, 0);

  if FLiveDataConfidence = 0 then
    FSpeedWord := 0;
end;

procedure TDAOCPlayer.Clear;
begin
  inherited Clear;
  FGuild := '';
  FLastName := '';
  FInventory.Clear;
  FRealmRank := rrUnknown;
  FRealmRankStr := '';
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

procedure TDAOCPlayer.SetRealmRank(const Value: TRealmRank);
begin
  FRealmRank := Value;
  UpdateRealmRankStr;
end;

procedure TDAOCPlayer.UpdateFullName;
begin
  if FLastName <> '' then
    FFullName := FName + ' ' + FLastName
  else
    FFullName := FName;
end;

procedure TDAOCPlayer.UpdateRealmRankStr;
begin
  case FRealmRank of
    rrUnknown:  FRealmRankStr := '';
    rr1:  FRealmRankStr := 'RR1';
    rr2:  FRealmRankStr := 'RR2';
    rr3:  FRealmRankStr := 'RR3';
    rr4:  FRealmRankStr := 'RR4';
    rr5:  FRealmRankStr := 'RR5';
    rr6:  FRealmRankStr := 'RR6';
    rr7:  FRealmRankStr := 'RR7';
    rr8:  FRealmRankStr := 'RR8';
    rr9:  FRealmRankStr := 'RR9';
    rr10: FRealmRankStr := 'RR10';
  end;
end;

{ TDAOCMob }

procedure TDAOCMob.Assign(ASrc: TDAOCMob);
begin
  inherited Assign(ASrc);
  FTypeTag := ASrc.TypeTag;
end;

procedure TDAOCMob.CheckStale;
var
  dwTicksSinceUpdate:   Cardinal;
begin
  inherited;
  if IsStale then
    exit;

  dwTicksSinceUpdate := TicksSinceUpdate;
    { live mobs are stale from 20s-40s.  It looks like they get an update every
      ~10s or ~20s over UDP, so this allows for a dropped packet or two.  Plus
      we have the DestinationCheck to back it up }
  if dwTicksSinceUpdate > 20000 then
    FLiveDataConfidence := max(
      LIVE_DATA_CONFIDENCE_MAX - (dwTicksSinceUpdate - 20000) div 400, 0);
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

function TDAOCCurrency.GetAsCopper: Cardinal;
begin
  Result := ( Copper ) +
            ( Silver   * COPPER_PER_SILVER ) +
            ( Gold     * COPPER_PER_GOLD ) +
            ( Platinum * COPPER_PER_PLATINUM ) +
            ( Mithril  * COPPER_PER_MITHRIL );
end;

procedure TDAOCCurrency.SetAsCopper(Value: Cardinal);
begin
  FMithril := Value div COPPER_PER_MITHRIL;
  dec(Value, FMithril * COPPER_PER_MITHRIL);

  FPlatinum := Value div COPPER_PER_PLATINUM;
  dec(Value, FPlatinum * COPPER_PER_PLATINUM);

  FGold := Value div COPPER_PER_GOLD;
  dec(Value, FGold * COPPER_PER_GOLD);

  FSilver := Value div COPPER_PER_SILVER;
  dec(Value, FSilver * COPPER_PER_SILVER);

  FCopper := Value;
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
  I := IndexOfPlayerID(APlayerID);
  if I <> -1 then
    Result := Items[I]
  else
    Result := nil;
end;

function TDAOCObjectList.FindNearest2D(X, Y: Cardinal): TDAOCObject;
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

function TDAOCObjectList.FindNearest3D(X, Y, Z: Cardinal): TDAOCObject;
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

function TDAOCObjectList.IndexOfPlayerID(APlayerID: integer): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].PlayerID = APlayerID then
      exit;
  Result := -1;
end;

procedure TDAOCObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
    { if we're taking the item, we don't want to call the inherited's Free on it }
  if not FTakingItem then
    inherited;
end;

function TDAOCObjectList.Take(I: integer): TDAOCObject;
begin
  Result := Items[I];
  FTakingItem := true;
  Delete(I);
  FTakingItem := false;
end;

{ TDAOCUnknownMovingObject }

procedure TDAOCUnknownMovingObject.CheckStale;
var
  dwTicksSinceUpdate:   Cardinal;
begin
  inherited;
  if IsStale then
    exit;

    { stale in 20-30s }
  dwTicksSinceUpdate := TicksSinceUpdate;
  if dwTicksSinceUpdate > 20000 then
    FLiveDataConfidence := max(
      LIVE_DATA_CONFIDENCE_MAX - (dwTicksSinceUpdate - 20000) div 100, 0);
end;

function TDAOCUnknownMovingObject.GetName: string;
begin
  Result := Format('Unknown NFO%4.4x 0x%4.4x', [FInfoID, FPlayerID]);
end;

function TDAOCUnknownMovingObject.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocUnknown;
end;

{ TDAOCObjectLinkedList }

procedure TDAOCObjectLinkedList.Add(ADAOCObj: TDAOCObject);
begin
//  WriteLn(Format('%8.8p BeforeAdd %8.8p count %d', [Pointer(Self), Pointer(ADAOCObj), FCount]));

  if Assigned(FHead) then begin
    ADAOCObj.FNext := FHead;
    ADAOCObj.FPrev := nil;
    FHead.FPrev := ADAOCObj;
  end
  else begin
    ADAOCObj.FNext := nil;
    ADAOCObj.FPrev := nil;
  end;

  FHead := ADAOCObj;
  inc(FCount);
end;

(**
procedure TDAOCObjectLinkedList.AddOrReplaceByInfoID(ADAOCObj: TDAOCObject);
begin
  Delete(FindByInfoID(ADAOCObj.InfoID));
  Add(ADAOCObj);
end;
**)

procedure TDAOCObjectLinkedList.Clear;
var
  pTmp:   TDAOCObject;
begin
  while Assigned(FHead) do begin
    pTmp := FHead;
    FHead := FHead.FNext;
    pTmp.Free;
  end;

  FCount := 0;
  FHead := nil;
end;

function TDAOCObjectLinkedList.Delete(ADAOCObj: TDAOCObject) : TDAOCObject;
begin
  Result := Remove(ADAOCObj);
  ADAOCObj.Free;
end;

destructor TDAOCObjectLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TDAOCObjectLinkedList.FindByInfoID(AInfoID: integer): TDAOCObject;
begin
  Result := FHead;
  while Assigned(Result) do begin
    if Result.InfoID = AInfoID then
      exit;
    Result := Result.FNext;
  end;
end;

function TDAOCObjectLinkedList.FindByName(const AName: string): TDAOCObject;
begin
  Result := FHead;
  while Assigned(Result) do begin
    if AnsiSameText(Result.Name, AName) then
      exit;
    Result := Result.FNext;
  end;
end;

function TDAOCObjectLinkedList.FindByPlayerID(APlayerID: integer): TDAOCObject;
begin
  Result := FHead;
  while Assigned(Result) do begin
    if Result.PlayerID = APlayerID then
      exit;
    Result := Result.FNext;
  end;
end;

function TDAOCObjectLinkedList.FindNearest2D(X, Y: Cardinal): TDAOCObject;
var
  dDist:    double;
  dMinDist: double;
  pTmp:     TDAOCObject;
begin
  Result := nil;
  dMinDist := 0;
  pTmp := FHead;

  while Assigned(pTmp) do begin
    dDist := Result.DistanceSqr2D(X, Y);
    if not Assigned(Result) or (dDist < dMinDist) then begin
      Result := pTmp;
      dMinDist := dDist;
    end;

    pTmp := pTmp.FNext;
  end;  { while pTmp }
end;

function TDAOCObjectLinkedList.FindNearest3D(X, Y, Z: Cardinal): TDAOCObject;
var
  dDist:    double;
  dMinDist: double;
  pTmp:     TDAOCObject;
begin
  Result := nil;
  dMinDist := 0;
  pTmp := FHead;
  while Assigned(pTmp) do begin
    dDist := Result.DistanceSqr3D(X, Y, Z);
    if not Assigned(Result) or (dDist < dMinDist) then begin
      Result := pTmp;
      dMinDist := dDist;
    end;

    pTmp := pTmp.FNext;
  end;  { while pTmp }
end;

function TDAOCObjectLinkedList.Remove(ADAOCObj: TDAOCObject) : TDAOCObject;
(*** Remove the object without freeing, return passed object's next ***)
begin
//  WriteLn(Format('%8.8p BeforeRemove %8.8p count %d', [Pointer(Self), Pointer(ADAOCObj), FCount]));

  if not Assigned(ADAOCObj) then begin
    Result := nil;
    exit;
  end;

  if Assigned(ADAOCObj.FNext) then
    ADAOCObj.FNext.FPrev := ADAOCObj.FPrev;
  if Assigned(ADAOCObj.FPrev) then
    ADAOCObj.FPrev.FNext := ADAOCObj.FNext;
  if ADAOCObj = FHead then
    FHead := ADAOCObj.FNext;

  Result := ADAOCObj.FNext;
  ADAOCObj.FNext := nil;
  ADAOCObj.FPrev := nil;
  dec(FCount);
end;

{ TDAOCVehicle }

procedure TDAOCVehicle.CheckStale;
var
  dwTicksSinceUpdate:   Cardinal;
begin
  inherited;
  if IsStale then
    exit;

  dwTicksSinceUpdate := TicksSinceUpdate;
    { copied from mob.  need to figure out how often we get vehicle updates }
  if dwTicksSinceUpdate > 30000 then
    FLiveDataConfidence := max(
      LIVE_DATA_CONFIDENCE_MAX - (dwTicksSinceUpdate - 30000) div 300, 0);
end;

function TDAOCVehicle.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocVehicle;
end;

{ TDAOCUnknownStealther }

procedure TDAOCUnknownStealther.CheckStale;
var
  dwLastUpdateDelta:  Cardinal;
begin
  if IsStale then
    exit;

  dwLastUpdateDelta := TicksSinceUpdate;
    { 4-10s stale decay }
  if dwLastUpdateDelta < 4000 then
    FLiveDataConfidence := LIVE_DATA_CONFIDENCE_MAX
  else
    FLiveDataConfidence := max(LIVE_DATA_CONFIDENCE_MAX - (dwLastUpdateDelta - 4000)
      div 60, 0);
end;

end.

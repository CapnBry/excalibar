unit DAOCObjs;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Graphics,
  DAOCRegion, DAOCInventory, DAOCPlayerAttributes;

type
  TDAOCObjectClass = (ocUnknown, ocObject, ocMob, ocPlayer, ocLocalPlayer);

  TDAOCObject = class(TObject)
  private
    FDestinationZ: WORD;
    procedure SetX(const Value: DWORD);
    procedure SetY(const Value: DWORD);
    procedure SetZ(const Value: DWORD);
    procedure SetDestinationX(const Value: DWORD);
    procedure SetDestinationY(const Value: DWORD);
    procedure SetDestinationZ(const Value: WORD);
    procedure SetLevel(const Value: integer);
    procedure SetRealm(const Value: TDAOCRealm);
    procedure SetName(const Value: string);
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
    FReam:  TDAOCRealm;
    FName:  string;
    FLastUpdate:  DWORD;
    FLevel: integer;
    FRealm: TDAOCRealm;
    FHeadWord:   WORD;
    FStealthed:  boolean;

    function HeadRad: double;
    function GetObjectClass : TDAOCObjectClass; virtual;
    function GetName : string; virtual;
  public
    constructor Create; virtual;

    procedure Assign(ASrc: TDAOCObject);
    procedure Touch;
    procedure Clear; virtual;
    function SameLoc(AObject: TDAOCObject) : boolean;
    function AsString: string;
    procedure LoadFromReader(AReader: TReader); virtual;
    procedure SaveToWriter(AWriter: TWriter); virtual;
    function GetConColor(AToLevel: integer) : TColor;
    function Distance2D(AObject: TDAOCObject) : double; overload;
    function Distance2D(X, Y: DWORD) : double; overload;
    function Distance3D(AObject: TDAOCObject) : double; overload;
    function Distance3D(X, Y, Z: DWORD) : double; overload;
    function DistanceSqr2D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr2D(X, Y: DWORD) : double; overload;
    function DistanceSqr3D(AObject: TDAOCObject) : double; overload;
    function DistanceSqr3D(X, Y, Z: DWORD) : double; overload;

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
    property Realm: TDAOCRealm read FReam write SetRealm;
    property Stealthed: boolean read FStealthed write SetStealthed;
    property ObjectClass: TDAOCObjectClass read GetObjectClass;
    property Name: string read GetName write SetName;
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
    function FindNearest(X, Y, Z: DWORD) : TDAOCObject;

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
  protected
    FHitPoints:  BYTE;
    FSpeedWord:  WORD;
  public
    procedure Assign(ASrc: TDAOCMovingObject);
    procedure Clear; override;

    property XProjected: DWORD read GetXProjected;
    property YProjected: DWORD read GetYProjected;
    property Speed: integer read GetSpeed;
    property SpeedWord: WORD read FSpeedWord write SetSpeedWord;
    property IsDead: boolean read GetIsDead;
    property IsSwimming: boolean read GetIsSwimming;
    property HitPoints: BYTE read FHitPoints write FHitPoints;
  end;

  TDAOCUnknownMovingObject = class(TDAOCMovingObject)
  protected
    function GetName : string; override;
    function GetObjectClass : TDAOCObjectClass; override;
  end;

  TDAOCPlayer = class(TDAOCMovingObject)
  private
    FGuild: string;
    FLastName: string;
    function GetFullName: string;
  protected
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCPlayer);
    procedure Clear; override;

    property Guild: string read FGuild write FGuild;
    property LastName: string read FLastName write FLastName;
    property FullName: string read GetFullName;
  end;

  TDAOCMob = class(TDAOCMovingObject)
  private
    FTypeTag: string;
  protected
    function GetObjectClass : TDAOCObjectClass; override;
  public
    procedure Assign(ASrc: TDAOCMob);

    property TypeTag: string read FTypeTag write FTypeTag;
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
    FInventory: TDAOCInventory;
    FSkills:    TDAOCNameValueList;
    FSpecializations: TDAOCNameValueList;
    FAbilities: TDAOCNameValueList;
    FSpells:    TDAOCNameValueList;
    FStyles:    TDAOCNameValueList;
    FCurrency:  TDAOCCurrency;
  protected
    function GetObjectClass : TDAOCObjectClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    property Inventory: TDAOCInventory read FInventory;
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

{ TDAOCMovingObject }

procedure TDAOCMovingObject.Assign(ASrc: TDAOCMovingObject);
begin
  inherited Assign(ASrc);

  FSpeedWord := ASrc.SpeedWord;
end;

(*
procedure TDAOCMovingObject.SetHeadWord(AHead: WORD);
begin
  AHead := AHead and $0fff;
    { (head * 360.0) / 4096.0 * M_PI / 180.0 }
//  FHeadRad := (AHead / 2048.0) * PI;
  FHead := (AHead * 360) div 4096;

  dec(FHead, 180);
  if FHead < 0  then
    inc(FHead, 360);

  Touch;
end;

procedure TDAOCMovingObject.SetSpeedWord(ASpeed: WORD);
begin
  FSpeed := ASpeed and $01ff;
  if (ASpeed and $0200) <> 0 then
    FSpeed := -FSpeed;
  Touch;
end;
*)

procedure TDAOCMovingObject.Clear;
begin
  inherited;
  FSpeedWord := 0;
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

function TDAOCMovingObject.GetXProjected: DWORD;
begin
    { TODO:  Project moving object position }
  if Speed = 0 then
    Result := FX
  else
    Result := round(FX + (sin(HeadRad) *
      (Speed * ((GetTickCount - LastUpdate) / 1000))));
end;

function TDAOCMovingObject.GetYProjected: DWORD;
begin
    { TODO:  Project moving object position }
  if Speed = 0 then
    Result := FY
  else
    Result := round(FY - (cos(HeadRad) *
      (Speed * ((GetTickCount - LastUpdate) / 1000))));
end;

procedure TDAOCMovingObject.SetSpeedWord(const Value: WORD);
begin
  FSpeedWord := Value;
  Touch;
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
  FReam := ASrc.Realm;
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
  FRealm := drFriend;
  FName := '';
  FHeadWord := 0;
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
var
  l_quanta:   integer;
  l_steps_taken:  integer;
begin
  l_steps_taken := 0;
  while (AToLevel > 0) and (AToLevel < 100) and (l_steps_taken > -3) and
    (l_steps_taken < 3) do begin
    l_quanta := (AToLevel div 10) + 1;
    if l_quanta > 5 then
      l_quanta := 5;

    if (FLevel > (AToLevel - l_quanta)) and (FLevel <= AToLevel) then
      break;

    if FLevel < AToLevel then begin
      dec(AToLevel, l_quanta);
      dec(l_steps_taken);
    end
    else begin
      inc(AToLevel, l_quanta);
      inc(l_steps_taken);
    end;
  end;  { while AToLevel in range }

  case l_steps_taken of
    -3:  Result := clSilver;
    -2:  Result := clLime;
    -1:  Result := clBlue;
     0:  Result := clYellow;
     1:  Result := $007fff;  // orange
     2:  Result := clRed;
     3:  Result := clFuchsia;
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
  FReam := TDAOCRealm(AReader.ReadInteger);
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
  AWriter.WriteInteger(ord(FReam));
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
  FReam := Value;
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
  FLastUpdate := GetTickCount;
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

{ TDAOCLocalPlayer }

procedure TDAOCLocalPlayer.Clear;
begin
  inherited Clear;
  FInventory.Clear;
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
  FInventory := TDAOCInventory.Create;
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
  FInventory.Free;

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

procedure TDAOCPlayer.Clear;
begin
  inherited Clear;
  FGuild := '';
  FLastName := '';  
end;

function TDAOCPlayer.GetFullName: string;
begin
  if FLastName <> '' then
    Result := FName + ' ' + FLastName
  else
    Result := FName;
end;

function TDAOCPlayer.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocPlayer;
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

function TDAOCObjectList.FindNearest(X, Y, Z: DWORD): TDAOCObject;
var
  dDist:    double;
  dMinDist: double;
  I:    integer;
begin
  Result := nil;
  dMinDist := 0;

  for I := 0 to Count - 1 do begin
    dDist := Items[I].Distance3D(X, Y, Z);
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
  Result := Format('UnknownObject NFO%4.4x 0x%4.4x', [FInfoID, FPlayerID]);
end;

function TDAOCUnknownMovingObject.GetObjectClass: TDAOCObjectClass;
begin
  Result := ocUnknown;
end;

end.

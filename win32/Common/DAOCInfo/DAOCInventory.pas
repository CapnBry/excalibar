unit DAOCInventory;

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
  SysUtils, Classes, Contnrs, DAOCClasses;

type
  TDAOCInventoryItem = class(TObject)
  private
    FSlot:  BYTE;
    FCount: integer;
    FDurability: integer;
    FQuality: integer;
    FBonus:   integer;
    FCondition: integer;
    FCountlessDescription: string;
    FDescription: string;
    FColor: BYTE;
    FLevel: BYTE;
    FItemIDMajor: BYTE;
    FItemIDMinor: BYTE;
    FDelveInfo: TStrings;

    procedure SetDescription(const Value: string);
    function GetSlotName: string;
    function SlotAsBag : integer;
    function SlotAsBagPos : integer;
    function GetIsInBag: boolean;
    function SlotAsVaultPage: integer;
    function SlotAsVaultPos: integer;
    procedure SetDelveInfo(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function AsText : string;

    function ClassRestriction : TDAOCCharacterClass;

    property Description: string read FDescription write SetDescription;
    property CountlessDescription: string read FCountlessDescription;
    property Slot: BYTE read FSlot write FSlot;
    property SlotName: string read GetSlotName;
    property Count: integer read FCount write FCount;
    property Condition: integer read FCondition write FCondition;
    property DelveInfo: TStrings read FDelveInfo write SetDelveInfo;
    property Durability: integer read FDurability write FDurability;
    property Quality: integer read FQuality write FQuality;
    property Bonus: integer read FBonus write FBonus;
    property Color: BYTE read FColor write FColor;
    property ItemIDMajor: BYTE read FItemIDMajor write FItemIDMajor;
    property ItemIDMinor: BYTE read FItemIDMinor write FItemIDMinor;
    property Level: BYTE read FLevel write FLevel;
    property IsInBag: boolean read GetIsInBag;
    property BagPage: integer read SlotAsBag;
    property BagItemIndex: integer read SlotAsBagPos;
    property VaultPage: integer read SlotAsVaultPage;
    property VaultItemIndex: integer read SlotAsVaultPos;
  end;

  TDAOCInventoryItemNotify = procedure (Sender: TObject; AItem: TDAOCInventoryItem) of object;

  TDAOCInventory = class(TObjectList)
  protected
    function GetItems(AIndex: integer) : TDAOCInventoryItem;
  public
    function Find(const AItem: string) : TDAOCInventoryItem;
    function FindCountless(const AItem: string) : TDAOCInventoryItem;
    function HasItem(const AItem: string) : boolean;
    function TotalCountOfItem(const AItem: string; AIncludeVault: boolean = false) : integer;
    procedure ClearSlot(ASlotID: BYTE);
    function ItemInSlot(ASlotID: BYTE) : TDAOCInventoryItem;
    function IndexOfSlot(ASlotID: BYTE) : integer;
    function AsText : string;
    procedure TakeItem(AItem: TDAOCInventoryItem);
    function IsFull: boolean;

    property Items[AIndex: integer]: TDAOCInventoryItem read GetItems; default;
  end;

implementation

{ TDAOCInventoryItem }

const
  INV_BAGPOS_FIRST = $28;
  INV_BAGPOS_LAST = $4F;
  INV_VAULTPOS_FIRST = $6E;
  INV_VAULTPOS_LAST = $95;

function TDAOCInventoryItem.AsText: string;
var
  I:    integer;
begin
  Result := Format('  %s (%s) quantity %d'#13#10 +
    '    Con %d%% Dur %d%% Qual %d%% Bon %d%%'#13#10 +
    '    Level %d Color 0x%2.2x ItemID 0x%2.2x %2.2x',
    [FDescription, GetSlotName, FCount, FCondition, FDurability, FQuality,
    FBonus, FLevel, FColor, FItemIDMajor, FItemIDMinor]);

  if Assigned(FDelveInfo) then
    for I := 0 to FDelveInfo.Count - 1 do
      Result := Result + #13#10'    ' + FDelveInfo[I];
end;

function TDAOCInventoryItem.ClassRestriction: TDAOCCharacterClass;
begin
    { DO NOT use Helms to identify a class, because most classes
    use the same helm mesh }
  case FItemIDMajor of
    $00:  Result := ccUnknown;
    $01:
//      case FItemIDMinor of
          // $50:  Result := ccRunemaster;  Helm
          // $51:  Result := ccShadowblade; Helm
          // $b6:  Result := ccHero; Helm
//        else
          Result := ccUnknown;
//      end  { case list 0x01 }

    $02:
      case FItemIDMinor of
        $aa:  Result := ccCabalist;  // chest
        $b0:  Result := ccArmsman;  // chest
        $b1:  Result := ccArmsman;  // leggings
        $b2:  Result := ccArmsman;  // sleeves
        $b3:  Result := ccArmsman;  // gloves
        $b4:  Result := ccArmsman;  // boots
        $b5:  Result := ccPaladin;  // chest
        $b6:  Result := ccPaladin;  // leggings
        $b7:  Result := ccPaladin;  // sleeves
        $b8:  Result := ccPaladin;  // gloves
        $b9:  Result := ccPaladin;  // boots
        $ba:  Result := ccHealer;  // chest
        $bb:  Result := ccHealer;  // leggings
        $bc:  Result := ccHealer;  // sleeves
        $bd:  Result := ccHealer;  // gloves
        $be:  Result := ccHealer;  // boots
        $bf:  Result := ccRunemaster;  // chest
        $c0:  Result := ccRunemaster;  // leggings
        $c1:  Result := ccRunemaster;  // sleeves
        $c2:  Result := ccRunemaster;  // boots
        $c3:  Result := ccRunemaster;  // gloves
        $c4:  Result := ccHero;  // chest
        $c5:  Result := ccHero;  // leggings
        $c6:  Result := ccHero;  // sleeves
        $c7:  Result := ccHero;  // gloves
        $c8:  Result := ccHero;  // boots
        $c9:  Result := ccCleric;  // chest
        $ca:  Result := ccCleric;  // leggings
        $cb:  Result := ccCleric;  // sleeves
        $cc:  Result := ccCleric;  // gloves
        $cd:  Result := ccCleric;  // boots
        $ce:  Result := ccMercenary;  // chest
        $cf:  Result := ccMercenary;  // leggings
        $d0:  Result := ccMercenary;  // sleeves
        $d1:  Result := ccMercenary;  // gloves
        $d2:  Result := ccMercenary;  // boots
        $d3:  Result := ccMinstrel;  // chest
        $d4:  Result := ccMinstrel;  // leggings
        $d5:  Result := ccMinstrel;  // sleeves
        $d6:  Result := ccMinstrel;  // gloves
        $d7:  Result := ccMinstrel;  // boots
        $d8:  Result := ccScout;  // chest
        $d9:  Result := ccScout;  // legs
        $da:  Result := ccScout;  // sleeves
        $db:  Result := ccScout;  // gloves
        $dc:  Result := ccScout;  // boots
        $dd:  Result := ccTheurgist;  // chest
        $de:  Result := ccBard;  // chest
        $df:  Result := ccBard;  // leggings
        $e0:  Result := ccBard;  // sleeves
        $e1:  Result := ccBard;  // gloves
        $e2:  Result := ccBard;  // boots
        $e3:  Result := ccDruid;  // chest
        $e4:  Result := ccDruid;  // leggings
        $e5:  Result := ccDruid;  // sleeves
        $e6:  Result := ccDruid;  // gloves
        $e7:  Result := ccDruid;  // boots
        $e8:  Result := ccEldritch;  // chest
        $e9:  Result := ccMentalist;  // chest
        $ea:  Result := ccNightshade;  // chest
        $eb:  Result := ccNightshade;  // leggings
        $ec:  Result := ccNightshade;  // sleeves
        $ed:  Result := ccNightshade;  // gloves
        $ee:  Result := ccNightshade;  // boots
        $ef:  Result := ccBerserker;  // chest
        $f0:  Result := ccBerserker;  // leggings
        $f1:  Result := ccBerserker;  // sleeves
        $f2:  Result := ccBerserker;  // gloves
        $f3:  Result := ccBerserker;  // boots
        $f4:  Result := ccHunter;  // chest
        $f5:  Result := ccHunter;  // leggings
        $f6:  Result := ccHunter;  // sleeves
        $f7:  Result := ccHunter;  // gloves
        $f8:  Result := ccHunter;  // boots
        $f9:  Result := ccShadowblade;  // chest
        $fa:  Result := ccShadowblade;  // leggings
        $fb:  Result := ccShadowblade;  // sleeves
        $fc:  Result := ccShadowblade;  // gloves
        $fd:  Result := ccShadowblade;  // boots
        $fe:  Result := ccShaman;  // chest
        $ff:  Result := ccShaman;  // leggings
        else
          Result := ccUnknown;
      end;  { case list 0x02 }
    $03:
      case FItemIDMinor of
        $00:  Result := ccShaman;  // sleeves
        $01:  Result := ccShaman;  // gloves
        $02:  Result := ccShaman;  // boots
        $03:  Result := ccSkald;  // chest
        $04:  Result := ccSkald;  // leggings
        $05:  Result := ccSkald;  // sleeves
        $06:  Result := ccSkald;  // gloves
        $07:  Result := ccSkald;  // boots
        $08:  Result := ccWarrior;  // chest
        $09:  Result := ccWarrior;  // leggings
        $0a:  Result := ccWarrior;  // sleeves
        $0b:  Result := ccWarrior;  // gloves
        $0c:  Result := ccWarrior;  // boots
        $0d:  Result := ccEnchanter; // chest
        $0e:  Result := ccBlademaster; // chest
        $0f:  Result := ccBlademaster; // leggings
        $10:  Result := ccBlademaster; // sleeves
        $11:  Result := ccBlademaster; // gloves
        $12:  Result := ccBlademaster; // boots
        $13:  Result := ccThane;  // chest
        $14:  Result := ccThane;  // leggings
        $15:  Result := ccThane;  // sleeves
        $16:  Result := ccThane;  // gloves
        $17:  Result := ccThane;  // boots
        $18:  Result := ccInfiltrator; // chest
        $19:  Result := ccInfiltrator; // leggings
        $1a:  Result := ccInfiltrator; // sleeves
        $1b:  Result := ccInfiltrator; // gloves
        $1c:  Result := ccInfiltrator; // boots
        $1d:  Result := ccFriar; // chest
        $1e:  Result := ccWizard; // chest
        $1f:  Result := ccSpiritmaster; // chest
        $20:  Result := ccSpiritmaster; // leggings
        $21:  Result := ccSpiritmaster; // sleeves
        $22:  Result := ccSpiritmaster; // gloves
        $23:  Result := ccSpiritmaster; // boots
        $24:  Result := ccSorcerer; // chest
        $25:  Result := ccWarden; // chest
        $26:  Result := ccWarden; // leggings
        $27:  Result := ccWarden; // sleeves
        $28:  Result := ccWarden; // gloves
        $29:  Result := ccWarden; // boots
        $2a:  Result := ccChampion; // chest
        $2b:  Result := ccChampion; // leggings
        $2c:  Result := ccChampion; // sleeves
        $2d:  Result := ccChampion; // gloves
        $2e:  Result := ccChampion; // boots
        $2f:  Result := ccRanger; // chest
        $30:  Result := ccRanger; // leggings
        $31:  Result := ccRanger; // sleeves
        $32:  Result := ccRanger; // gloves
        $33:  Result := ccRanger; // boots
        else
          Result := ccUnknown;
      end;  { case list 0x03 }
    else
      Result := ccUnknown;
  end;  { case itemmajor }
end;

constructor TDAOCInventoryItem.Create;
begin
  FCount := 1;
end;

destructor TDAOCInventoryItem.Destroy;
begin
  FDelveInfo.Free;
  inherited;
end;

function TDAOCInventoryItem.GetIsInBag: boolean;
begin
  Result := FSlot in [INV_BAGPOS_FIRST..INV_BAGPOS_LAST];
end;

function TDAOCInventoryItem.GetSlotName: string;
begin
  case FSlot of
    $0a:  Result := 'Left hand';
    $0b:  Result := 'Right hand';
    $0c:  Result := '2h';
    $0d:  Result := 'Ranged';
    $0e:  Result := 'Thrown';
    $15:  Result := 'Head';
    $16:  Result := 'Hands';
    $17:  Result := 'Feet';
    $18:  Result := 'Jewelry';
    $19:  Result := 'Chest';
    $1a:  Result := 'Cloak';
    $1b:  Result := 'Legs';
    $1c:  Result := 'Sleeves';
    $1d:  Result := 'Necklace';
    $20:  Result := 'Belt';
    $21:  Result := 'Left wrist';
    $22:  Result := 'Right wrist';
    $23:  Result := 'Left finger';
    $24:  Result := 'Right finger';
    INV_BAGPOS_FIRST..INV_BAGPOS_LAST:  Result := 'Inventory bag ' +
      IntToStr(SlotAsBag) + ' pos ' + IntToStr(SlotAsBagPos);
    INV_VAULTPOS_FIRST..INV_VAULTPOS_LAST:  Result := 'Vault page ' +
      IntToStr(SlotAsVaultPage) + ' pos ' + IntToStr(SlotAsVaultPos);
    else
      Result := 'slot 0x' + IntToHex(FSlot, 2);
  end;
end;

procedure TDAOCInventoryItem.SetDelveInfo(const Value: TStrings);
{ TAKES the delve information, freeing the old copy.  I say agai:
  This does NOT copy }
begin
  FDelveInfo.Free;
  FDelveInfo := Value;
end;

procedure TDAOCInventoryItem.SetDescription(const Value: string);
begin
  FDescription := Value;
  FCountlessDescription := FDescription;
  while (FCountlessDescription <> '') and (FCountlessDescription[1] in [' ', '0'..'9']) do
    Delete(FCountlessDescription, 1, 1);
end;

function TDAOCInventoryItem.SlotAsBag: integer;
begin
  if FSlot in [INV_BAGPOS_FIRST..INV_BAGPOS_LAST] then
    Result := (FSlot - INV_BAGPOS_FIRST) div 8
  else
    Result := -1;
end;

function TDAOCInventoryItem.SlotAsBagPos: integer;
begin
  if FSlot in [INV_BAGPOS_FIRST..INV_BAGPOS_LAST] then
    Result := (FSlot - INV_BAGPOS_FIRST) mod 8
  else
    Result := -1;
end;

function TDAOCInventoryItem.SlotAsVaultPage: integer;
begin
  if FSlot in [INV_VAULTPOS_FIRST..INV_VAULTPOS_LAST] then
    Result := (FSlot - INV_VAULTPOS_FIRST) div 20
  else
    Result := -1;
end;

function TDAOCInventoryItem.SlotAsVaultPos: integer;
begin
  if FSlot in [INV_VAULTPOS_FIRST..INV_VAULTPOS_LAST] then
    Result := (FSlot - INV_VAULTPOS_FIRST) mod 20
  else
    Result := -1;
end;

{ TDAOCInventory }

function TDAOCInventory.AsText: string;
var
  I:    integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    Result := Result + Items[I].AsText;
    if I < (Count - 1) then
      Result := Result + #13#10;
  end;
end;

procedure TDAOCInventory.ClearSlot(ASlotID: BYTE);
var
  I:    integer;
begin
  I := IndexOfSlot(ASlotID);
  if I <> -1 then
    Delete(I);
end;

function TDAOCInventory.Find(const AItem: string): TDAOCInventoryItem;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AItem, Items[I].Description) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TDAOCInventory.FindCountless(const AItem: string): TDAOCInventoryItem;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AItem, Items[I].CountlessDescription) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TDAOCInventory.IsFull: boolean;
var
  I:  integer;
begin
  for I := INV_BAGPOS_FIRST to INV_BAGPOS_LAST do
    if IndexOfSlot(I) = -1 then begin
      Result := false;
      exit;
    end;

  Result := true;
end;

function TDAOCInventory.GetItems(AIndex: integer): TDAOCInventoryItem;
begin
  Result := TDAOCInventoryItem(inherited Items[AIndex]);
end;

function TDAOCInventory.HasItem(const AItem: string): boolean;
begin
  Result := Assigned(Find(AItem));
end;

function TDAOCInventory.IndexOfSlot(ASlotID: BYTE): integer;
begin
  for Result := 0 to Count - 1 do
    if ASlotID = Items[Result].Slot then
      exit;

  Result := -1;
end;

function TDAOCInventory.ItemInSlot(ASlotID: BYTE): TDAOCInventoryItem;
var
  I:    integer;
begin
  I := IndexOfSlot(ASlotID);
  if I <> -1 then
    Result := Items[I]
  else
    Result := nil;
end;

procedure TDAOCInventory.TakeItem(AItem: TDAOCInventoryItem);
begin
  ClearSlot(AItem.Slot);
    { if the description is blank, we're just clearing the slot }
  if (AItem.ItemIDMajor = 0) and (AItem.ItemIDMinor = 0) then  // if AItem.Description = '' then
    AItem.Free
  else
    Add(AItem);
end;

function TDAOCInventory.TotalCountOfItem(const AItem: string;
  AIncludeVault: boolean = false): integer;
var
  I:    integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if AnsiSameText(AItem, Items[I].CountlessDescription) and
      (AIncludeVault or Items[I].IsInBag) then
      inc(Result, Items[I].Count);
end;

end.

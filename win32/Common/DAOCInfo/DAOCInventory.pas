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
  SysUtils, Classes, Contnrs, DAOCClasses, LinedFileStream, CSVLineParser;

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
    FItemID: WORD;
    FDelveInfo: TStrings;

    procedure SetDescription(const Value: string);
    function GetSlotName: string;
    function SlotAsBag : integer;
    function SlotAsBagPos : integer;
    function GetIsInBag: boolean;
    function SlotAsVaultPage: integer;
    function SlotAsVaultPos: integer;
    function SlotAsMerchantPage: integer;
    function SlotAsMerchantPos: integer;
    procedure SetDelveInfo(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function AsString(AVerbose: boolean) : string;
    function SummaryLine : string;

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
    property ItemID: WORD read FItemID write FItemID;
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
    function AsString(AVerbose: boolean) : string;
    procedure TakeItem(AItem: TDAOCInventoryItem);
    function IsFull: boolean;

    property Items[AIndex: integer]: TDAOCInventoryItem read GetItems; default;
  end;

  TDAOCInventoryItemLookupList = class(TObject)
  private
    FList:  TStringList;
    function IndexOfID(AID: Cardinal) : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function ItemName(AID: Cardinal) : string;
    procedure LoadFromStream(AStrm: TStream);
  end;

implementation

{ TDAOCInventoryItem }

const
  INV_BAGPOS_FIRST = $28;
  INV_BAGPOS_LAST = $4F;
  INV_VAULTPOS_FIRST = $6E;
  INV_VAULTPOS_LAST = $95;
  INV_MERCHANTPOS_FIRST = $96;
  INV_MERCHANTPOS_LAST = $FA;  //??

function TDAOCInventoryItem.AsString(AVerbose: boolean) : string;
var
  I:    integer;
begin
  if AVerbose then begin
    Result := Format('  %s (%s) quantity %d'#13#10 +
      '    Con %d%% Dur %d%% Qual %d%% Bon %d%%'#13#10 +
      '    Level %d Color 0x%2.2x ItemID 0x%4.4x',
      [FDescription, GetSlotName, FCount, FCondition, FDurability, FQuality,
      FBonus, FLevel, FColor, FItemID]);

    if Assigned(FDelveInfo) then
      for I := 0 to FDelveInfo.Count - 1 do
        Result := Result + #13#10'    ' + FDelveInfo[I];
  end
  else
    Result := GetSlotName + ': ' + FDescription;
end;

function TDAOCInventoryItem.ClassRestriction: TDAOCCharacterClass;
begin
    { DO NOT use Helms to identify a class, because most classes
    use the same helm mesh }
  case FItemID of
    $02aa:  Result := ccCabalist;  // chest
    $02b0:  Result := ccArmsman;  // chest
    $02b1:  Result := ccArmsman;  // leggings
    $02b2:  Result := ccArmsman;  // sleeves
    $02b3:  Result := ccArmsman;  // gloves
    $02b4:  Result := ccArmsman;  // boots
    $02b5:  Result := ccPaladin;  // chest
    $02b6:  Result := ccPaladin;  // leggings
    $02b7:  Result := ccPaladin;  // sleeves
    $02b8:  Result := ccPaladin;  // gloves
    $02b9:  Result := ccPaladin;  // boots
    $02ba:  Result := ccHealer;  // chest
    $02bb:  Result := ccHealer;  // leggings
    $02bc:  Result := ccHealer;  // sleeves
    $02bd:  Result := ccHealer;  // gloves
    $02be:  Result := ccHealer;  // boots
    $02bf:  Result := ccRunemaster;  // chest
    $02c0:  Result := ccRunemaster;  // leggings
    $02c1:  Result := ccRunemaster;  // sleeves
    $02c2:  Result := ccRunemaster;  // boots
    $02c3:  Result := ccRunemaster;  // gloves
    $02c4:  Result := ccHero;  // chest
    $02c5:  Result := ccHero;  // leggings
    $02c6:  Result := ccHero;  // sleeves
    $02c7:  Result := ccHero;  // gloves
    $02c8:  Result := ccHero;  // boots
    $02c9:  Result := ccCleric;  // chest
    $02ca:  Result := ccCleric;  // leggings
    $02cb:  Result := ccCleric;  // sleeves
    $02cc:  Result := ccCleric;  // gloves
    $02cd:  Result := ccCleric;  // boots
    $02ce:  Result := ccMercenary;  // chest
    $02cf:  Result := ccMercenary;  // leggings
    $02d0:  Result := ccMercenary;  // sleeves
    $02d1:  Result := ccMercenary;  // gloves
    $02d2:  Result := ccMercenary;  // boots
    $02d3:  Result := ccMinstrel;  // chest
    $02d4:  Result := ccMinstrel;  // leggings
    $02d5:  Result := ccMinstrel;  // sleeves
    $02d6:  Result := ccMinstrel;  // gloves
    $02d7:  Result := ccMinstrel;  // boots
    $02d8:  Result := ccScout;  // chest
    $02d9:  Result := ccScout;  // legs
    $02da:  Result := ccScout;  // sleeves
    $02db:  Result := ccScout;  // gloves
    $02dc:  Result := ccScout;  // boots
    $02dd:  Result := ccTheurgist;  // chest
    $02de:  Result := ccBard;  // chest
    $02df:  Result := ccBard;  // leggings
    $02e0:  Result := ccBard;  // sleeves
    $02e1:  Result := ccBard;  // gloves
    $02e2:  Result := ccBard;  // boots
    $02e3:  Result := ccDruid;  // chest
    $02e4:  Result := ccDruid;  // leggings
    $02e5:  Result := ccDruid;  // sleeves
    $02e6:  Result := ccDruid;  // gloves
    $02e7:  Result := ccDruid;  // boots
    $02e8:  Result := ccEldritch;  // chest
    $02e9:  Result := ccMentalist;  // chest
    $02ea:  Result := ccNightshade;  // chest
    $02eb:  Result := ccNightshade;  // leggings
    $02ec:  Result := ccNightshade;  // sleeves
    $02ed:  Result := ccNightshade;  // gloves
    $02ee:  Result := ccNightshade;  // boots
    $02ef:  Result := ccBerserker;  // chest
    $02f0:  Result := ccBerserker;  // leggings
    $02f1:  Result := ccBerserker;  // sleeves
    $02f2:  Result := ccBerserker;  // gloves
    $02f3:  Result := ccBerserker;  // boots
    $02f4:  Result := ccHunter;  // chest
    $02f5:  Result := ccHunter;  // leggings
    $02f6:  Result := ccHunter;  // sleeves
    $02f7:  Result := ccHunter;  // gloves
    $02f8:  Result := ccHunter;  // boots
    $02f9:  Result := ccShadowblade;  // chest
    $02fa:  Result := ccShadowblade;  // leggings
    $02fb:  Result := ccShadowblade;  // sleeves
    $02fc:  Result := ccShadowblade;  // gloves
    $02fd:  Result := ccShadowblade;  // boots
    $02fe:  Result := ccShaman;  // chest
    $02ff:  Result := ccShaman;  // leggings
    $0300:  Result := ccShaman;  // sleeves
    $0301:  Result := ccShaman;  // gloves
    $0302:  Result := ccShaman;  // boots
    $0303:  Result := ccSkald;  // chest
    $0304:  Result := ccSkald;  // leggings
    $0305:  Result := ccSkald;  // sleeves
    $0306:  Result := ccSkald;  // gloves
    $0307:  Result := ccSkald;  // boots
    $0308:  Result := ccWarrior;  // chest
    $0309:  Result := ccWarrior;  // leggings
    $030a:  Result := ccWarrior;  // sleeves
    $030b:  Result := ccWarrior;  // gloves
    $030c:  Result := ccWarrior;  // boots
    $030d:  Result := ccEnchanter; // chest
    $030e:  Result := ccBlademaster; // chest
    $030f:  Result := ccBlademaster; // leggings
    $0310:  Result := ccBlademaster; // sleeves
    $0311:  Result := ccBlademaster; // gloves
    $0312:  Result := ccBlademaster; // boots
    $0313:  Result := ccThane;  // chest
    $0314:  Result := ccThane;  // leggings
    $0315:  Result := ccThane;  // sleeves
    $0316:  Result := ccThane;  // gloves
    $0317:  Result := ccThane;  // boots
    $0318:  Result := ccInfiltrator; // chest
    $0319:  Result := ccInfiltrator; // leggings
    $031a:  Result := ccInfiltrator; // sleeves
    $031b:  Result := ccInfiltrator; // gloves
    $031c:  Result := ccInfiltrator; // boots
    $031d:  Result := ccFriar; // chest
    $031e:  Result := ccWizard; // chest
    $031f:  Result := ccSpiritmaster; // chest
    $0320:  Result := ccSpiritmaster; // leggings
    $0321:  Result := ccSpiritmaster; // sleeves
    $0322:  Result := ccSpiritmaster; // gloves
    $0323:  Result := ccSpiritmaster; // boots
    $0324:  Result := ccSorcerer; // chest
    $0325:  Result := ccWarden; // chest
    $0326:  Result := ccWarden; // leggings
    $0327:  Result := ccWarden; // sleeves
    $0328:  Result := ccWarden; // gloves
    $0329:  Result := ccWarden; // boots
    $032a:  Result := ccChampion; // chest
    $032b:  Result := ccChampion; // leggings
    $032c:  Result := ccChampion; // sleeves
    $032d:  Result := ccChampion; // gloves
    $032e:  Result := ccChampion; // boots
    $032f:  Result := ccRanger; // chest
    $0330:  Result := ccRanger; // leggings
    $0331:  Result := ccRanger; // sleeves
    $0332:  Result := ccRanger; // gloves
    $0333:  Result := ccRanger; // boots
    else
      Result := ccUnknown;
  end;  { case itemid }
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
    INV_MERCHANTPOS_FIRST..INV_MERCHANTPOS_LAST:  Result := 'Consignment page ' +
      IntToStr(SlotAsMerchantPage) + ' pos ' + IntToStr(SlotAsMerchantPos);
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

function TDAOCInventoryItem.SlotAsMerchantPage: integer;
begin
  if FSlot in [INV_MERCHANTPOS_FIRST..INV_MERCHANTPOS_LAST] then
    Result := (FSlot - INV_MERCHANTPOS_FIRST) div 20
  else
    Result := -1;
end;

function TDAOCInventoryItem.SlotAsMerchantPos: integer;
begin
  if FSlot in [INV_MERCHANTPOS_FIRST..INV_MERCHANTPOS_LAST] then
    Result := (FSlot - INV_MERCHANTPOS_FIRST) mod 20
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

function TDAOCInventoryItem.SummaryLine: string;
{ Like AsText, but all on one line.  Format is:
  Name, Slot, Count, Condition, Durability, Quality, Bonus, Level, Color,
  ItemID, DelveInfoCount, DelveInfo0, DelveInfo1, ... }
var
  I:    integer;
begin
  Result :=Format('"%s",0x%2.2x,%d,%d,%d,%d,%d,%d,%d,0x%4.4x,%d',
    [CountlessDescription, Slot, Count, Condition, Durability, Quality,
     Bonus, Level, Color, ItemID, FDelveInfo.Count]);
  for I := 0 to FDelveInfo.Count - 1 do
    Result := Result + ',"' + FDelveInfo[I] + '"';
end;

{ TDAOCInventory }

function TDAOCInventory.AsString(AVerbose: boolean) : string;
var
  I:    integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    Result := Result + Items[I].AsString(AVerbose);
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
  if AItem.ItemID = 0 then  // if AItem.Description = '' then
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

{ TDAOCInventoryItemLookupList }

procedure TDAOCInventoryItemLookupList.Clear;
begin
  FList.Clear;
end;

constructor TDAOCInventoryItemLookupList.Create;
begin
  inherited;
  FList := TStringList.Create;
end;

destructor TDAOCInventoryItemLookupList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TDAOCInventoryItemLookupList.IndexOfID(AID: Cardinal): integer;
{ quick bin search.  List should be sorted by IID }
var
  L, H: Integer;
  CID:  Cardinal;
begin
  L := 0;
  H := FList.Count - 1;

  while L <= H do begin
    Result := (L + H) shr 1;
    CID := Cardinal(FList.Objects[Result]);
    if CID = AID then
      exit
    else if CID < AID then
      L := Result + 1
    else
      H := Result - 1;
  end;  { while L <= H }

  Result := -1;
end;

function TDAOCInventoryItemLookupList.ItemName(AID: Cardinal): string;
var
  I:    integer;
begin
  I := IndexOfID(AID);
  if I <> -1 then
    Result := FList[I]
  else
    Result := '';
end;

procedure TDAOCInventoryItemLookupList.LoadFromStream(AStrm: TStream);
var
  CSV:  TCSVLineParser;
  FS:   TLinedStreamWrapper;
begin
  Clear;
  FS := TLinedStreamWrapper.Create(AStrm);
  CSV := TCSVLineParser.Create;
  try
      { first two lines are header }
    FS.ReadLn;
    FS.ReadLn;

    while not FS.EOF do begin
      CSV.DataString := FS.ReadLn;

      if CSV.FieldCount < 2 then
        continue;

      FList.AddObject(CSV[1], TObject(CSV.FieldAsInt(0, 0)));
    end;  { while !EOF }
  finally
    FS.Free;
    CSV.Free;
  end;
end;

end.

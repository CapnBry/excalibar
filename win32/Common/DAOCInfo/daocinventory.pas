unit DAOCInventory;

interface

uses
  SysUtils, Contnrs;

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
    FIcon:  BYTE;
    FColor: BYTE;
    FLevel: BYTE;

    procedure SetDescription(const Value: string);
    function GetSlotName: string;
    function SlotAsBag : integer;
    function SlotAsBagPos : integer;
    function GetIsInBag: boolean;
    function SlotAsVaultPage: integer;
    function SlotAsVaultPos: integer;
  public
    constructor Create;
    function AsText : string;


    property Description: string read FDescription write SetDescription;
    property CountlessDescription: string read FCountlessDescription;
    property Slot: BYTE read FSlot write FSlot;
    property SlotName: string read GetSlotName;
    property Count: integer read FCount write FCount;
    property Condition: integer read FCondition write FCondition;
    property Durability: integer read FDurability write FDurability;
    property Quality: integer read FQuality write FQuality;
    property Bonus: integer read FBonus write FBonus;
    property Color: BYTE read FColor write FColor;
    property Icon: BYTE read FIcon write FIcon;
    property Level: BYTE read FLevel write FLevel;
    property IsInBag: boolean read GetIsInBag;
    property BagPage: integer read SlotAsBag;
    property BagItemIndex: integer read SlotAsBagPos;
    property VaultPage: integer read SlotAsVaultPage;
    property VaultItemIndex: integer read SlotAsVaultPos;
  end;

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
begin
  Result := Format('  %s (%s) quantity %d'#13#10 +
    '    Con %d%% Dur %d%% Qual %d%% Bon %d%%'#13#10 +
    '    Level %d Color 0x%2.2x Icon 0x%2.2x',
    [FDescription, GetSlotName, FCount, FCondition, FDurability, FQuality,
    FBonus, FLevel, FColor, FIcon]);
end;

constructor TDAOCInventoryItem.Create;
begin
  FCount := 1;
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
  if AItem.Description = '' then
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

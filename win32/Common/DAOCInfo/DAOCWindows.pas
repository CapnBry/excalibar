unit DAOCWindows;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

(***
  CAVEAT:  These use DAOC 'classic' interface click positions.
    Also, if you move a window, the window positons are not saved
    (and therefore usable) until you exit the game are relogin.
***)

interface

uses
  Windows, Classes, SysUtils, StringParseHlprs, INIFiles, DAOCRegion,
  Recipes;

type
  TNeedClickEvent = procedure (Sender: TObject; X, Y: integer) of object;
  TNeedSendKeysEvent = procedure (Sender: TObject; const AKeys: string) of object;
  TNeedVKEvent = procedure (Sender: TObject; vk: byte) of object;

  TDAOCWindowManager = class(TObject)
  private
    FDAOCPath:  string;
    FServerIP: string;
    FCharacterName: string;
    FOnNeedLeftClick: TNeedClickEvent;
    FOnNeedRightClick: TNeedClickEvent;
    FOnNeedSendKeys: TNeedSendKeysEvent;
    FOnNeedVKDown: TNeedVKEvent;
    FOnNeedVKUp: TNeedVKEvent;
    FUIStyle: string;
    function GetSettingsFile: string;
  protected
    function LastIPOctet: string;
    procedure DoLeftClick(Sender: TObject; X, Y: integer);
    procedure DoRightClick(Sender: TObject; X, Y: integer);
    procedure DoSendKeys(Sender: TObject; const AKeys: string);
    procedure DoVKDown(Sender: TObject; vk: byte);
    procedure DoVKUp(Sender: TObject; vk: byte);
  public
    constructor Create;

    procedure GetAvailableUIStyles(AStyles: TStrings);

    property CharacterName: string read FCharacterName write FCharacterName;
    property DAOCPath: string read FDAOCPath write FDAOCPath;
    property ServerIP: string read FServerIP write FServerIP;
    property SettingsFile: string read GetSettingsFile;
    property UIStyle: string read FUIStyle write FUIStyle;

      { inventory functions }
    procedure MoveInventoryItem(AFromBag, AFromItem, AToBag, AToItem: integer);

    property OnNeedLeftClick: TNeedClickEvent read FOnNeedLeftClick write FOnNeedLeftClick;
    property OnNeedRightClick: TNeedClickEvent read FOnNeedRightClick write FOnNeedRightClick;
    property OnNeedSendKeys: TNeedSendKeysEvent read FOnNeedSendKeys write FOnNeedSendKeys;
    property OnNeedVKUp: TNeedVKEvent read FOnNeedVKUp write FOnNeedVKUp;
    property OnNeedVKDown: TNeedVKEvent read FOnNeedVKDown write FOnNeedVKDown;
  end;

  TDAOCWindow = class(TObject)
  private
    function GetLeft: integer;
    function GetTop: integer;
    function GetVisible: boolean;
  protected
    FSettings:    TStringList;
    FWndManager:  TDAOCWindowManager;
    FWindowName:  string;

    procedure LoadSettingsLine;
    procedure DoLeftClick(X, Y: integer);
    procedure DoRightClick(X, Y: integer);
    procedure DoSendKeys(const AKeys: string);
    procedure DoVKDown(vk: byte);
    procedure DoVKUp(vk: byte);
    function GetUIOffset(const AName: string; ADefault: integer) : integer;
    procedure FixupWindowName;
  public
    constructor Create(AWndManager: TDAOCWindowManager); virtual;
    destructor Destroy; override;

    property WindowName: string read FWindowName;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Visible: boolean read GetVisible;
  end;

  TStatsWindowPage = (swpAttributes, swpInventory, swpSkills, swpCombat, swpSpells, swpGroup, swpNone);

  TStatsWindow = class(TDAOCWindow)
  private
    FTopOffset:   integer;
    FLeftOffset:  integer;
    FButtonHeight:  integer;
    FButtonWidth:   integer;
    FInventoryTop:  integer;
    FInventoryLeft: integer;
    FInventoryItemHeight:  integer;
    FInventoryBagLeft:  integer;
    FInventoryBagTops:  array[0..4] of integer;
    function GetStartingOpenPage: TStatsWindowPage;
  protected
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;

    procedure ClickPage(APage: TStatsWindowPage);
    procedure SelectInventoryItem(AItem: integer);
    procedure SelectInventoryBag(ABag: integer);
    procedure MoveInventoryItem(AFromBag, AFromItem, AToBag, AToItem: integer);

    property StartingOpenPage: TStatsWindowPage read GetStartingOpenPage;
  end;

  TDialogWindow = class(TDAOCWindow)
  private
    FOKTop:   integer;
    FOKLeft:  integer;
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;
    procedure ClickOK;

    class procedure CloseDialog(AWndManager: TDAOCWindowManager);
  end;

  TScrollableListWindow = class(TDAOCWindow)
  protected
    FPage:  integer;
    FItem:  integer;
    FTopItem: integer;
    FScrollSmooth:  boolean;

    FItemsPerPage: integer;
    FScrollLeftOffset: integer;
    FScrollUpTopOffset: integer;
    FScrollDownTopOffset: integer;
    FPageTopOffset: integer;
    FPageLeftLeftOffset:  integer;
    FPageRightLeftOffset: integer;
    FItemTopOffset: integer;
    FItemLeftOffset:  integer;
    FItemHeight:      integer;
    FCloseTopOffset:  integer;
    FLastSelectWasIcon: boolean;

    procedure SelectItemCommon(Value, XOff: integer);
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;

    procedure SetPage(Value: integer; AssumePage0: boolean);
    procedure SelectItem(Value: integer);
    procedure SelectItemIcon(Value: integer);

    procedure PageLeft;
    procedure PageRight;
    procedure ScrollUp;
    procedure ScrollDown;
    procedure Close;
  end;

  TVendorWindow = class(TScrollableListWindow)
  private
    FBuyTop:  integer;
    FBuyLeft: integer;
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;
    procedure Buy;
    procedure BuyMultiple(AQuantity: integer);
    function MBuyQuantity(ACountNeeded, AVendorQuantity: integer) : integer;
  end;

  TQuickbarOrientation = (qboVertical, qboHorizontal, qboClosed);

  TQuickbar = class(TDAOCWindow)
  private
    FOrientation:   TQuickBarOrientation;
    FCenterPerp:    integer;
    FCenterPara:    integer;
    FButtonWidth:   integer;
    procedure GetStartingOrientation;
    procedure SlotXY(ASlot: integer; var X, Y: integer);
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;
    procedure ClickSlot(ASlot: integer);
    procedure ClearSlot(ASlot: integer);
  end;

  TTradeRecipeWindow = class(TScrollableListWindow)
  private
    FExpandedGroupIdx:  integer;
    FItemRecipeLeft:  integer;
    FItemGroupLeft:   integer;
  public
    constructor Create(AWndManager: TDAOCWindowManager); override;
    procedure ExpandGroup(AItem: TTradeSkillRecipe; ACraft: TCraftRecipeCollection;
      ACurrentSkill: integer);
    procedure ClickRecipe(AItem: TTradeSkillRecipe; ACraft: TCraftRecipeCollection;
      ACurrentSkill: integer);
  end;

implementation

{ TDAOCWindowManager }

constructor TDAOCWindowManager.Create;
begin
  FUIStyle := 'Classic';
end;

procedure TDAOCWindowManager.DoLeftClick(Sender: TObject; X, Y: integer);
begin
  if Assigned(FOnNeedLeftClick) then
    FOnNeedLeftClick(Sender, X, Y);
end;

procedure TDAOCWindowManager.DoRightClick(Sender: TObject; X, Y: integer);
begin
  if Assigned(FOnNeedRightClick) then
    FOnNeedRightClick(Sender, X, Y);
end;

procedure TDAOCWindowManager.DoSendKeys(Sender: TObject; const AKeys: string);
begin
  if Assigned(FOnNeedSendKeys) then
    FOnNeedSendKeys(Sender, AKeys);
end;

procedure TDAOCWindowManager.DoVKDown(Sender: TObject; vk: byte);
begin
  if Assigned(FOnNeedVKDown) then
    FOnNeedVKDown(Sender, vk);
end;

procedure TDAOCWindowManager.DoVKUp(Sender: TObject; vk: byte);
begin
  if Assigned(FOnNeedVKDown) then
    FOnNeedVKUp(Sender, vk);
end;

procedure TDAOCWindowManager.GetAvailableUIStyles(AStyles: TStrings);
begin
  with TINIFile.Create(ExtractFilePath(ParamStr(0)) + 'UI.ini') do begin
    ReadSections(AStyles);
    Free;
  end;
end;

function TDAOCWindowManager.GetSettingsFile: string;
begin
  if (FDAOCPath = '') or not DirectoryExists(FDAOCPath) then
    raise Exception.Create('DAOCPath is not set correctly.'#13  +
      'Windows on Dark Age can not be located properly.');

  Result :=
{$IFDEF VER130}
    IncludeTrailingBackslash(FDAOCPath)
{$ELSE}
    IncludeTrailingPathDelimiter(FDAOCPath)
{$ENDIF}
    + FCharacterName + '-' + LastIPOctet + '.ini';
end;

function TDAOCWindowManager.LastIPOctet: string;
var
  iStartPos:    integer;
begin
  iStartPos := 1;
  ParseWord(FServerIP, iStartPos);
  ParseWord(FServerIP, iStartPos);
  ParseWord(FServerIP, iStartPos);
  Result := ParseWord(FServerIP, iStartPos);
end;

procedure TDAOCWindowManager.MoveInventoryItem(AFromBag, AFromItem, AToBag,
  AToItem: integer);
begin
  with TStatsWindow.Create(Self) do begin
    MoveInventoryItem(AFromBag, AFromItem, AToBag, AToItem);
    Free;
  end;
end;

{ TDAOCWindow }

constructor TDAOCWindow.Create(AWndManager: TDAOCWindowManager);
begin
  inherited Create;
  FWndManager := AWndManager;
  FSettings := TStringList.Create;
  FixupWindowName;
  LoadSettingsLine;
end;

destructor TDAOCWindow.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

procedure TDAOCWindow.DoLeftClick(X, Y: integer);
begin
  FWndManager.DoLeftClick(Self, X, Y);
end;

procedure TDAOCWindow.DoRightClick(X, Y: integer);
begin
  FWndManager.DoRightClick(Self, X, Y);
end;

procedure TDAOCWindow.DoSendKeys(const AKeys: string);
begin
  FWndManager.DoSendKeys(Self, AKeys);
end;

procedure TDAOCWindow.DoVKDown(vk: byte);
begin
  FWndManager.DoVKDown(Self, vk);    
end;

procedure TDAOCWindow.DoVKUp(vk: byte);
begin
  FWndManager.DoVKUp(Self, vk);    
end;

function TDAOCWindow.GetLeft: integer;
begin
  if FSettings.Count > 0 then
    Result := StrToIntDef(FSettings[0], 0)
  else
    Result := 0;
end;

function TDAOCWindow.GetTop: integer;
begin
  if FSettings.Count > 1 then
    Result := StrToIntDef(FSettings[1], 0)
  else
    Result := 0;
end;

function TDAOCWindow.GetUIOffset(const AName: string; ADefault: integer): integer;
begin
  with TINIFile.Create(ExtractFilePath(ParamStr(0)) + 'UI.ini') do begin
    Result := ReadInteger(FWndManager.UIStyle, AName, ADefault);
    Free;
  end;  { with }
end;

function TDAOCWindow.GetVisible: boolean;
begin
  if FSettings.Count > 2 then
    Result := FSettings[2] = '1'
  else
    Result := false;
end;

procedure TDAOCWindow.FixupWindowName;
begin
  with TINIFile.Create(ExtractFilePath(ParamStr(0)) + 'UI.ini') do begin
    FWindowName := ReadString(FWndManager.UIStyle, FWindowName + 'WindowName', FWindowName);
    Free;
  end;  { with }
end;

procedure TDAOCWindow.LoadSettingsLine;
var
  s:    string;
begin
  if FWindowName = '' then
    exit;

  with TIniFile.Create(FWndManager.SettingsFile) do begin
    s := ReadString('Panels', FWindowName, '');
    Free;
  end;    { with INI }

  FSettings.CommaText := s;
end;

{ TStatsWindow }

procedure TStatsWindow.ClickPage(APage: TStatsWindowPage);
begin
  DoLeftClick(Left + FTopOffset + (FButtonWidth * ord(APage)) +
    (FButtonWidth div 2), Top + FTopOffset + (FButtonHeight div 2));
end;

constructor  TStatsWindow.Create(AWndManager: TDAOCWindowManager);
begin
  FWindowName := 'Stats';
  inherited Create(AWndManager);

  FTopOffset := GetUIOffset('StatsTop', 1);
  FLeftOffset := GetUIOffset('StatsLeft', 14);
  FButtonHeight := GetUIOffset('StatsButtonWidth', 28);
  FButtonWidth := GetUIOffset('StatsButtonHeight', 32);
  FInventoryTop := GetUIOffset('InventoryTop', 278);
  FInventoryLeft := GetUIOffset('InventoryLeft', 20);
  FInventoryItemHeight := GetUIOffset('InventoryItemHeight', 17);
  FInventoryBagLeft := GetUIOffset('InventoryBagLeft', 180);
  FInventoryBagTops[0] := GetUIOffset('InventoryBagTop1', 291);
  FInventoryBagTops[1] := GetUIOffset('InventoryBagTop2', 318);
  FInventoryBagTops[2] := GetUIOffset('InventoryBagTop3', 343);
  FInventoryBagTops[3] := GetUIOffset('InventoryBagTop4', 368);
  FInventoryBagTops[4] := GetUIOffset('InventoryBagTop5', 393);
end;

function TStatsWindow.GetStartingOpenPage: TStatsWindowPage;
begin
  if FSettings.Count > 3 then
    Result := TStatsWindowPage(StrToIntDef(FSettings[3], 0))
  else
    Result := swpNone;
end;

procedure TStatsWindow.MoveInventoryItem(AFromBag, AFromItem, AToBag,
  AToItem: integer);
begin
    { switch away then back to make sure the window is open }
  ClickPage(swpAttributes);
  sleep(500);
  ClickPage(swpInventory);
  sleep(500);

  SelectInventoryBag(AFromBag);
  sleep(500);
  SelectInventoryItem(AFromItem);
  sleep(500);

  if AFromBag <> AToBag then begin
    SelectInventoryBag(AToBag);
    sleep(500);
  end;

  SelectInventoryItem(AToItem);
  sleep(1000);

  ClickPage(swpInventory);
end;

procedure TStatsWindow.SelectInventoryBag(ABag: integer);
(*** Assumes we've already ClickPage(swpInventory) ***)
begin
  if not (ABag in [0..4]) then
    exit;

  DoLeftClick(Left + FInventoryBagLeft, Top + FInventoryBagTops[ABag]);
end;

procedure TStatsWindow.SelectInventoryItem(AItem: integer);
(*** Assumes we've already ClickPage(swpInventory) ***)
begin
  if not (AItem in [0..7]) then
    exit;

  DoLeftClick(Left + FInventoryLeft, Top + FInventoryTop + (AItem * (FInventoryItemHeight)) +
    (FInventoryItemHeight div 2));
end;

{ TDialogWindow }

procedure TDialogWindow.ClickOK;
begin
  DoLeftClick(Left + FOKLeft, Top + FOKTop);
end;

class procedure TDialogWindow.CloseDialog(AWndManager: TDAOCWindowManager);
var
  frm:  TDialogWindow;
begin
  frm := TDialogWindow.Create(AWndManager);
  frm.ClickOK;
  frm.Free;
end;

constructor TDialogWindow.Create(AWndManager: TDAOCWindowManager);
begin
  FWindowName := 'Dialog';
  inherited Create(AWndManager);

  FOKTop := GetUIOffset('DialogOKTop', 85);
  FOKLeft := GetUIOffset('DialogOKLeft', 125);
end;

{ TVendorWindow }

procedure TVendorWindow.Buy;
begin
  DoLeftClick(Left + FBuyLeft, Top + FBuyTop);
end;

constructor TVendorWindow.Create(AWndManager: TDAOCWindowManager);
begin
  FWindowName := 'Merchant';
  inherited Create(AWndManager);

  FItemsPerPage := GetUIOffset('MerchantItemsPerPage', 20);
  FScrollLeftOffset := GetUIOffset('MerchantScrollbarLeft', 345);
  FScrollUpTopOffset := GetUIOffset('MerchantScrollbarUpTop', 25);
  FScrollDownTopOffset := GetUIOffset('MerchantScrollbarDownTop', 370);
  FScrollSmooth := GetUIOffset('MerchantScrollSmooth', 0) = 1;
  FPageTopOffset := GetUIOffset('MerchantPageTop', 430);
  FPageLeftLeftOffset := GetUIOffset('MerchantPageLeftLeft', 258);
  FPageRightLeftOffset := GetUIOffset('MerchantPageRightLeft', 278);
  FItemTopOffset := GetUIOffset('MerchantItemTop', 25);
  FItemLeftOffset := GetUIOffset('MerchantItemLeft', 15);
  FItemHeight := GetUIOffset('MerchantItemHeight', 18);
  FCloseTopOffset := GetUIOffset('MerchantCloseTop', 5);

  FBuyTop := GetUIOffset('MerchantBuyTop', 430);
  FBuyLeft := GetUIOffset('MerchantBuyLeft', 41);
end;

procedure TVendorWindow.BuyMultiple(AQuantity: integer);
(*** Remember that multi buy buys IN STACKS, so if the vendor sells stacks
  of 20, and you BuyMultiple 20, you're gonna get 400.
  Also, you need to select the icon using SelectItemIcon, not SelectItem ***)
begin
  if not FLastSelectWasIcon then
    SelectItemIcon(FItem);
  DoSendKeys('/mbuy ' + IntToStr(AQuantity) + '[cr]');

    { since /mbuy resets the selection (icon) }
  FLastSelectWasIcon := false;
end;

function TVendorWindow.MBuyQuantity(ACountNeeded, AVendorQuantity: integer) : integer;
begin
  if ACountNeeded = 0 then
    Result := 0
  else
    Result := (ACountNeeded + AVendorQuantity - 1) div AVendorQuantity;

    { can only buy 100 at a time }
  if Result > 100 then
    Result := 100;
end;


{ TTradeRecipeWindow }

procedure TTradeRecipeWindow.ClickRecipe(AItem: TTradeSkillRecipe;
  ACraft: TCraftRecipeCollection; ACurrentSkill: integer);
var
  iGroupIdx:    integer;
//  iSubGroupIdx: integer;
//  iTierIdx:     integer;
  iAdjustedOrdinal:  integer;
begin
  ExpandGroup(AItem, ACraft, ACurrentSkill);
  sleep(500);

  iGroupIdx := ACraft.OrdinalOfGroup(AItem.Group);
  if iGroupIdx = -1 then
    exit;

  iAdjustedOrdinal := ACraft.AdjustOrdinalInGroupForSkill(AItem, ACurrentSkill);
  if iAdjustedOrdinal = -1 then
    exit;

  FItemLeftOffset := FItemRecipeLeft;
  SelectItem(iGroupIdx + 1 + iAdjustedOrdinal);
(***
  iTierIdx := ACraft.OrdinalOfTierInGroup(AItem.Group, AItem.Tier);
  if iTierIdx = -1 then
    exit;

  iSubTierIdx := ACraft.OrdinalOfItemInTier(AItem.Group, AItem.Tier, AItem.SkillLevel);

  FItemLeftOffset := FItemRecipeLeft;
  SelectItem(iGroupIdx + 1 + iTierIdx + iSubTierIdx);
**)
end;

constructor TTradeRecipeWindow.Create(AWndManager: TDAOCWindowManager);
begin
  FWindowName := 'TradeSkillWindow';
  inherited Create(AWndManager);

  FExpandedGroupIdx := -1;

  //FPageTopOffset := GetUIOffset('', 430);
  //FPageLeftLeftOffset := GetUIOffset('', 258);
  //FPageRightLeftOffset := GetUIOffset('', 278);
  
  FItemsPerPage := GetUIOffset('TradeskillItemsPerPage', 21); // actually 22, but the last one on every page is the same as the first on the next
  FScrollLeftOffset := GetUIOffset('TradeskillScrollbarLeft', 295);
  FScrollUpTopOffset := GetUIOffset('TradeskillScrollbarUpTop', 25);
  FScrollDownTopOffset := GetUIOffset('TradeskillScrollbarDownTop', 370);
  FItemTopOffset := GetUIOffset('TradeskillItemTop', 25);
  FItemRecipeLeft := GetUIOffset('TradeskillItemLeft', 15);
  FItemGroupLeft := GetUIOffset('TradeskillItemGroupLeft', 25);
  FItemHeight := GetUIOffset('TradeskillItemHeight', 18);
  FCloseTopOffset := GetUIOffset('TradeskillCloseTop', 5);
end;

procedure TTradeRecipeWindow.ExpandGroup(AItem: TTradeSkillRecipe;
  ACraft: TCraftRecipeCollection; ACurrentSkill: integer);
var
  iGroupIdx: integer;
  iVisibleItemCount:  integer;
begin
  iGroupIdx := ACraft.OrdinalOfGroup(AItem.Group);
  if iGroupIdx <> -1 then begin
    FItemLeftOffset := FItemGroupLeft;
    SelectItem(iGroupIdx);
    FExpandedGroupIdx := iGroupIdx;

      { check to see if we scrolled up when we expanded }
    iVisibleItemCount := ACraft.VisibleRecipesInGroup(AItem.Group, ACurrentSkill);
    if (FExpandedGroupIdx - FTopItem) + iVisibleItemCount >= FItemsPerPage then begin
      FTopItem := FExpandedGroupIdx + iVisibleItemCount + 1 - FItemsPerPage;
      sleep(200);
    end;
  end;  { if group index }
end;

{ TQuickbar }

procedure TQuickbar.ClearSlot(ASlot: integer);
var
  X, Y:   integer;
begin
  if not ASlot in [1..10] then
    exit;
  SlotXY(ASlot, X, Y);

  DoVKDown(VK_SHIFT);
  sleep(200);
  DoRightClick(X, Y);
  sleep(200);
  DoVKUp(VK_SHIFT);
end;

procedure TQuickbar.ClickSlot(ASlot: integer);
var
  X, Y:   integer;
begin
  if not ASlot in [1..10] then
    exit;
  SlotXY(ASlot, X, Y);
  DoLeftClick(X, Y);
end;

constructor TQuickbar.Create(AWndManager: TDAOCWindowManager);
begin
  FWindowName := 'Quickbar';
  inherited Create(AWndManager);
  GetStartingOrientation;

  FCenterPerp := GetUIOffset('QuickbarCenterPerp', 25);
  FCenterPara := GetUIOffset('QuickbarCenterPara', 45);
  FButtonWidth := GetUIOffset('QuickbarButtonWidth', 34);
end;

procedure TQuickbar.GetStartingOrientation;
begin
  if FSettings.Count > 3 then
    FOrientation := TQuickbarOrientation(StrToIntDef(FSettings[3], 0))
  else
    FOrientation := qboClosed;
end;

procedure TQuickbar.SlotXY(ASlot: integer; var X, Y: integer);
begin
  case FOrientation of
    qboVertical:
      begin
        X := Left + FCenterPerp;
        Y := Top + FCenterPara + (FButtonWidth * (ASlot-1));
      end;
    qboHorizontal:
      begin
        X := Top + FCenterPara + (FButtonWidth * (ASlot-1));
        Y := Left + FCenterPerp;
      end;
  end;  { case orientation }
end;

{ TScrollableListWindow }

procedure TScrollableListWindow.Close;
begin
  DoLeftClick(Left + FScrollLeftOffset, Top + FCloseTopOffset);
end;

constructor TScrollableListWindow.Create(AWndManager: TDAOCWindowManager);
begin
  inherited Create(AWndManager);

  FPage := -1;
  FItem := -1;
end;

procedure TScrollableListWindow.PageLeft;
begin
  DoLeftClick(Left + FPageLeftLeftOffset, Top + FPageTopOffset);
  dec(FPage);

  if FScrollSmooth then
    FTopItem := 0;
end;

procedure TScrollableListWindow.PageRight;
begin
  DoLeftClick(Left + FPageRightLeftOffset, Top + FPageTopOffset);
  inc(FPage);

  if FScrollSmooth then
    FTopItem := 0;
end;

procedure TScrollableListWindow.ScrollDown;
begin
  DoLeftClick(Left + FScrollLeftOffset, Top + FScrollDownTopOffset);
end;

procedure TScrollableListWindow.ScrollUp;
begin
  DoLeftClick(Left + FScrollLeftOffset, Top + FScrollUpTopOffset);
end;

procedure TScrollableListWindow.SelectItem(Value: integer);
begin
  FLastSelectWasIcon := false;
  SelectItemCommon(Value, 15);
end;

procedure TScrollableListWindow.SelectItemCommon(Value, XOff: integer);
begin
//  if FItem = Value then
//    exit;

    { if we don't know where we are, assume we're scrolled all the way up }
  if FItem = -1 then
    FItem := 0;

    { if we're too far down, scroll up }
  while Value < FTopItem do begin
    ScrollUp;
    sleep(200);
    if FScrollSmooth then
      dec(FTopItem, 1)
    else
      dec(FTopItem, FItemsPerPage);
  end;

  if FTopItem < 0 then
    FTopItem := 0;

    { if we're too far up, scroll down }
  while Value >= (FTopItem + FItemsPerPage) do begin
    ScrollDown;
    sleep(200);
    if FScrollSmooth then
      inc(FTopItem, 1)
    else
      inc(FTopItem, FItemsPerPage);
  end;

  DoLeftClick(Left + FItemLeftOffset + XOff, Top + FItemTopOffset +
    ((Value - FTopItem) * FItemHeight));
  FItem := Value;
end;

procedure TScrollableListWindow.SelectItemIcon(Value: integer);
begin
  FLastSelectWasIcon := true;
  SelectItemCommon(Value, 0);
end;

procedure TScrollableListWindow.SetPage(Value: integer;
  AssumePage0: boolean);
begin
  if FPage = Value then
    exit;

    { if we don't know what page we're on, scroll left until we know we're on 1 }
  if FPage = -1 then begin
      { if AssumePage0 is set, we assume this window just popped up and
        we're on page 0 }
    if AssumePage0 then
      FPage := 0
    else
      FPage := 5;
    while FPage > 0 do begin
      PageLeft;
      sleep(200);
    end;
  end;

  while FPage <> Value do begin
    if FPage > Value then
      PageLeft
    else
      PageRight;
    sleep(200);
  end;
end;

end.

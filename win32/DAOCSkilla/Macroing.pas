unit Macroing;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DAOCConnection, DAOCControl, DAOCControlList,
  PowerSkill, ExtCtrls, DAOCPlayerAttributes, MapNavigator;

type
  TfrmMacroing = class(TForm)
    btnPowerskillBuy: TSpeedButton;
    Label1: TLabel;
    chkAutosell: TCheckBox;
    btnMacroTradeskill: TButton;
    btnAFK: TButton;
    btnTellMacro: TButton;
    btnSpellcraftHlp: TButton;
    btnShowMapModes: TButton;
    tmrTimeoutDelay: TTimer;
    btnLowOnStat: TButton;
    Label2: TLabel;
    cbxConnectionList: TComboBox;
    Label3: TLabel;
    lblMacroState: TLabel;
    cbxUIStyle: TComboBox;
    Label4: TLabel;
    procedure btnShowMapModesClick(Sender: TObject);
    procedure btnPowerskillBuyClick(Sender: TObject);
    procedure chkAutosellClick(Sender: TObject);
    procedure btnMacroTradeskillClick(Sender: TObject);
    procedure btnAFKClick(Sender: TObject);
    procedure btnTellMacroClick(Sender: TObject);
    procedure btnSpellcraftHlpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrTimeoutDelayTimer(Sender: TObject);
    procedure btnLowOnStatClick(Sender: TObject);
    procedure cbxConnectionListChange(Sender: TObject);
    procedure cbxUIStyleChange(Sender: TObject);
  private
    FAutoSell:      boolean;
    FInSellOff:     boolean;
    FInAutoBuy:     boolean;
    FPSItemList:    TPowerSkillItemList;
    FSellingBeforeBuying:   boolean;
    FSelectingNPC:          boolean;
    FTrinketList:   TStringList;
    FDAOCControlList: TDAOCControlList;
    FCurrConn:        TDAOCControl;

    procedure DoAutoSell;
    procedure DoAutoBuy;
    function CheckConflictingWindows(AWindowList: array of const) : boolean;
    function CheckGoForMorePSMaterials : boolean;
    procedure Log(const s: string);
    procedure ArrivedAtForge(Sender: TObject; AParm: Cardinal);
    procedure ArrivedAtMerchant(Sender: TObject; AParm: Cardinal);
    procedure OpenMacroTradeSkillWindow;
    procedure InventoryChangeComplete;
    procedure VendorChangeComplete;
    function IsAtForgeNode : boolean;
    function IsAtMerchantNode : boolean;
    function NoMerchantNode : boolean;
    function GetTrinketList: string;
    procedure SetTrinketList(const Value: string);
    procedure SetDAOCControlList(const Value: TDAOCControlList);
    procedure AutoSelectConnection;
    procedure SetCurrentConnection(AConn: TDAOCControl);
    procedure UpdateConnCbx;
    procedure ShowMacroState(const s: string);
    function FindNearestMerchantNodeIdx : integer;
    procedure UpdateUIStyleCbx;
  protected
    function NoForgeNode : boolean;
  public
    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCInventoryChanged(Sender: TObject);
    procedure DAOCVendorWindow(Sender: TObject);
    procedure DAOCPathChanged(Sender: TObject);
    procedure DAOCStopAllActions(Sender: TObject);
    procedure DAOCSkillLevelChanged(Sender: TObject; AItem: TDAOCNameValuePair);
    procedure DAOCArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
    procedure DAOCSelectNPCSuccess(Sender: TObject);
    procedure DAOCSelectNPCFailed(Sender: TObject);
    procedure DAOCAttemptNPCRightClickFailed(Sender: TObject);
    procedure DAOCLocalHealthUpdate(Sender: TObject);
    procedure DAOCTradeskillCapped(Sender: TObject);

    property DAOCControlList: TDAOCControlList read FDAOCControlList write SetDAOCControlList;
    property TrinketList: string read GetTrinketList write SetTrinketList;
  end;

var
  frmMacroing: TfrmMacroing;

implementation

uses
  ShowMapNodes, PowerSkillSetup, SpellcraftHelp, MacroTradeSkill,
  AFKMessage, DAOCWindows, DAOCInventory, Unit1, StringParseHlprs,
  LowOnStat
{$IFDEF DAOC_AUTO_SERVER}
  ,TellMacro
{$ENDIF DAOC_AUTO_SERVER}
  ;

{$R *.dfm}

const
  TIMEOUT_INVENTORYCHANGECOMPLETE = 1;
  TIMEOUT_VENDORCHANGECOMPLETE = 2;

procedure TfrmMacroing.btnShowMapModesClick(Sender: TObject);
begin
  if not Assigned(FCurrConn) then
    exit;
    
  if frmShowMapNodes.Visible then
    frmShowMapNodes.Close
  else
    frmShowMapNodes.Show;
  frmShowMapNodes.ShowMapNodes(FCurrConn.MapNodes);
end;

procedure TfrmMacroing.btnPowerskillBuyClick(Sender: TObject);
begin
  frmPowerSkill.PSItemList := FPSItemList;

  if not CheckConflictingWindows([frmSpellcraftHelp]) then
    exit;

  if frmPowerSkill.Visible then
    frmPowerSkill.Close
  else
    frmPowerSkill.Show;
end;

procedure TfrmMacroing.chkAutosellClick(Sender: TObject);
begin
  FAutoSell := chkAutoSell.Checked;
end;

procedure TfrmMacroing.btnMacroTradeskillClick(Sender: TObject);
begin
//  if not CheckConflictingWindows([frmSpellcraftHelp]) then
//    exit;

  if frmMacroTradeSkills.Visible then
    frmMacroTradeSkills.Close
  else
    frmMacroTradeSkills.Show;
end;

procedure TfrmMacroing.btnAFKClick(Sender: TObject);
begin
  if frmAFK.Visible then
    frmAFK.Close
  else
    frmAFK.Show;
end;

procedure TfrmMacroing.btnTellMacroClick(Sender: TObject);
begin
{$IFDEF DAOC_AUTO_SERVER}
  if frmTellMacro.Visible then
    frmTellMacro.Close
  else
    frmTellMacro.Show;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMacroing.btnSpellcraftHlpClick(Sender: TObject);
begin
  if not CheckConflictingWindows([frmPowerskill]) then
    exit;

  if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.Close
  else
    frmSpellcraftHelp.Show;
end;

procedure TfrmMacroing.FormCreate(Sender: TObject);
begin
  FPSItemList := TPowerSkillItemList.Create;

  FTrinketList := TStringList.Create;
  FTrinketList.Add('*hinge');

{$IFNDEF DAOC_AUTO_SERVER}
  btnTellMacro.Visible := false;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMacroing.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrinketList);
  FreeAndNil(FPSItemList);
end;

procedure TfrmMacroing.DoAutoSell;
var
  I:    integer;
  pWnd: TStatsWindow;
  iCnt: integer;
  iTrinket:     integer;
  pFirstItem:   TDAOCInventoryItem;

  procedure ItemMatch;
  begin
    inc(iCnt);
    if not Assigned(pFirstItem) then
      pFirstItem := FCurrConn.LocalPlayer.Inventory.Items[I];
  end;

begin
  if not FAutoSell or (FCurrConn.SelectedID = 0) then begin
    FInSellOff := false;
    exit;
  end;

  ShowMacroState('Checking autosell...');
  
  iCnt := 0;
  pFirstItem := nil;
  with FCurrConn.LocalPlayer.Inventory do
    for I := 0 to Count - 1 do
      if Items[I].IsInBag and (Items[I].Quality <> 100) then begin
        if Assigned(FPSItemList.Find(Items[I].CountlessDescription)) then begin
          ItemMatch;
          continue;
        end;

        for iTrinket := 0 to FTrinketList.Count - 1 do
          if WildMatch(FTrinketList[iTrinket], LowerCase(Items[I].Description)) then begin
            ItemMatch;
            break;
          end;
      end;  { If in bag and not 100%q }

  if (FInSellOff or (iCnt >= Random(7) + 1)) and Assigned(pFirstItem) then begin
    ShowMacroState('Autoselling ' + pFirstItem.CountlessDescription);

    // Log('InvChg: Found item - ' + Items[I].Description);
    pWnd := TStatsWindow.Create(FCurrConn.WindowManager);
    pWnd.SelectInventoryBag(pFirstItem.BagPage);
    sleep(200);
    pWnd.SelectInventoryItem(pFirstItem.BagItemIndex);
    pWnd.Free;
    sleep(500);
    FCurrConn.DoSendKeys(FCurrConn.KeyQuickSell);
    FInSellOff := iCnt > 0;
    exit;
  end;

  FInSellOff := false;
end;

procedure TfrmMacroing.DoAutoBuy;
var
  bWasAutoSell:  boolean;
  iIdx:          integer;
begin
  if not (frmPowerskill.Visible or frmSpellcraftHelp.Visible) then begin
    FInAutoBuy := false;
    FSellingBeforeBuying := false;
    exit;
  end;

    { prevent reentrance }
  if FInAutoBuy then
    exit;

  bWasAutoSell := FAutoSell;
  FAutoSell := false;
  FInAutoBuy := true;

  if frmPowerskill.Visible then begin
    ShowMacroState('Performing autobuy');

    frmPowerskill.ExecutePurchases;

    if (NoMerchantNode or IsAtMerchantNode) and
      FPSItemList.AutoDeselectMerchant then
      FCurrConn.DoSendKeys('[esc]');

    if frmPowerskill.KeepBuying then begin
        { if we have a list of merchant nodes, find the index of the closest one
          and try to go to the next one after this }
      iIdx := FindNearestMerchantNodeIdx;
      if (iIdx <> -1) and
        (iIdx < (FPSItemList.MerchantNodeCount - 1)) then begin
        FPSItemList.MerchantNodeIndex := FPSItemList.MerchantNodeIndex + 1;
        ShowMacroState('Heading to merchant node ' + FPSItemList.MerchantNodeName);
        FCurrConn.PathToNodeName(FPSItemList.MerchantNodeName);
      end

        { otherwise, return to the forge }
      else begin
        if FPSItemList.MerchantNodeCount > 0 then
          FPSItemList.MerchantNodeIndex := 0;
        ShowMacroState('Heading to forge');
        FCurrConn.PathToNodeName(FPSItemList.ForgeNodeName);
      end;
    end  { if keepbuying }
    else begin
      Log('User aborted buying via STOP command.');
      ShowMacroState('');
    end;
  end  // if frmPowerskill

  else if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.ExecutePurchases

  else
    ShowMacroState('');

  FAutoSell := bWasAutoSell;
  FInAutoBuy := false;
  FSellingBeforeBuying := false;
  Log('Purchase complete');
end;

function TfrmMacroing.CheckConflictingWindows(AWindowList: array of const): boolean;
var
  I:    integer;
  sVisWindows:  string;
begin
  sVisWindows := '';
  for I := low(AWindowList) to high(AWindowList) do
    if TForm(AWindowList[I].VObject).Visible then
      sVisWindows := sVisWindows + #9 + TForm(AWindowList[I].VObject).Caption + #13;

  if sVisWindows = '' then begin
    Result := true;
    exit;
  end;

  Result := MessageDlg('That function conflicts with the operation of the following windows:'#13 +
    sVisWindows +
    'If you continue, those windows will be closed.',
    mtWarning, [mbOk, mbCancel], 0) = mrOK;

  if Result then
    for I := low(AWindowList) to high(AWindowList) do
      if TForm(AWindowList[I].VObject).Visible then
        TForm(AWindowList[I].VObject).Close;
end;

function TfrmMacroing.CheckGoForMorePSMaterials : boolean;
{ Returns true if it is heading for more PSMaterials }
begin
  Result := false;

  if frmPowerskill.Visible then begin
      { If we have materials we don't need to go for more }
    if frmPowerskill.HasMaterialsForItem then
      exit;

    if IsAtForgeNode then begin
        { reset the merchant index because we're just starting out }
      if FPSItemList.MerchantNodeCount > 0 then
        FPSItemList.MerchantNodeIndex := 0;

      ShowMacroState('No materials, going to merchant node ' + FPSItemList.MerchantNodeName);
      FCurrConn.PathToNodeName(FPSItemList.MerchantNodeName);
      Result := true;
    end
    else
      ShowMacroState('No materials, but not at forge.');
  end;  { if frmPowerskill }
end;

procedure TfrmMacroing.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmMacroing.DAOCInventoryChanged(Sender: TObject);
begin
  if not Visible or (Sender <> FCurrConn) then
    exit;

  // ShowMacroState('Waiting for inventory change to complete');

  // Log('Setting timer to TIMEOUT_INVENTORYCHANGECOMPLETE');
  tmrTimeoutDelay.Tag := TIMEOUT_INVENTORYCHANGECOMPLETE;
  tmrTimeoutDelay.Enabled := false;
  tmrTimeoutDelay.Enabled := true;
end;

procedure TfrmMacroing.tmrTimeoutDelayTimer(Sender: TObject);
begin
  tmrTimeoutDelay.Enabled := false;

  case tmrTimeoutDelay.Tag of
    TIMEOUT_INVENTORYCHANGECOMPLETE:
      InventoryChangeComplete;
    TIMEOUT_VENDORCHANGECOMPLETE:
      VendorChangeComplete;
  end;
end;

procedure TfrmMacroing.DAOCVendorWindow(Sender: TObject);
begin
  if not Visible or (Sender <> FCurrConn) then
    exit;

  ShowMacroState('Waiting for complete vendor item list to load');

  // Log('Setting timer to TIMEOUT_VENDORCHANGECOMPLETE');
  tmrTimeoutDelay.Tag := TIMEOUT_VENDORCHANGECOMPLETE;
  tmrTimeoutDelay.Enabled := false;
  tmrTimeoutDelay.Enabled := true;
end;

procedure TfrmMacroing.DAOCPathChanged(Sender: TObject);
begin
  if Sender <> FCurrConn then
    exit;

  if frmShowMapNodes.Visible then
    frmShowMapNodes.PathChanged(FCurrConn.CurrentPath);
end;

procedure TfrmMacroing.DAOCStopAllActions(Sender: TObject);
begin
  if Sender <> FCurrConn then
    exit;

  ShowMacroState('');

  FInSellOff := false;
  FSellingBeforeBuying := false;
  FSelectingNPC := false;
  
  if FPSItemList.MerchantNodeCount > 0 then
    FPSItemList.MerchantNodeIndex := 0;
  if frmPowerskill.Visible then
    frmPowerskill.KeepBuying := false;
  if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.KeepBuying := false;
end;

procedure TfrmMacroing.DAOCSkillLevelChanged(Sender: TObject; AItem: TDAOCNameValuePair);
begin
  if Sender <> FCurrConn then
    exit;
    
  if frmPowerskill.Visible then
    frmPowerskill.SkillLevelChanged(AItem);
end;

procedure TfrmMacroing.DAOCArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
begin
  if Sender <> FCurrConn then
    exit;

    { ANode will not be assigned if we're arriving at a non-pathed destination }
  if Assigned(ANode) and frmPowerskill.Visible then begin
    if ANode.IsNamed(FPSItemList.ForgeNodeName) then begin
      ShowMacroState('At forge.  Waiting 5 seconds');
        { we wait 5s so we shouldn't get a "You move and cancel..." message }
      FCurrConn.ScheduleCallback(5000, ArrivedAtForge, 0);
    end;

    if ANode.IsNamed(FPSItemList.MerchantNodeName) then begin
      ShowMacroState('At merchant. Waiting 5 seconds');
      FCurrConn.ScheduleCallback(5000, ArrivedAtMerchant, 0);
    end;
  end;  { if Node and PowerSkill.Visible }
end;

procedure TfrmMacroing.ArrivedAtForge(Sender: TObject; AParm: Cardinal);
begin
  if Sender <> FCurrConn then
    exit;

  if frmPowerskill.Visible then begin
    ShowMacroState('At forge');
    Log('I am at the forge');

    if FPSItemList.AutoQuickbarSlot <> 0 then begin
      ShowMacroState('Putting recipe in quickbar slot');

      frmPowerskill.RecipeToQuickbar(FPSItemList.AutoQuickbarSlot);
      if FPSItemList.AutoQuickbarSlot = 10 then
        frmMacroTradeSkills.Progression := '0'
      else
        frmMacroTradeSkills.Progression := IntToStr(FPSItemList.AutoQuickbarSlot);
    end;

    if FPSItemList.AutoStartProgression then begin
      ShowMacroState('Crafting...');

      OpenMacroTradeSkillWindow;
      frmMacroTradeSkills.StartProgression;
     end;
  end;
end;

procedure TfrmMacroing.ArrivedAtMerchant(Sender: TObject; AParm: Cardinal);
begin
  if Sender <> FCurrConn then
    exit;

  if frmPowerskill.Visible then begin
    ShowMacroState('Attempting to select merchant...');
    Log('I am at the merchant');
    
    FSelectingNPC := true;
    frmPowerskill.SelectItemMaterialMerchant;
  end;
end;

procedure TfrmMacroing.OpenMacroTradeSkillWindow;
begin
  if not frmMacroTradeSkills.Visible then
    frmMacroTradeSkills.Show;
end;

function TfrmMacroing.IsAtForgeNode: boolean;
var
  pNearestNode:   TMapNode;
begin
  pNearestNode := FCurrConn.NodeClosestToPlayerPos;
  Result := frmPowerskill.Visible and Assigned(pNearestNode) and
    pNearestNode.IsNamed(FPSItemList.ForgeNodeName);
end;

function TfrmMacroing.IsAtMerchantNode: boolean;
var
  pNearestNode:   TMapNode;
begin
  pNearestNode := FCurrConn.NodeClosestToPlayerPos;
  Result := frmPowerskill.Visible and Assigned(pNearestNode) and
    pNearestNode.IsNamed(FPSItemList.MerchantNodeName);
end;

procedure TfrmMacroing.InventoryChangeComplete;
begin
//  if FAutoSell or FInSellOff or FSellingBeforeBuying then
//    ShowMacroState('Inventory change complete');
  
    { most important thing is to start/complete the autosell if we have to.
      This can occur at both the forge or the merchant }
  DoAutoSell;

  if not FInSellOff then
      { if we're FSellingBeforeBuying, then we should already be there with the
        merchant window open }
    if FSellingBeforeBuying then
      DoAutoBuy
    else
      CheckGoForMorePSMaterials;
end;

procedure TfrmMacroing.VendorChangeComplete;
begin
  ShowMacroState('Vendor change complete');

    { force a sell off before we start, selloff should reset to false
      even if we don't have autosell on }
  FInSellOff := true;
  DoAutoSell;

  if FInSellOff then
      { set a flag to remind us to buy after autosell is complete }
    FSellingBeforeBuying := true
  else
      { if autosell isn't going, then kick off the autobuy }
    DoAutoBuy;
end;

procedure TfrmMacroing.DAOCSelectNPCSuccess(Sender: TObject);
begin
  if Visible and (Sender = FCurrConn) and FSelectingNPC then begin
    ShowMacroState('Selected merchant, attempting right-click...');

    FSelectingNPC := false;
    FCurrConn.Stick;
    FCurrConn.AttemptNPCRightClick;
  end;
end;

procedure TfrmMacroing.DAOCSelectNPCFailed(Sender: TObject);
begin
  if Sender = FCurrConn then begin
    ShowMacroState('Could not select merchant!');
    FSelectingNPC := false;
  end;
end;

procedure TfrmMacroing.DAOCAttemptNPCRightClickFailed(Sender: TObject);
begin
  if Visible and (Sender = FCurrConn) then begin
    ShowMacroState('Right-click failed, moving around...');
      { srafe right a bit and try again }
    FCurrConn.StrafeRight(500);
    ArrivedAtMerchant(nil, 0);
  end;
end;

procedure TfrmMacroing.btnLowOnStatClick(Sender: TObject);
begin
  if frmLowOnStat.Visible then
    frmLowOnStat.Close
  else
    frmLowOnStat.Show;
end;

procedure TfrmMacroing.DAOCLocalHealthUpdate(Sender: TObject);
begin
  if Sender <> FCurrConn then
    exit;

  if frmLowOnStat.Visible then
    frmLowOnStat.DAOCLocalHealthUpdate;
end;

function TfrmMacroing.NoForgeNode: boolean;
begin
  Result := FPSItemList.ForgeNodeName = '';
end;

function TfrmMacroing.NoMerchantNode: boolean;
begin
  Result := FPSItemList.MerchantNodeName = '';
end;

function TfrmMacroing.GetTrinketList: string;
begin
  Result := FTrinketList.CommaText;
end;

procedure TfrmMacroing.SetTrinketList(const Value: string);
begin
  FTrinketList.CommaText := Value;
end;

procedure TfrmMacroing.SetDAOCControlList(const Value: TDAOCControlList);
begin
  FDAOCControlList := Value;
  AutoSelectConnection;
end;

procedure TfrmMacroing.AutoSelectConnection;
var
  iActiveIdx:   integer;
begin
    { if theres any active connections in this list, then select the last one }
  for iActiveIdx := FDAOCControlList.Count - 1 downto 0 do
    if FDAOCControlList[iActiveIdx].Active then begin
      SetCurrentConnection(FDAOCControlList[iActiveIdx]);
      exit;
    end;

    { else clear the current connection, because there isn't one }
  if Assigned(FCurrConn) then
    SetCurrentConnection(nil);
end;

procedure TfrmMacroing.SetCurrentConnection(AConn: TDAOCControl);
begin
//  if FCurrConn = AConn then
//    exit;

  FCurrConn := AConn;

  if Assigned(FCurrConn) then begin
    btnShowMapModes.Enabled := true;
    btnPowerskillBuy.Enabled := true;
    btnMacroTradeskill.Enabled := true;
    btnAFK.Enabled := true;
    btnTellMacro.Enabled := true;
    btnLowOnStat.Enabled := true;
    btnSpellcraftHlp.Enabled := true;
    chkAutosell.Enabled := true;

    if cbxUIStyle.Text <> '' then
      FCurrConn.WindowManager.UIStyle := cbxUIStyle.Text;
  end

    { no connection.  Clean up }
  else begin
    btnShowMapModes.Enabled := false;
    btnPowerskillBuy.Enabled := false;
    btnMacroTradeskill.Enabled := false;
    btnAFK.Enabled := false;
    btnTellMacro.Enabled := false;
    btnLowOnStat.Enabled := false;
    btnSpellcraftHlp.Enabled := false;
    chkAutosell.Enabled := false;

    frmPowerskill.Close;
    frmMacroTradeSkills.Close;
    frmAFK.Close;
    frmLowOnStat.Close;
    frmSpellcraftHelp.Close;
  end;

  ShowMacroState('');
  frmPowerskill.DAOCControl := FCurrConn;
  frmMacroTradeSkills.DAOCControl := FCurrConn;
  frmAFK.DAOCControl := FCurrConn;
  frmLowOnStat.DAOCControl := FCurrConn;
  frmSpellcraftHelp.DAOCControl := FCurrConn;
end;

procedure TfrmMacroing.DAOCConnect(Sender: TObject);
begin
  if not Assigned(FCurrConn) then
    AutoSelectConnection;
  UpdateUIStyleCbx;
  UpdateConnCbx;
end;

procedure TfrmMacroing.DAOCDisconnect(Sender: TObject);
begin
  if FCurrConn = Sender then
    AutoSelectConnection;
  UpdateConnCbx;
end;

procedure TfrmMacroing.UpdateConnCbx;
var
  I:      integer;
  iSel:   integer;
begin
  cbxConnectionList.Items.Clear;

  iSel := -1;
  if Assigned(FDAOCControlList) then
    for I := 0 to FDAOCControlList.Count - 1 do
      if FDAOCControlList[I].Active then begin
        if FDAOCControlList[I] = FCurrConn then
          iSel := I;
        cbxConnectionList.Items.Add(FDAOCControlList[I].ServerIP);
      end;  { if active }

  if iSel <> -1 then
    cbxConnectionList.ItemIndex := iSel;
end;

procedure TfrmMacroing.cbxConnectionListChange(Sender: TObject);
begin
  if Assigned(FDAOCControlList) then
    SetCurrentConnection(FDAOCControlList[cbxConnectionList.ItemIndex]);
end;

procedure TfrmMacroing.ShowMacroState(const s: string);
begin
  if s = '' then
    lblMacroState.Visible := false
  else begin
    lblMacroState.Caption := s;
    lblMacroState.Visible := true;
    lblMacroState.Update;
  end;
end;

function TfrmMacroing.FindNearestMerchantNodeIdx: integer;
var
  pNode:    TMapNode;
  iDist:    integer;
  iMinDist: integer;
  I:        integer;
begin
  Result := -1;
  iMinDist := 0;  // to prevent compiler warning

  for I := 0 to FPSItemList.MerchantNodeNames.Count - 1 do begin
    pNode := FCurrConn.MapNodes.NodeByName(FPSItemList.MerchantNodeNames[I]);
    if Assigned(pNode) then begin
      iDist := pNode.Distance2D(FCurrConn.LocalPlayer.X, FCurrConn.LocalPlayer.Y);
      if (I = 0) or (iDist < iMinDist) then begin
        iMinDist := iDist;
        Result := I;
      end;
    end;  { if Node }
  end;  { for I to merchantcount }
end;

procedure TfrmMacroing.UpdateUIStyleCbx;
var
  iIdx:   integer;
begin
  if not Assigned(FCurrConn) then
    exit;

  FCurrConn.WindowManager.GetAvailableUIStyles(cbxUIStyle.Items);
  iIdx := cbxUIStyle.Items.IndexOf(FCurrConn.WindowManager.UIStyle);
  if iIdx <> -1 then
    cbxUIStyle.ItemIndex := iIdx
  else if cbxUIStyle.Items.Count > 0 then
    cbxUIStyle.ItemIndex := 0;
end;

procedure TfrmMacroing.cbxUIStyleChange(Sender: TObject);
begin
  if Assigned(FCurrConn) and (cbxUIStyle.Text <> '') then
    FCurrConn.WindowManager.UIStyle := cbxUIStyle.Text;
end;

procedure TfrmMacroing.DAOCTradeskillCapped(Sender: TObject);
begin
  if Sender <> FCurrConn then
    exit;
  frmMacroTradeSkills.DAOCTradeskillCapped(Sender);
end;

end.

unit Macroing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DAOCControl, PowerSkill, ExtCtrls, DAOCPlayerAttributes,
  MapNavigator;

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
  private
    FDControl: TDAOCControl;
    FAutoSell:      boolean;
    FInSellOff:     boolean;
    FInAutoBuy:     boolean;
    FPSItemList:    TPowerSkillItemList;
    FSellingBeforeBuying:   boolean;

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
  public
    procedure DAOCInventoryChanged;
    procedure DAOCVendorWindow;
    procedure DAOCPathChanged;
    procedure DAOCStopAllActions;
    procedure DAOCSkillLevelChanged(AItem: TDAOCNameValuePair);
    procedure DAOCArriveAtGotoDest;
    procedure DAOCSelectNPCSuccess;

    property DAOCControl: TDAOCControl read FDControl write FDControl;
  end;

var
  frmMacroing: TfrmMacroing;

implementation

uses
  ShowMapNodes, PowerSkillSetup, SpellcraftHelp, MacroTradeSkill,
  AFKMessage, DAOCWindows, DAOCInventory, Unit1
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
  if frmShowMapNodes.Visible then
    frmShowMapNodes.Close
  else
    frmShowMapNodes.Show;
  frmShowMapNodes.ShowMapNodes(FDControl.MapNodes);
end;

procedure TfrmMacroing.btnPowerskillBuyClick(Sender: TObject);
begin
  frmPowerSkill.DAOCControl := FDControl;
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
  frmMacroTradeSkills.DAOCControl := FDControl;

//  if not CheckConflictingWindows([frmSpellcraftHelp]) then
//    exit;

  if frmMacroTradeSkills.Visible then
    frmMacroTradeSkills.Close
  else
    frmMacroTradeSkills.Show;
end;

procedure TfrmMacroing.btnAFKClick(Sender: TObject);
begin
  frmAFK.DAOCControl := FDControl;
  if frmAFK.Visible then
    frmAFK.Close
  else
    frmAFK.Show;
end;

procedure TfrmMacroing.btnTellMacroClick(Sender: TObject);
begin
{$IFDEF DAOC_AUTO_SERVER}
  frmTellMacro.DAOCControl := FDControl;
  if frmTellMacro.Visible then
    frmTellMacro.Close
  else
    frmTellMacro.Show;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMacroing.btnSpellcraftHlpClick(Sender: TObject);
begin
  frmSpellcraftHelp.DAOCControl := FDControl;

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
{$IFNDEF DAOC_AUTO_SERVER}
  btnTellMacro.Visible := false;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMacroing.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPSItemList);
end;

procedure TfrmMacroing.DoAutoSell;
var
  I:    integer;
  pWnd: TStatsWindow;
  iCnt: integer;
  pFirstItem:   TDAOCInventoryItem;
begin
  if not FAutoSell or (FDControl.SelectedID = 0) then begin
    FInSellOff := false;
    exit;
  end;

  iCnt := 0;
  pFirstItem := nil;
  with FDControl.LocalPlayer.Inventory do
    for I := 0 to Count - 1 do
      if Items[I].IsInBag and (Items[I].Quality <> 100) and
        (Assigned(FPSItemList.Find(Items[I].CountlessDescription)) or
        (Pos('cloak', Items[I].Description) > 0) or
        (Pos('hinge', Items[I].Description) > 0)) then begin
        inc(iCnt);
        if not Assigned(pFirstItem) then
          pFirstItem := Items[I];
      end;  { If match }

  if (FInSellOff or (iCnt >= Random(7) + 1)) and Assigned(pFirstItem) then begin
    // Log('InvChg: Found item - ' + Items[I].Description);
    pWnd := TStatsWindow.Create(FDControl.WindowManager);
    pWnd.SelectInventoryBag(pFirstItem.BagPage);
    sleep(200);
    pWnd.SelectInventoryItem(pFirstItem.BagItemIndex);
    pWnd.Free;
    sleep(500);
    FDControl.DoSendKeys(FDControl.KeyQuickSell);
    FInSellOff := iCnt > 0;
    exit;
  end;

  FInSellOff := false;
end;

procedure TfrmMacroing.DoAutoBuy;
var
  bWasAutoSell:   boolean;
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
    frmPowerskill.ExecutePurchases;

    if FPSItemList.AutoDeselectMerchant then
      FDControl.DoSendKeys('[esc]');
      
    if frmPowerskill.KeepBuying and IsAtMerchantNode then
      FDControl.PathToNodeName(FPSItemList.ForgeNodeName);
  end

  else if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.ExecutePurchases;

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
      FDControl.PathToNodeName(FPSItemList.MerchantNodeName);
      Result := true;
    end;
  end;  { if frmPowerskill }
end;


procedure TfrmMacroing.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmMacroing.DAOCInventoryChanged;
begin
  if not Visible then
    exit;

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

procedure TfrmMacroing.DAOCVendorWindow;
begin
  if not Visible then
    exit;

  // Log('Setting timer to TIMEOUT_VENDORCHANGECOMPLETE');
  tmrTimeoutDelay.Tag := TIMEOUT_VENDORCHANGECOMPLETE;
  tmrTimeoutDelay.Enabled := false;
  tmrTimeoutDelay.Enabled := true;
end;

procedure TfrmMacroing.DAOCPathChanged;
begin
  if frmShowMapNodes.Visible then
    frmShowMapNodes.PathChanged(FDControl.CurrentPath);
end;

procedure TfrmMacroing.DAOCStopAllActions;
begin
  if frmPowerskill.Visible then
    frmPowerskill.KeepBuying := false;
  if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.KeepBuying := false;
end;

procedure TfrmMacroing.DAOCSkillLevelChanged(AItem: TDAOCNameValuePair);
begin
  if frmPowerskill.Visible then
    frmPowerskill.SkillLevelChanged(AItem);
end;

procedure TfrmMacroing.DAOCArriveAtGotoDest;
begin
  if frmPowerskill.Visible and frmMacroTradeSkills.Visible then begin
    if IsAtForgeNode then
        { we wait 5s so we shouldn't get a "You move and cancel..." message }
      FDControl.ScheduleCallback(5000, ArrivedAtForge, 0);

    if IsAtMerchantNode then
      FDControl.ScheduleCallback(5000, ArrivedAtMerchant, 0);
  end;
end;

procedure TfrmMacroing.ArrivedAtForge(Sender: TObject; AParm: Cardinal);
begin
  Log('I am at the forge');

  if frmPowerskill.Visible then begin
    if FPSItemList.AutoQuickbarSlot <> 0 then begin
      frmPowerskill.RecipeToQuickbar(FPSItemList.AutoQuickbarSlot);
      frmMacroTradeSkills.Progression := IntToStr(FPSItemList.AutoQuickbarSlot);
    end;

    if FPSItemList.AutoStartProgression then begin
      OpenMacroTradeSkillWindow;
      frmMacroTradeSkills.StartProgression;
     end;
  end;
end;

procedure TfrmMacroing.ArrivedAtMerchant(Sender: TObject; AParm: Cardinal);
begin
  if not Visible then
    exit;
    
  Log('I am at the merchant');
  if frmPowerskill.Visible then
    frmPowerskill.SelectItemMaterialMerchant;
end;

procedure TfrmMacroing.OpenMacroTradeSkillWindow;
begin
  if not frmMacroTradeSkills.Visible then begin
    frmMacroTradeSkills.DAOCControl := FDControl;
    frmMacroTradeSkills.Show;
  end;
end;

function TfrmMacroing.IsAtForgeNode: boolean;
var
  pNearestNode:   TMapNode;
begin
  pNearestNode := FDControl.NodeClosestToPlayerPos;
  Result := frmPowerskill.Visible and Assigned(pNearestNode) and
    pNearestNode.IsNamed(FPSItemList.ForgeNodeName);
end;

function TfrmMacroing.IsAtMerchantNode: boolean;
var
  pNearestNode:   TMapNode;
begin
  pNearestNode := FDControl.NodeClosestToPlayerPos;
  Result := frmPowerskill.Visible and Assigned(pNearestNode) and
    pNearestNode.IsNamed(FPSItemList.MerchantNodeName);
end;

procedure TfrmMacroing.InventoryChangeComplete;
begin
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

procedure TfrmMacroing.DAOCSelectNPCSuccess;
begin
  FDControl.DoSendKeys('/stick');
  FDControl.AttemptNPCRightClick;
end;

end.

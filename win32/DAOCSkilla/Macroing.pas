unit Macroing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DAOCControl, PowerSkill, ExtCtrls, DAOCPlayerAttributes;

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
    FPSItemList:    TPowerSkillItemList;

    procedure DoAutoSell;
    procedure DoAutoBuy;
    function CheckConflictingWindows(AWindowList: array of const) : boolean;
    procedure CheckNeedMorePSMaterials;
    procedure Log(const s: string);
  public
    procedure DAOCInventoryChanged;
    procedure DAOCVendorWindow;
    procedure DAOCPathChanged;
    procedure DAOCStopAllActions;
    procedure DAOCSkillLevelChanged(AItem: TDAOCNameValuePair);

    property DAOCControl: TDAOCControl read FDControl write FDControl;
  end;

var
  frmMacroing: TfrmMacroing;

implementation

uses ShowMapNodes, PowerSkillSetup, SpellcraftHelp, MacroTradeSkill,
  AFKMessage, TellMacro, DAOCWindows, DAOCInventory, Unit1;

{$R *.dfm}

const
  TIMEOUT_AUTOSELL = 1;
  TIMEOUT_AUTOBUY = 2;

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
  if FDControl.SelectedID = 0 then begin
//    Log('InvChg: Timer popped with noone selected');
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
    // emo1.Lines.Add('InvChg: Found item - ' + Items[I].Description);
    pWnd := TStatsWindow.Create(FDControl.WindowManager);
    pWnd.SelectInventoryBag(pFirstItem.BagPage);
    sleep(200);
    pWnd.SelectInventoryItem(pFirstItem.BagItemIndex);
    pWnd.Free;
    sleep(500);
    FDControl.DoSendKeys('[f2]');
    FInSellOff := iCnt > 0;
    exit;
  end;

  FInSellOff := false;
//  Log('InvChg: could not find N items');
  CheckNeedMorePSMaterials;
end;

procedure TfrmMacroing.DoAutoBuy;
var
  bWasAutoSell:   boolean;
begin
  bWasAutoSell := FAutoSell;
  FAutoSell := false;

  if frmPowerskill.Visible then begin
    frmPowerskill.ExecutePurchases;

    if frmPowerskill.KeepBuying and
      AnsiSameText(FDControl.NodeClosestToPlayerPos.Name, FPSItemList.MerchantNodeName) then
      FDControl.PathToNodeName(FPSItemList.ForgeNodeName);
  end

  else if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.ExecutePurchases;

  FAutoSell := bWasAutoSell;
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

procedure TfrmMacroing.CheckNeedMorePSMaterials;
begin
  if frmPowerskill.Visible then begin
    if not AnsiSameText(FDControl.NodeClosestToPlayerPos.Name, FPSItemList.ForgeNodeName) then
      exit;

    if not frmPowerskill.HasMaterialsForItem then
      FDControl.PathToNodeName(FPSItemList.MerchantNodeName)
  end;
end;


procedure TfrmMacroing.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmMacroing.DAOCInventoryChanged;
begin
  if not (Visible and FAutoSell) then
    exit;

  if FDControl.SelectedID = 0 then
    exit;

  tmrTimeoutDelay.Tag := TIMEOUT_AUTOSELL;
  // Log('Setting timer to TIMEOUT_AUTOSELL');
  tmrTimeoutDelay.Enabled := false;
  tmrTimeoutDelay.Enabled := true;
end;

procedure TfrmMacroing.tmrTimeoutDelayTimer(Sender: TObject);
begin
  tmrTimeoutDelay.Enabled := false;

  case tmrTimeoutDelay.Tag of
    TIMEOUT_AUTOSELL:
      DoAutoSell;
    TIMEOUT_AUTOBUY:
      DoAutoBuy;
  end;
end;

procedure TfrmMacroing.DAOCVendorWindow;
begin
  if frmPowerskill.Visible or frmSpellcraftHelp.Visible then begin
    Log('Setting timer to TIMEOUT_AUTOBUY');
    tmrTimeoutDelay.Tag := TIMEOUT_AUTOBUY;
    tmrTimeoutDelay.Enabled := false;
    tmrTimeoutDelay.Enabled := true;
  end;
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

end.

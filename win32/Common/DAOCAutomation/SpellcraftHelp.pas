unit SpellcraftHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, INIFiles, DAOCControl, Recipes, VendorItems,
  DAOCObjs, Buttons, DAOCWindows, DAOCPlayerAttributes, Menus;

type
  TSCHelpGemInfo = record
    Name:       string;
    SCPoints:   double;
    Cost:       integer;
  end;

  TfrmSpellcraftHelp = class(TForm)
    cbxType1: TComboBox;
    cbxPlus1: TComboBox;
    cbxEffect1: TComboBox;
    cbxRealm: TComboBox;
    Label1: TLabel;
    lblGemName1: TLabel;
    cbxType2: TComboBox;
    cbxPlus2: TComboBox;
    cbxEffect2: TComboBox;
    cbxType3: TComboBox;
    cbxPlus3: TComboBox;
    cbxEffect3: TComboBox;
    cbxType4: TComboBox;
    cbxPlus4: TComboBox;
    cbxEffect4: TComboBox;
    lblGemName2: TLabel;
    lblGemName3: TLabel;
    lblGemName4: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblTotalSCCost: TLabel;
    btnQuickbars: TSpeedButton;
    lblTotalCost: TLabel;
    memMaterials: TMemo;
    pumMaterialsList: TPopupMenu;
    mniRefreshMaterialsList: TMenuItem;
    pumSCHelpMain: TPopupMenu;
    mniSaveToFile: TMenuItem;
    mniLoadFromFile: TMenuItem;
    dlgSCFilename: TOpenDialog;
    chkQuantity: TCheckBox;
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbxRealmChange(Sender: TObject);
    procedure cbxPlusChange(Sender: TObject);
    procedure cbxEffectChange(Sender: TObject);
    procedure btnQuickbarsClick(Sender: TObject);
    procedure mniRefreshMaterialsListClick(Sender: TObject);
    procedure mniSaveToFileClick(Sender: TObject);
    procedure mniLoadFromFileClick(Sender: TObject);
    procedure chkQuantityClick(Sender: TObject);
  private
    FGemEffects:  TStringList;
    FSCGems:      array[1..4] of TSCHelpGemInfo;
    FMaterialReqList:   TMaterialReqList;
    FTotalSCCost: integer;
    FDControl:    TDAOCControl;
    FTotalCost:   integer;
    FKeepBuying: boolean;
    procedure ResetGem(cbxType: TComboBox);
    function Plusses(AType: Integer) : TStrings;
    function PlusAsInt(AType, ATier: Integer) : integer;
    function HasEffects(AType: Integer) : boolean;
    function Effects(AType: Integer) : TStrings;
    function LoadGemEffects(const ARealm: string) : boolean;
    procedure ClearGemEffects;
    procedure UpdateGemName(AIndex: integer);
    function CalcSCPoints(AType, ATier: integer) : double;
    procedure UpdateTotalCosts;
    procedure UpdateGemCost(AIndex: integer);
    function GetCraftRealm : TCraftRealm;
    function FindRecipeForGem(AIndex: integer) : TTradeskillRecipe;
    function GetSpellcraftingRecipeList : TCraftRecipeCollection;
    procedure SetCraftRealm(const Value: TCraftRealm);
    procedure UpdateTotalMaterials;
    procedure Log(const s: string);
  public
    procedure ExecutePurchases;
    
    property DAOCControl: TDAOCControl read FDControl write FDControl;
    property CraftRealm: TCraftRealm read GetCraftRealm write SetCraftRealm;
    property KeepBuying: boolean read FKeepBuying write FKeepBuying;
  end;

var
  frmSpellcraftHelp: TfrmSpellcraftHelp;

implementation

{$R *.dfm}

uses
  Unit1;

procedure TfrmSpellcraftHelp.cbxTypeChange(Sender: TObject);
var
  cbxType:    TComboBox;
  cbxPlus:    TComboBox;
  cbxEffect:  TComboBox;
  iType:      integer;
  I:    integer;
begin
  cbxType := TComboBox(Sender);
  if cbxType = cbxType1 then begin
    cbxPlus := cbxPlus1;
    cbxEffect := cbxEffect1;
  end
  else if cbxType = cbxType2 then begin
    cbxPlus := cbxPlus2;
    cbxEffect := cbxEffect2;
  end
  else if cbxType = cbxType3 then begin
    cbxPlus := cbxPlus3;
    cbxEffect := cbxEffect3;
  end
  else if cbxType = cbxType4 then begin
    cbxPlus := cbxPlus4;
    cbxEffect := cbxEffect4;
  end
  else
    exit;

  iType := cbxType.ItemIndex;
  cbxPlus.Items.Clear;
  cbxEffect.Items.Clear;

  if iType < FGemEffects.Count then begin
    with Plusses(iType) do
      for I := 0 to Count - 1 do
        cbxPlus.Items.Add(Values[Names[I]]);
    if cbxPlus.Items.Count > 0 then
      cbxPlus.ItemIndex := 0;

    if HasEffects(iType) then
      with Effects(iType) do
        for I := 0 to Count - 1 do
          cbxEffect.Items.Add(Names[I]);
    if cbxEffect.Items.Count > 0 then
      cbxEffect.ItemIndex := 0;
  end;

  UpdateGemName(cbxType.Tag);
end;

procedure TfrmSpellcraftHelp.FormCreate(Sender: TObject);
begin
  FGemEffects := TStringList.Create;
  FMaterialReqList := TMaterialReqList.Create;
  FMaterialReqList.OwnsObjects := false;
end;

procedure TfrmSpellcraftHelp.FormDestroy(Sender: TObject);
begin
  ClearGemEffects;
  FMaterialReqList.Free;
  FGemEffects.Free;
end;

procedure TfrmSpellcraftHelp.cbxRealmChange(Sender: TObject);
begin
  if cbxRealm.ItemIndex = -1 then
    exit;

  LoadGemEffects(cbxRealm.Text);
  ResetGem(cbxType1);
  ResetGem(cbxType2);
  ResetGem(cbxType3);
  ResetGem(cbxType4);
end;

function TfrmSpellcraftHelp.LoadGemEffects(const ARealm: string) : boolean;
var
  I:    integer;
  INI:  TINIFile;
  sFileName:  string;
  sSection:   string;
  slEffects:  TStringList;
  slPlusses:  TStringList;
begin
  ClearGemEffects;
  sFileName := ExtractFilePath(ParamStr(0)) + ARealm + '_SC.ini';
  if not FileExists(sFileName) then begin
    raise Exception.Create('No spellcraft definition file: ' + ARealm + '_SC.ini');
    Result := false;
    exit;
  end;

  INI := TINIFile.Create(sFileName);
  with INI do begin
    for I := 0 to 6 do begin
      sSection := 'Type' + IntToStr(I);
      slEffects := TStringList.Create;
      slPlusses := TStringList.Create;
      FGemEffects.AddObject(sSection, slPlusses);

      ReadSectionValues(sSection + '_Effects', slEffects);
      ReadSectionValues(sSection, slPlusses);

      if slPlusses.Count = 0 then
        slEffects.Free
      else
        slPlusses.Objects[0] := slEffects;
    end;  { for I }

    Free;
  end;  { with INI }

  Result := true;
end;

procedure TfrmSpellcraftHelp.ClearGemEffects;
var
  I:      integer;
begin
  for I := 0 to FGemEffects.Count - 1 do begin
      { Must free the effects before plusses }
    if HasEffects(I) then
      Effects(I).Free;
    Plusses(I).Free;
  end;

  FGemEffects.Clear;
end;

procedure TfrmSpellcraftHelp.ResetGem(cbxType: TComboBox);
begin
  cbxType.ItemIndex := 0;
  cbxTypeChange(cbxType);
end;

function TfrmSpellcraftHelp.Plusses(AType: Integer): TStrings;
begin
  if AType = -1 then
    Result := nil
  else
    Result := TStringList(FGemEffects.Objects[AType]);
end;

function TfrmSpellcraftHelp.HasEffects(AType: Integer): boolean;
begin
  Result := Plusses(AType).Count > 0;
end;

function TfrmSpellcraftHelp.Effects(AType: Integer): TStrings;
begin
  Result := TStringList(Plusses(AType).Objects[0]);
end;

procedure TfrmSpellcraftHelp.UpdateGemName(AIndex: integer);
var
  cbxType:    TComboBox;
  cbxPlus:    TComboBox;
  cbxEffect:  TComboBox;
  lbl:    TLabel;
  sTier:  string;
  sGem:   string;
  iType:  integer;
begin
  case AIndex of
    1:
      begin
        lbl := lblGemName1;
        cbxType := cbxType1;
        cbxPlus := cbxPlus1;
        cbxEffect := cbxEffect1;
      end;
    2:
      begin
        lbl := lblGemName2;
        cbxType := cbxType2;
        cbxPlus := cbxPlus2;
        cbxEffect := cbxEffect2;
      end;
    3:
      begin
        lbl := lblGemName3;
        cbxType := cbxType3;
        cbxPlus := cbxPlus3;
        cbxEffect := cbxEffect3;
      end;
    4:
      begin
        lbl := lblGemName4;
        cbxType := cbxType4;
        cbxPlus := cbxPlus4;
        cbxEffect := cbxEffect4;
      end;
    else
      exit;
  end;  { case AIndex }

  iType := cbxType.ItemIndex;
  FSCGems[AIndex].SCPoints := CalcSCPoints(iType, cbxPlus.ItemIndex + 1);

  if cbxPlus.ItemIndex <> -1 then
    sTier := Plusses(iType).Names[cbxPlus.ItemIndex]
  else
    sTier := '';

  sGem := '';
  if (cbxEffect.ItemIndex <> -1) and HasEffects(iType) then
    with Effects(iType) do
      sGem := Values[Names[cbxEffect.ItemIndex]];

  FSCGems[AIndex].Name := Trim(sTier + ' ' + sGem);
  UpdateGemCost(AIndex);
  with FSCGems[AIndex] do
    lbl.Caption := Format('%s (%.1f mVal) %s', [Name, SCPoints, CopperToStr(Cost)]);

  UpdateTotalCosts;
  lblTotalSCCost.Caption := Format('%d points used', [FTotalSCCost]);
  lblTotalCost.Caption := CopperToStr(FTotalCost);

  UpdateTotalMaterials;
end;

procedure TfrmSpellcraftHelp.cbxPlusChange(Sender: TObject);
begin
  UpdateGemName(TComboBox(Sender).Tag);
end;

procedure TfrmSpellcraftHelp.cbxEffectChange(Sender: TObject);
begin
  UpdateGemName(TComboBox(Sender).Tag);
end;

function TfrmSpellcraftHelp.CalcSCPoints(AType, ATier: integer) : double;
begin
  Result := 0;

  case AType of
    0:    // Unused
      Result := 0;
    1:    // Stat
        Result := PlusAsInt(AType, ATier) * 2 / 3;
    2:    // Resist
      if ATier = 1 then
        Result := 0.5
      else
        Result := (PlusAsInt(AType, ATier) - 1) * 2;
    3:    // Hits
        Result := PlusAsInt(AType, ATier) / 4;
    4:    // Power
      if ATier = 1 then
        Result := 0.5
      else
        Result := (PlusAsInt(AType, ATier) - 1) * 2;
    5:    // Focus
      Result := 0;
    6:    // Skill
      if ATier = 1 then
        Result := 0.5
      else
        Result := (PlusAsInt(AType, ATier) - 1) * 5;
  end;
end;

procedure TfrmSpellcraftHelp.UpdateTotalCosts;
var
  I:    integer;
  dMax:   double;
  dTotal: double;
begin
  dMax := 0;
  dTotal := 0;
  FTotalCost := 0;

  for I := 1 to 4 do begin
    inc(FTotalCost, FSCGems[I].Cost);
    if FSCGems[I].SCPoints > dMax then
      dMax := FSCGems[I].SCPoints;
    dTotal := dTotal + FSCGems[I].SCPoints;
  end;

  dTotal := (dTotal + dMax) / 2;
  FTotalSCCost := trunc(dTotal);
end;

function TfrmSpellcraftHelp.PlusAsInt(AType, ATier: Integer): integer;
begin
  with Plusses(AType) do
    Result := StrToIntDef(Values[Names[ATier-1]], 0);
end;

procedure TfrmSpellcraftHelp.UpdateGemCost(AIndex: integer);
var
  pItem:  TTradeSkillRecipe;
  pVendorItems: TDAOCVendorItemList;
  I:      integer;
  iCost:  integer;
begin
  if not Assigned(FDControl) or (FSCGems[AIndex].Name = '') then begin
    FSCGems[AIndex].Cost := 0;
    exit;
  end;

  iCost := 0;
  pItem := FindRecipeForGem(AIndex);

//FDControl.MasterVendorList.LoadFromFile('region101.vend');

  if Assigned(pItem) then
    for I := 0 to pItem.Materials.Count - 1 do begin
      pVendorItems := FDControl.MasterVendorList.FindItemVendor(
        pItem.Materials[I].Name);
      if Assigned(pVendorItems) then
        inc(iCost, pVendorItems.CostOfItemQ1(pItem.Materials[I].Name) * pItem.Materials[I].Count);
    end;  { for I to materials.count }

  FSCGems[AIndex].Cost := iCost;
end;

function TfrmSpellcraftHelp.GetCraftRealm: TCraftRealm;
begin
  Result := crNone;
  case cbxRealm.ItemIndex of
    0:    Result := crAlbion;
    1:    Result := crMidgard;
    2:    Result := crHibernia;
  end;
end;

procedure TfrmSpellcraftHelp.btnQuickbarsClick(Sender: TObject);
var
  I:      integer;
  pSCSkill: TDAOCNameValuePair;
  pRecipes: TCraftRecipeCollection;
  pItem:    TTradeskillRecipe;
  pTrWin:     TTradeRecipeWindow;
  pQuWin:     TQuickbar;
begin
  pSCSkill := FDControl.LocalPlayer.Skills.Find('Spellcrafting');
  if not Assigned(pSCSkill) then begin
    ShowMessage('Player does not appear to have Spellcrafting skill');
    exit; 
  end;

  if MessageDlg('Make sure:'#13 +
    #9'The Spellcrafting icon is in quickbar slot 1.'#13 +
    #9'Slots 2-4 do not have anything you want to keep in them.'#13 +
    #9'The Spellcrafting recipe list is closed.'#13 +
    'and then press OK',
    mtInformation, [mbOK, mbCancel], 0) = mrCancel then
    exit;

  FDControl.FocusDAOCWindow;

  pQuWin := TQuickbar.Create(FDControl.WindowManager);
  for I := 1 to 4 do begin
    pQuWin.ClearSlot(I+1);
    sleep(200);

    pRecipes := GetSpellcraftingRecipeList;
    pItem := FindRecipeForGem(I);
    if Assigned(pItem) then begin
      FDControl.DoSendKeys('1');
      sleep(200);

      pTrWin := TTradeRecipeWindow.Create(FDControl.WindowManager);
      pTrWin.ClickRecipe(pItem, pRecipes, pSCSkill.Value);
      sleep(100);

      pQuWin.ClickSlot(I+1);
      sleep(200);

        { this is to get around a bug where the first click in recipe window
          is ignored next time it is used ????  WTF MYSTIK!?!! }
      pTrWin.ScrollUp;
      pTrWin.Free;

      FDControl.DoSendKeys('1');
      sleep(200);
    end;  { if assigned recipe }
  end;  { for I }
  pQuWin.Free;
end;

function TfrmSpellcraftHelp.FindRecipeForGem(AIndex: integer): TTradeskillRecipe;
var
  pCraft: TCraftRecipeCollection;
begin
  Result := nil;

  if FSCGems[AIndex].Name = '' then
    exit;

  pCraft := GetSpellcraftingRecipeList;
  if Assigned(pCraft) then
    Result := pCraft.FindDisplayName(FSCGems[AIndex].Name);
end;

function TfrmSpellcraftHelp.GetSpellcraftingRecipeList: TCraftRecipeCollection;
begin
  Result := FDControl.TradeRecipes.FindRealmAndCraft(GetCraftRealm, 'Spellcraft');
end;

procedure TfrmSpellcraftHelp.SetCraftRealm(const Value: TCraftRealm);
begin
  cbxRealm.ItemIndex := ord(Value) - 1;
  try
    cbxRealmChange(nil);
  except
  end;
end;

procedure TfrmSpellcraftHelp.UpdateTotalMaterials;
var
  I:    integer;
  iMat: integer;
  pRecipe:  TTradeskillRecipe;
  pMat:     TMaterialReq;
  iCountHeld: integer;
  iQuantity:  integer;
begin
  if chkQuantity.Checked then
    iQuantity := 10
  else
    iQuantity := 1;
  FMaterialReqList.Clear;
  for I := low(FSCGems) to high(FSCGems) do begin
    pRecipe := FindRecipeForGem(I);
    if Assigned(pRecipe) then begin
      for iMat := 0 to pRecipe.Materials.Count - 1 do begin
        pMat := FMaterialReqList.FindOrAdd(pRecipe.Materials[iMat].Name);
        pMat.Count := pMat.Count + (pRecipe.Materials[iMat].Count * iQuantity);
      end;  { for each mat }
    end;  { if assigned recipe }
  end;  { for I in Gems }

  FMaterialReqList.SortByCount(true);
  
  memMaterials.Clear;
  for I := 0 to FMaterialReqList.Count - 1 do
    with FMaterialReqList[I] do begin
      iCountHeld := FDControl.LocalPlayer.Inventory.TotalCountOfItem(Name);
      memMaterials.Lines.Add(Format('%3.3d of %3.3d %s', [iCountHeld, Count, Name]));
    end;
end;

procedure TfrmSpellcraftHelp.mniRefreshMaterialsListClick(Sender: TObject);
begin
  UpdateTotalMaterials;
end;

procedure TfrmSpellcraftHelp.ExecutePurchases;
var
  I:      integer;
  iCountNeeded: integer;
  iCountHeld:   integer;
  pVendorWnd:   TVendorWindow;
  pVendorItem:  TDAOCVendorItem;
  iQuantity:    integer;
begin
  FKeepBuying := true;

  UpdateTotalMaterials;

  if chkQuantity.Checked then
    iQuantity := 10
  else
    iQuantity := 1;

  pVendorWnd := TVendorWindow.Create(FDControl.WindowManager);
  for I := 0 to FMaterialReqList.Count - 1 do begin
    pVendorItem := FDControl.VendorItems.Find(FMaterialReqList[I].Name);

    if Assigned(pVendorItem) then begin
      iCountNeeded := FMaterialReqList[I].Count * iQuantity;
      iCountHeld := FDControl.LocalPlayer.Inventory.TotalCountOfItem(
        FMaterialReqList[I].Name, false);
      dec(iCountNeeded, iCountHeld);

      if iCountNeeded > 0 then begin
        pVendorWnd.SetPage(pVendorItem.Page, true);
        sleep(500);
        pVendorWnd.SelectItemIcon(pVendorItem.Position);
        sleep(500);
          { Adjust for multibuy for stacks }
        iCountNeeded := (iCountNeeded + pVendorItem.Quantity - 1) div
          pVendorItem.Quantity;
        pVendorWnd.BuyMultiple(iCountNeeded);
        sleep(1000);
        Application.ProcessMessages;  // This may cause us to be re-entrant
                                      // Since the packet capture is still running
        UpdateTotalMaterials;
      end;  { if countneeded > 0 }
    end  { if vendor has item }
    else
      Log('Vendor does not have item: ' + FMaterialReqList[I].Name);

    if not FKeepBuying then
      break;
  end;  { for each material }

  pVendorWnd.Free;
  UpdateTotalMaterials;

  FDControl.DoSendKeys('[esc]');

{ old buy code }
//        pVendorWnd.SelectItem(pVendorItem.Position);
//        while (iCountNeeded > 0) and FKeepBuying do begin
//          pVendorWnd.Buy;
//          dec(iCountNeeded, pVendorItem.Quantity);
//          inc(iBuyCnt);
//          if (iBuyCnt mod 5) = 0 then
//            UpdateTotalMaterials;
end;

procedure TfrmSpellcraftHelp.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmSpellcraftHelp.mniSaveToFileClick(Sender: TObject);
begin
  if dlgSCFilename.Execute then 
    with TINIFile.Create(dlgSCFilename.FileName) do begin
      WriteInteger('Main', 'Realm', cbxRealm.ItemIndex);

      WriteInteger('Gem1', 'Type', cbxType1.ItemIndex);
      WriteInteger('Gem1', 'Plus', cbxPlus1.ItemIndex);
      WriteInteger('Gem1', 'Effect', cbxEffect1.ItemIndex);

      WriteInteger('Gem2', 'Type', cbxType2.ItemIndex);
      WriteInteger('Gem2', 'Plus', cbxPlus2.ItemIndex);
      WriteInteger('Gem2', 'Effect', cbxEffect2.ItemIndex);

      WriteInteger('Gem3', 'Type', cbxType3.ItemIndex);
      WriteInteger('Gem3', 'Plus', cbxPlus3.ItemIndex);
      WriteInteger('Gem3', 'Effect', cbxEffect3.ItemIndex);

      WriteInteger('Gem4', 'Type', cbxType4.ItemIndex);
      WriteInteger('Gem4', 'Plus', cbxPlus4.ItemIndex);
      WriteInteger('Gem4', 'Effect', cbxEffect4.ItemIndex);
      Free;
    end;
end;

procedure TfrmSpellcraftHelp.mniLoadFromFileClick(Sender: TObject);
begin
  if dlgSCFilename.Execute then 
    with TINIFile.Create(dlgSCFilename.FileName) do begin
      cbxRealm.ItemIndex := ReadInteger('Main', 'Realm', cbxRealm.ItemIndex);

      cbxType1.ItemIndex := ReadInteger('Gem1', 'Type', cbxType1.ItemIndex);
      cbxTypeChange(cbxType1);
      cbxPlus1.ItemIndex := ReadInteger('Gem1', 'Plus', cbxPlus1.ItemIndex);
      cbxEffect1.ItemIndex := ReadInteger('Gem1', 'Effect', cbxEffect1.ItemIndex);
      cbxPlusChange(cbxPlus1);
      cbxEffectChange(cbxEffect1);

      cbxType2.ItemIndex := ReadInteger('Gem2', 'Type', cbxType2.ItemIndex);
      cbxTypeChange(cbxType2);
      cbxPlus2.ItemIndex := ReadInteger('Gem2', 'Plus', cbxPlus2.ItemIndex);
      cbxEffect2.ItemIndex := ReadInteger('Gem2', 'Effect', cbxEffect2.ItemIndex);
      cbxPlusChange(cbxPlus2);
      cbxEffectChange(cbxEffect2);

      cbxType3.ItemIndex := ReadInteger('Gem3', 'Type', cbxType3.ItemIndex);
      cbxTypeChange(cbxType3);
      cbxPlus3.ItemIndex := ReadInteger('Gem3', 'Plus', cbxPlus3.ItemIndex);
      cbxEffect3.ItemIndex := ReadInteger('Gem3', 'Effect', cbxEffect3.ItemIndex);
      cbxPlusChange(cbxPlus3);
      cbxEffectChange(cbxEffect3);

      cbxType4.ItemIndex := ReadInteger('Gem4', 'Type', cbxType4.ItemIndex);
      cbxTypeChange(cbxType4);
      cbxPlus4.ItemIndex := ReadInteger('Gem4', 'Plus', cbxPlus4.ItemIndex);
      cbxEffect4.ItemIndex := ReadInteger('Gem4', 'Effect', cbxEffect4.ItemIndex);
      cbxPlusChange(cbxPlus4);
      cbxEffectChange(cbxEffect4);
      Free;
    end;
end;

procedure TfrmSpellcraftHelp.chkQuantityClick(Sender: TObject);
begin
  UpdateTotalMaterials;
end;

end.

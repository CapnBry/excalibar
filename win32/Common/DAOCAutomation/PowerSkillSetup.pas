unit PowerSkillSetup;

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
  Dialogs, PowerSkill, StdCtrls, ExtCtrls, Buttons, DAOCControl, DAOCWindows,
  VendorItems, DAOCPlayerAttributes, DAOCObjs, Recipes;

type
  TfrmPowerskill = class(TForm)
    grpRecipe: TGroupBox;
    lstItems: TListBox;
    lblName: TLabel;
    lblIngr1: TLabel;
    pnlTop: TPanel;
    edtProfile: TEdit;
    Label1: TLabel;
    btnLoad: TButton;
    lblCount: TLabel;
    chkAutoAdvance: TCheckBox;
    pnlLoading: TPanel;
    lblCost: TLabel;
    btnToQuickbar: TSpeedButton;
    procedure btnLoadClick(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);
    procedure btnBuyClick(Sender: TObject);
    procedure edtProfileKeyPress(Sender: TObject; var Key: Char);
    procedure lblCountClick(Sender: TObject);
    procedure lstItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure chkAutoAdvanceClick(Sender: TObject);
    procedure btnToQuickbarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPSItemList:  TPowerSkillItemList;
    FDControl:    TDAOCControl;
    FKeepBuying:    boolean;
    FLastQuickSlot: string;
    FCurrentSkillLevel:   integer;
    procedure SetPSItemList(const Value: TPowerSkillItemList);
    procedure RefreshListItems;
    procedure RefreshRecipe;
    procedure RefreshFromPSList;
    function GetSelectedItem : TPowerSkillItemDef;
    procedure Log(const s: string);
    procedure SelectItem(AItem: TPowerSkillItemDef);
    function GetAutoAdvance: boolean;
    function GetProfile: string;
    procedure SetAutoAdvance(const Value: boolean);
    procedure SetProfile(const Value: string);
    procedure UpdateCurrentSkillLevel;
    function MBuyQuantity(ACountNeeded, AVendorQuantity: integer): integer;
  public
    procedure ExecutePurchases;
    procedure SelectItemForSkill;
    function HasMaterialsForItem : boolean;
    procedure SkillLevelChanged(AItem: TDAOCNameValuePair);
    procedure RecipeToQuickbar(ASlot: integer);

    property PSItemList: TPowerSkillItemList read FPSItemList write SetPSItemList;
    property DAOCControl: TDAOCControl read FDControl write FDControl;
    property KeepBuying: boolean read FKeepBuying write FKeepBuying;
    property Profile: string read GetProfile write SetProfile;
    property AutoAdvance: boolean read GetAutoAdvance write SetAutoAdvance;
    property LastQuickSlot: string read FLastQuickSlot write FLastQuickSlot;
  end;

var
  frmPowerskill: TfrmPowerskill;

implementation

uses
  Unit1;

{$R *.dfm}

{ TfrmPowerskill }

procedure TfrmPowerskill.RefreshListItems;
var
  I:    integer;
begin
  lstItems.Items.Clear;
  if Assigned(FPSItemList) then
    for I := 0 to FPSItemList.Count - 1 do
      with FPSItemList[I] do
        lstItems.Items.AddObject(Format('%3d %s', [StartSkillAt, Name]), FPSItemList[I]);
end;

procedure TfrmPowerskill.RefreshRecipe;
var
  pItem:    TPowerSkillItemDef;
  s:    string;
  I:    integer;
  iCost:      integer;
  iTotalCost: integer;
  pVendorItems:  TDAOCVendorItemList;
begin
  pItem := GetSelectedItem;

  if Assigned(pItem) then begin
    lblName.Caption := pItem.Name;
    lblCount.Caption := IntToStr(pItem.MinPurchase) + 'x';
    s := '';
    iTotalCost := 0;
    for I := 0 to pItem.Materials.Count - 1 do begin
      if s <> '' then
        s := s + #13;

      s := s + IntToStr(pItem.Materials[I].Count) + ' ' + pItem.Materials[I].Name;

      pVendorItems := FDControl.MasterVendorList.FindNearestItemVendor(
        pItem.Materials[I].Name, FDControl.LocalPlayer);
      if Assigned(pVendorItems) then begin
        iCost := pVendorItems.CostOfItemQ1(pItem.Materials[I].Name);
        iCost := iCost * pItem.Materials[I].Count;
        inc(iTotalCost, iCost);
        s := s + ' (' + CopperToStr(iCost) + ' - ' + pVendorItems.Vendor.Name + ')';
      end;
    end;  { for I to materials.count }
    lblIngr1.Caption := s;
    lblCost.Caption := CopperToStr(iTotalCost);
  end
  else begin
    lblName.Caption := '- No item selected -';
    lblCount.Caption := '';
    lblIngr1.Caption := '';
    lblCost.Caption := '';
  end;
end;

procedure TfrmPowerskill.SetPSItemList(const Value: TPowerSkillItemList);
begin
  FPSItemList := Value;
  UpdateCurrentSkillLevel;
  RefreshFromPSList;
end;

procedure TfrmPowerskill.btnLoadClick(Sender: TObject);
begin
  if Assigned(FPSItemList) then begin
    pnlLoading.Visible := true;
    pnlLoading.Update;
    try
      FPSItemList.LoadFromFile(ExtractFilePath(ParamStr(0)) + edtProfile.Text +
        '.ini', FDControl.TradeRecipes);
    finally
      pnlLoading.Visible := false;
    end;
    RefreshFromPSList;
  end;
end;

procedure TfrmPowerskill.lstItemsClick(Sender: TObject);
begin
  RefreshRecipe;
end;

procedure TfrmPowerskill.btnBuyClick(Sender: TObject);
begin
  ExecutePurchases;
end;

function TfrmPowerskill.GetSelectedItem: TPowerSkillItemDef;
begin
  if (lstItems.ItemIndex <> -1) and (lstItems.ItemIndex < FPSItemList.Count) then
    Result := FPSItemList[lstItems.ItemIndex]
  else
    Result := nil;
end;

function TfrmPowerSkill.MBuyQuantity(ACountNeeded, AVendorQuantity: integer) : integer;
begin
  if ACountNeeded = 0 then
    Result := 0
  else
    Result := (ACountNeeded + AVendorQuantity - 1) div AVendorQuantity;
    
    { can only buy 100 at a time }
  if Result > 100 then
    Result := 100;
end;

procedure TfrmPowerskill.ExecutePurchases;
var
  I:      integer;
  pItem:  TPowerSkillItemDef;
  iMBuyQuantity:  integer;
  bShouldMBuy:    boolean;
  iCountNeeded:   integer;
  iCountHeld:     integer;
  pVendorWnd:   TVendorWindow;
  pVendorItem:  TDAOCVendorItem;
begin
  FKeepBuying := true;

  if chkAutoAdvance.Checked then
    SelectItemForSkill;

  pItem := GetSelectedItem;
  if Assigned(pItem) and Assigned(FDControl) then begin
    pVendorWnd := TVendorWindow.Create(FDControl.WindowManager);

    for I := 0 to pItem.Materials.Count - 1 do begin
      pVendorItem := FDControl.VendorItems.Find(pItem.Materials[I].Name);

      if Assigned(pVendorItem) then begin
        iCountNeeded := pItem.Materials[I].Count * pItem.MinPurchase;
        iCountHeld := FDControl.LocalPlayer.Inventory.TotalCountOfItem(
          pItem.Materials[I].Name);
        dec(iCountNeeded, iCountHeld);

        if iCountNeeded > 0 then begin
          iMBuyQuantity := MBuyQuantity(iCountNeeded, pVendorItem.Quantity);
          bShouldMBuy := FPSItemList.UseMBuy and (iMBuyQuantity > 1);

          pVendorWnd.SetPage(pVendorItem.Page, true);
          sleep(500);
          pVendorWnd.SelectItem(pVendorItem.Position);
          sleep(500);

          while (iCountNeeded > 0) and FKeepBuying do begin
            if bShouldMBuy then begin
              pVendorWnd.BuyMultiple(iMBuyQuantity);
              dec(iCountNeeded, iMBuyQuantity * pVendorItem.Quantity);
              iMBuyQuantity := MBuyQuantity(iCountNeeded, pVendorItem.Quantity);
              sleep(500);
            end

            else begin
              pVendorWnd.Buy;
              dec(iCountNeeded, pVendorItem.Quantity);
              sleep(250);
            end;

            Application.ProcessMessages;  // This may cause us to be re-entrant
                                          // Since the packet capture is still running
          end;
        end;  { if countneeded > 0 }
      end  { if vendor has item }
      else
        Log('Vendor does not have item: ' + pItem.Materials[I].Name);

      if not FKeepBuying then
        break;
    end;  { for each material }

    pVendorWnd.Free;
  end;  { if pItem and FDControl }
end;

procedure TfrmPowerskill.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmPowerskill.SelectItemForSkill;
var
  pItem:    TPowerSkillItemDef;
begin
  UpdateCurrentSkillLevel;

  if FCurrentSkillLevel <> -1 then
    pItem := FPSItemList.ItemForSkillLevel(FCurrentSkillLevel)
  else
    pItem := nil;

  SelectItem(pItem);
end;

procedure TfrmPowerskill.SelectItem(AItem: TPowerSkillItemDef);
var
  I:    integer;
begin
  if not Assigned(FPSItemList) then
    exit;

  for I := 0 to FPSItemList.Count - 1 do
    if FPSItemList[I] = AItem then begin
      lstItems.ItemIndex := I;
      lstItemsClick(lstItems);
      grpRecipe.Update;
      exit;
    end;

  lstItems.ItemIndex := -1;
end;

function TfrmPowerskill.HasMaterialsForItem: boolean;
var
  pItem:  TPowerSkillItemDef;
  I:      integer;
  iCountNeeded: integer;
  iCountHeld:   integer;
begin
  pItem := GetSelectedItem;

  if Assigned(pItem) then begin
    for I := 0 to pItem.Materials.Count - 1 do begin
      iCountNeeded := pItem.Materials[I].Count;
      iCountHeld := FDControl.LocalPlayer.Inventory.TotalCountOfItem(
        pItem.Materials[I].Name);
      if iCountNeeded > iCountHeld then begin
        Log(Format('Not enough materials to make a %s.  Short on %s.  Need %d have %d', [
          pItem.Name, pItem.Materials[I].Name, iCountNeeded, iCountHeld])); 
        Result := false;
        exit;
      end;
    end;  { for each material }
  end;  { if pItem }

  Result := true;
end;

procedure TfrmPowerskill.RefreshFromPSList;
begin
  if Assigned(FPSItemList) and (FPSItemList.LocaleNodeListName <> '') then
    FDControl.MapNodes.LoadFromFile(FPSItemList.LocaleNodeListName);
    
  UpdateCurrentSkillLevel;
  RefreshListItems;
end;

function TfrmPowerskill.GetAutoAdvance: boolean;
begin
  Result := chkAutoAdvance.Checked;
end;

function TfrmPowerskill.GetProfile: string;
begin
  Result := edtProfile.Text;
end;

procedure TfrmPowerskill.SetAutoAdvance(const Value: boolean);
begin
  chkAutoAdvance.Checked := Value;
  chkAutoAdvanceClick(nil);
end;

procedure TfrmPowerskill.SetProfile(const Value: string);
begin
  edtProfile.Text := Value;
end;

procedure TfrmPowerskill.edtProfileKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    btnLoad.Click;
  end;
end;

procedure TfrmPowerskill.lblCountClick(Sender: TObject);
var
  pItem:  TPowerSkillItemDef;
  sCount: string;
begin
  pItem := GetSelectedItem;
  if not Assigned(pItem) then
    exit;

  sCount := IntToStr(pItem.MinPurchase);
  if InputQuery('Min purchase', 'Number of items to purchase materials for', sCount) then begin
    pItem.MinPurchase := StrToInt(sCount);
    RefreshRecipe;
  end;
end;

procedure TfrmPowerskill.SkillLevelChanged(AItem: TDAOCNameValuePair);
begin
  if not Assigned(FPSItemList) then
    exit;

  if AnsiSameText(AItem.Name, FPSItemList.SkillName) then
    FCurrentSkillLevel := AItem.Value;

  lstItems.Invalidate;
end;

procedure TfrmPowerskill.UpdateCurrentSkillLevel;
var
  pSkill:   TDAOCNameValuePair;
begin
  FCurrentSkillLevel := -1;
  if not (Assigned(FPSItemList) and Assigned(FDControl)) then
    exit;

  pSkill := FDControl.LocalPlayer.Skills.Find(FPSItemList.SkillName);
  if Assigned(pSkill) then
    FCurrentSkillLevel := pSkill.Value;
end;

procedure TfrmPowerskill.lstItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  pPSItem:  TPowerSkillItemDef;
  rcc:      TRecipeCraftCon;
begin
  pPSItem := TPowerSkillItemDef(lstItems.Items.Objects[Index]);
  if not Assigned(pPSItem) then
    exit;

  with lstItems.Canvas do begin
    if odSelected in State then
      Font.Color := clHighlightText
    else begin
      rcc := pPSItem.CraftCon(FCurrentSkillLevel);
      case rcc of
        rccGray:    Font.Color := clGray;
        rccGreen:   Font.Color := clGreen;
        rccBlue:    Font.Color := clNavy;
        rccYellow:  Font.Color := clOlive;
        rccOrange:  Font.Color := RGB(255, 127, 0);
        rccRed:     Font.Color := clMaroon;
        rccPurple:  Font.Color := clFuchsia;
      end;  { case rcc }
    end;  { if not focused }

    FillRect(Rect);
    TextOut(Rect.Left + 2, Rect.Top, lstItems.Items[Index]);
  end;  { with canvas }
end;

procedure TfrmPowerskill.chkAutoAdvanceClick(Sender: TObject);
begin
  if chkAutoAdvance.Checked then
    lstItems.Style := lbOwnerDrawFixed
  else
    lstItems.Style := lbStandard;
end;

procedure TfrmPowerskill.btnToQuickbarClick(Sender: TObject);
var
  iQuickSlot: integer;
begin
  if not InputQuery('Move to quickbar (Trade must be in slot 1)', 'Quickbar slot number', FLastQuickSlot) then
    exit;
  iQuickSlot := StrToInt(FLastQuickSlot);
  RecipeToQuickbar(iQuickSlot);
end;

procedure TfrmPowerskill.FormCreate(Sender: TObject);
begin
  FLastQuickSlot := '10';
end;

procedure TfrmPowerskill.RecipeToQuickbar(ASlot: integer);
{ Trade must be in slot 1 and not open before calling.  Slots
  are numbered 1-10 }
var
  pRecipes:   TCraftRecipeCollection;
  pItem:      TTradeSkillRecipe;
  pTrWin:     TTradeRecipeWindow;
  pQuWin:     TQuickbar;
  pSelectedPowerskillDef: TPowerSkillItemDef;
begin
  pSelectedPowerskillDef := GetSelectedItem;
  if not Assigned(pSelectedPowerskillDef) then begin
    Log('PowerskillBuy: No recipe selected.');
    exit;
  end;

  FDControl.FocusDAOCWindow;

  FDControl.DoSendKeys('1');
  sleep(500);

  pRecipes := FDControl.TradeRecipes.FindRealmAndCraft(FPSItemList.RecipeRealm,
    FPSItemList.RecipeSkillName);
  if Assigned(pRecipes) then begin
    pItem := pRecipes.FindDisplayName(pSelectedPowerskillDef.Name);
    if Assigned(pItem) then begin
      pQuWin := TQuickbar.Create(FDControl.WindowManager);
      pQuWin.ClearSlot(ASlot);

      pTrWin := TTradeRecipeWindow.Create(FDControl.WindowManager);
      pTrWin.ClickRecipe(pItem, pRecipes, FCurrentSkillLevel);

      pQuWin.ClickSlot(ASlot);
      pQuWin.Free;

        { this is to get around a bug where the first click in recipe window
          is ignored next time it is used ????  WTF MYSTIK!?!! }
      pTrWin.ScrollUp;
      pTrWin.Free;
    end;  { if assigned Item }
  end;  { if assigned Recipes }

  FDControl.DoSendKeys('1');
end;

end.

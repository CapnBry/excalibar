unit PowerSkill;

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
  Classes, SysUtils, Contnrs, INIFiles, Recipes;

type
  TPowerSkillItemDef = class(TObject)
  private
    FMinPurchase: integer;
    FStartSkillAt: integer;
    FName: string;
    FMaterials: TMaterialReqList;
    FRecipeSkill: integer;
    FRecipeTier: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function CraftCon(AAtSkill: integer) : TRecipeCraftCon;

    property Name: string read FName write FName;
    property RecipeTier: integer read FRecipeTier write FRecipeTier;
    property RecipeSkill: integer read FRecipeSkill write FRecipeSkill;
    property StartSkillAt: integer read FStartSkillAt write FStartSkillAt;
    property MinPurchase: integer read FMinPurchase write FMinPurchase;
    property Materials: TMaterialReqList read FMaterials;
  end;

  TPowerSkillItemList = class(TObjectList)
  private
    FDefaultMinPurchase:  integer;
    FSkillName: string;
    FLocaleNodeListName: string;
    FForgeNodeName: string;
    FRecipeSkillName: string;
    FRecipeRealm: TCraftRealm;
    FHowOrange: integer;
    FUseMBuy: boolean;
    FIncludeRecipes: TStringList;
    FExcludeRecipes: TStringList;
    FAutoStartProgression: boolean;
    FAutoDeselectMerchant: boolean;
    FAutoQuickbarSlot: integer;
    FMasterNodeName:    string;
    FMasterName:        string;
    FMerchantNodeNames: TStrings;
    FMerchantNames:     TStrings;
    FMerchantNodeIndex: integer;

    function GetItems(Index: integer): TPowerSkillItemDef;
    procedure SetIncludeRecipes(const ARecipeWildcard: string);
    procedure SetExcludeRecipes(const ARecipeWildcard: string);
    function IsIncludeMatch(ARecipe: TTradeSkillRecipe) : boolean;
    function IsExcludeMatch(ARecipe: TTradeSkillRecipe) : boolean;
    function GetMerchantName: string;
    function GetMerchantNodeName: string;
    function GetMerchantNodeCount: integer;
    procedure SetMerchantNodeIndex(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    procedure LoadFromFile(const AFileName: string; ARecipes: TUniversalRecipeCollection);

    function ItemBefore(AItem: TPowerSkillItemDef) : TPowerSkillItemDef;
    function ItemForSkillLevel(ALevel: integer) : TPowerSkillItemDef;
    function Find(const AName: string) : TPowerSkillItemDef;

    property Items[Index: integer]: TPowerSkillItemDef read GetItems; default;
    property DefaultMinPurchase: integer read FDefaultMinPurchase;
    property LocaleNodeListName: string read FLocaleNodeListName;
    property SkillName: string read FSkillName;
    property ForgeNodeName: string read FForgeNodeName;
    property MasterNodeName: string read FMasterNodeName;
    property MasterName: string read FMasterName;
    property MerchantNodeName: string read GetMerchantNodeName;
    property MerchantName: string read GetMerchantName;
    property MerchantNodeCount: integer read GetMerchantNodeCount;
    property MerchantNodeIndex: integer read FMerchantNodeIndex write SetMerchantNodeIndex;
    property MerchantNodeNames: TStrings read FMerchantNodeNames;
    property MerchantNames: TStrings read FMerchantNames;
    property RecipeSkillName: string read FRecipeSkillName;
    property RecipeRealm: TCraftRealm read FRecipeRealm;
    property HowOrange: integer read FHowOrange;
    property UseMBuy: boolean read FUseMBuy;
    property AutoStartProgression: boolean read FAutoStartProgression;
    property AutoDeselectMerchant: boolean read FAutoDeselectMerchant;
    property AutoQuickbarSlot: integer read FAutoQuickbarSlot;
  end;

implementation

uses
  StringParseHlprs;
  
{ TPowerSkillItemDef }

function TPowerSkillItemDef.CraftCon(AAtSkill: integer): TRecipeCraftCon;
var
  iAtTier:  integer;
begin
  iAtTier := AAtSkill div 100;
  if iAtTier < FRecipeTier then
    Result := rccPurple
  else
    Result := RecipeCraftCon(FRecipeSkill, AAtSkill);
end;

constructor TPowerSkillItemDef.Create;
begin
  inherited Create;
  FMaterials := TMaterialReqList.Create;
  FMinPurchase := 1;
end;

destructor TPowerSkillItemDef.Destroy;
begin
  FreeAndNil(FMaterials);
  inherited Destroy;
end;

{ TPowerSkillItemList }

constructor TPowerSkillItemList.Create;
begin
  inherited Create(true);
  FIncludeRecipes := TStringList.Create;
  FExcludeRecipes := TStringList.Create;
  FMerchantNodeNames := TStringList.Create;
  FMerchantNames := TStringList.Create;
end;

destructor TPowerSkillItemList.Destroy;
begin
  FreeAndNil(FMerchantNodeNames);
  FreeAndNil(FMerchantNames);
  
  FreeAndNil(FIncludeRecipes);
  FreeAndNil(FExcludeRecipes);
  inherited;
end;

function TPowerSkillItemList.Find(const AName: string): TPowerSkillItemDef;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AName, Items[I].Name) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TPowerSkillItemList.GetItems(Index: integer): TPowerSkillItemDef;
begin
  Result := TPowerSkillItemDef(inherited Items[Index]);
end;

function TPowerSkillItemList.GetMerchantNodeCount: integer;
begin
  Result := FMerchantNodeNames.Count;
end;

function TPowerSkillItemList.GetMerchantName: string;
begin
  if (FMerchantNodeIndex <> -1) and (FMerchantNodeIndex < FMerchantNames.Count) then
    Result := FMerchantNames[FMerchantNodeIndex]
  else
    Result := '';
end;

function TPowerSkillItemList.GetMerchantNodeName: string;
begin
  if (FMerchantNodeIndex <> -1) and (FMerchantNodeIndex < FMerchantNodeNames.Count) then
    Result := FMerchantNodeNames[FMerchantNodeIndex]
  else
    Result := '';
end;

function TPowerSkillItemList.IsExcludeMatch(ARecipe: TTradeSkillRecipe): boolean;
var
  I:  integer;
  sRecipeName:  string;
begin
  Result := FExcludeRecipes.Count > 0;

  if Result then begin
    sRecipeName := LowerCase(ARecipe.DisplayName);

    for I := 0 to FExcludeRecipes.Count - 1 do begin
      Result := WildMatch(FExcludeRecipes[I], sRecipeName);

      if Result then
        exit;
    end;
  end;  { if we have an exclude list }
end;

function TPowerSkillItemList.IsIncludeMatch(ARecipe: TTradeSkillRecipe): boolean;
var
  I:  integer;
  sRecipeName:  string;
begin
  Result := FIncludeRecipes.Count = 0;

  if not Result then begin
    sRecipeName := LowerCase(ARecipe.DisplayName);

    for I := 0 to FIncludeRecipes.Count - 1 do begin
      Result := WildMatch(FIncludeRecipes[I], sRecipeName);

      if Result then
        exit;
    end;
  end;  { if we have an include list }
end;

function TPowerSkillItemList.ItemBefore(AItem: TPowerSkillItemDef): TPowerSkillItemDef;
// Return the item which should be skilled on before AItem
var
  I:    integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if not Assigned(Result) or
      ((Items[I].StartSkillAt < AItem.StartSkillAt) and
      ((AItem.StartSkillAt - Items[I].StartSkillAt) < (AItem.StartSkillAt - Result.StartSkillAt))) then
      Result := Items[I];
end;

function TPowerSkillItemList.ItemForSkillLevel(ALevel: integer): TPowerSkillItemDef;
/// Find the item which StartSkill at is closest to ALevel without going over
var
  I:    integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if not Assigned(Result) or
      ((Items[I].StartSkillAt <= ALevel) and
      ((ALevel - Items[I].StartSkillAt) < (ALevel - Result.StartSkillAt))) then
      Result := Items[I];
end;

procedure TPowerSkillItemList.LoadFromFile(const AFileName: string; ARecipes: TUniversalRecipeCollection);
var
  INI:    TINIFile;
  I:      integer;
  sNode:  string;
  sName:  string;
  pTmpItem: TPowerSkillItemDef;
  pRecipes: TCraftRecipeCollection;
  U:  TUniversalRecipeCollection;
  iStartSkillAt:    integer;
//  iItemCount: integer;
//  iPos:       integer;
//  iMaterial:  integer;
//  sMaterial:  string;
//  pTmpMaterial: TMaterialReq;
begin
  Clear;
  INI := TINIFile.Create(AFileName);
  with INI do begin
    FDefaultMinPurchase := ReadInteger('Main', 'DefaultMinPurchase', 1);
    FSkillName := ReadString('Main', 'SkillName', ChangeFileExt(ExtractFileName(AFileName), ''));
    FLocaleNodeListName := ReadString('Main', 'LocaleNodeListName', '');
    FForgeNodeName := ReadString('Main', 'ForgeNodeName', '');
    FRecipeSkillName := ReadString('Main', 'RecipeSkillName', FSkillName);
    FRecipeRealm := TCraftRealm(ReadInteger('Main', 'RecipeRealm', 1));
    FHowOrange := ReadInteger('Main', 'HowOrange', 0);
    FUseMBuy := ReadBool('Main', 'UseMBuy', true);
    SetIncludeRecipes(ReadString('Main', 'IncludeRecipes', ''));
    SetExcludeRecipes(ReadString('Main', 'ExcludeRecipes', ''));
    FAutoStartProgression := ReadBool('Main', 'AutoStartProgression', true);
    FAutoDeselectMerchant := ReadBool('Main', 'AutoDeselectMerchant', true);
    FAutoQuickbarSlot := ReadInteger('Main', 'AutoQuickbarSlot', 0);
    FMasterNodeName := ReadString('Main', 'MasterNodeName', '');
    FMasterName := ReadString('Main', 'MasteName', '');

    I := 0;
    repeat
        { on the first item, use the bacward compatible name (no 0) }
      if I = 0 then begin
        sNode := ReadString('Main', 'MerchantNodeName0', ReadString('Main', 'MerchantNodeName', ''));
        sName := ReadString('Main', 'MerchantName0', ReadString('Main', 'MerchantName', ''));
      end
      else begin
        sNode := ReadString('Main', 'MerchantNodeName' + IntToStr(I), '');
        sName := ReadString('Main', 'MerchantName' + IntToStr(I), '');
      end;

        { always add both, so the list indexes are the same on both lists }
      if sNode <> '' then begin
        FMerchantNodeNames.Add(sNode);
        FMerchantNames.Add(sName);
      end;

      inc(I)
    until sNode = '';

    if I > 1 then
      FMerchantNodeIndex := 0;

    Free;
  end;  { with INI }

  if not Assigned(ARecipes) then begin
    U := TUniversalRecipeCollection.Create;
    U.LoadFromFileIRF('tdl.irf', FRecipeRealm, FRecipeSkillName);
    pRecipes := U.FindRealmAndCraft(FRecipeRealm, FRecipeSkillName);
  end
  else begin
    U := nil;
    pRecipes := ARecipes.FindRealmAndCraft(FRecipeRealm, FRecipeSkillName);
  end;

  if Assigned(pRecipes) then begin
    pRecipes.SortBySkill;
    for I := 0 to pRecipes.Count - 1 do begin
      if not IsIncludeMatch(pRecipes[I]) or IsExcludeMatch(pRecipes[I]) then
        continue;

      pTmpItem := TPowerSkillItemDef.Create;
      Add(pTmpItem);
      pTmpItem.FName := pRecipes[I].DisplayName;
      pTmpItem.RecipeSkill := pRecipes[I].SkillLevel;
      pTmpItem.RecipeTier := pRecipes[I].Tier;
      iStartSkillAt := pRecipes[I].SkillLevel - FHowOrange;
        { if the startskillat goes below a tier boundary, cut off at the boundary,
          since at 498 skill, the 500+ items aren't available yet }
      if (iStartSkillAt < 0) or ((iStartSkillAt div 100) < pRecipes[I].Tier) then // ((pRecipes[I].SkillLevel mod 100) < FHowOrange) then
        pTmpItem.FStartSkillAt := (pRecipes[I].SkillLevel div 100) * 100
      else
        pTmpItem.FStartSkillAt := iStartSkillAt;
      pTmpItem.FMinPurchase := FDefaultMinPurchase;
      pTmpItem.Materials.Assign(pRecipes[I].Materials);
        { if we created a recipe collection locally, after the assign the
          new list owns the objects.  If one was passed, the collection
          keeps its objects }
      if Assigned(U) then
        pRecipes[I].Materials.OwnsObjects := false
      else
        pTmpItem.Materials.OwnsObjects := false;
    end;
  end;  { if pRecipes }
  U.Free;

(*****
    iItemCount := ReadInteger('Main', 'ItemCount', 0);
    for I := 0 to iItemCount - 1 do
      try
        pTmpItem := TPowerSkillItemDef.Create;
        Add(pTmpItem);
        pTmpItem.FName := ReadString('Item_' + IntToStr(I), 'Name', '');
        pTmpItem.FStartSkillAt := ReadInteger('Item_' + IntToStr(I), 'StartSkillAt', 0);
        pTmpItem.FMinPurchase := ReadInteger('Item_' + IntToStr(I), 'MinPurchase', FDefaultMinPurchase);

        iMaterial := 0;
        repeat
            { material is <quantity> <name> }
          sMaterial := ReadString('Item_' + IntToStr(I), 'Material' + IntToStr(iMaterial), '');
          if sMaterial <> '' then begin
            iPos := Pos(' ', sMaterial);
            if iPos = 0 then
              raise Exception.CreateFmt('Material%d has no quantity?', [iMaterial]);

            pTmpMaterial := TMaterialReq.Create;
            pTmpItem.Materials.Add(pTmpMaterial);
            pTmpMaterial.Count := StrToInt(copy(sMaterial, 1, iPos - 1));
            pTmpMaterial.Name := copy(sMaterial, iPos + 1, Length(sMaterial));
          end;  { if Material <> '' }

          inc(iMaterial);
        until sMaterial = '';
      except
        on E: Exception do
          raise Exception.CreateFmt('Creating Item_%d:'#13'%s', [I, E.Message]);
      end;  { for I .. iCount }
*****)
end;

procedure TPowerSkillItemList.SetExcludeRecipes(const ARecipeWildcard: string);
begin
  FExcludeRecipes.CommaText := LowerCase(ARecipeWildcard);
end;

procedure TPowerSkillItemList.SetIncludeRecipes(const ARecipeWildcard: string);
begin
  FIncludeRecipes.CommaText := LowerCase(ARecipeWildcard);
end;

procedure TPowerSkillItemList.Clear;
begin
  inherited;
  FDefaultMinPurchase := 1;
  FSkillName := '';
  FLocaleNodeListName := '';
  FForgeNodeName := '';
  FRecipeSkillName := '';
  FRecipeRealm := crAlbion;
  FHowOrange := 0;
  FUseMBuy := true;
  FAutoStartProgression := true;
  FAutoDeselectMerchant := true;
  FAutoQuickbarSlot := 0;
  FMasterNodeName := '';
  FMasterName := '';
  FMerchantNodeIndex := -1;
    { these will not be assigned if this is the clear from the destructor }
  if Assigned(FIncludeRecipes) then
    FIncludeRecipes.Clear;
  if Assigned(FExcludeRecipes) then
    FExcludeRecipes.Clear;
  if Assigned(FMerchantNodeNames) then
    FMerchantNodeNames.Clear;
  if Assigned(FMerchantNames) then
    FMerchantNames.Clear;
end;

procedure TPowerSkillItemList.SetMerchantNodeIndex(const Value: integer);
begin
  if (Value < -1) or (Value >= MerchantNodeCount) then
    raise Exception.CreateFmt('MerchantNodeIndex out of bounds: %d', [Value]);
     
  FMerchantNodeIndex := Value;
end;

end.

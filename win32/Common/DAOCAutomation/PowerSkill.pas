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
  public
    constructor Create;
    destructor Destroy; override;

    function CraftCon(AAtSkill: integer) : TRecipeCraftCon;

    property Name: string read FName write FName;
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
    FMerchantNodeName: string;
    FRecipeSkillName: string;
    FRecipeRealm: TCraftRealm;
    FHowOrange: integer;
    FUseMBuy: boolean;
    function GetItems(Index: integer): TPowerSkillItemDef;
  public
    procedure LoadFromFile(const AFileName: string; ARecipes: TUniversalRecipeCollection);

    function ItemBefore(AItem: TPowerSkillItemDef) : TPowerSkillItemDef;
    function ItemForSkillLevel(ALevel: integer) : TPowerSkillItemDef;
    function Find(const AName: string) : TPowerSkillItemDef;

    property Items[Index: integer]: TPowerSkillItemDef read GetItems; default;
    property DefaultMinPurchase: integer read FDefaultMinPurchase;
    property LocaleNodeListName: string read FLocaleNodeListName;
    property SkillName: string read FSkillName;
    property ForgeNodeName: string read FForgeNodeName;
    property MerchantNodeName: string read FMerchantNodeName;
    property RecipeSkillName: string read FRecipeSkillName;
    property RecipeRealm: TCraftRealm read FRecipeRealm;
    property HowOrange: integer read FHowOrange;
    property UseMBuy: boolean read FUseMBuy;
  end;

implementation

{ TPowerSkillItemDef }

function TPowerSkillItemDef.CraftCon(AAtSkill: integer): TRecipeCraftCon;
begin
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
  pTmpItem: TPowerSkillItemDef;
  pRecipes: TCraftRecipeCollection;
  U:  TUniversalRecipeCollection;
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
    FMerchantNodeName := ReadString('Main', 'MerchantNodeName', '');
    FForgeNodeName := ReadString('Main', 'ForgeNodeName', '');
    FRecipeSkillName := ReadString('Main', 'RecipeSkillName', FSkillName);
    FRecipeRealm := TCraftRealm(ReadInteger('Main', 'RecipeRealm', 1));
    FHowOrange := ReadInteger('Main', 'HowOrange', 0);
    FUseMBuy := ReadBool('Main', 'UseMBuy', true);
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
      pTmpItem := TPowerSkillItemDef.Create;
      Add(pTmpItem);
      pTmpItem.FName := pRecipes[I].DisplayName;
      pTmpItem.RecipeSkill := pRecipes[I].SkillLevel;
        { if the startskillat goes below a 100 boundary, cut off at the boundary,
          since at 498 skill, the 500+ items aren't available yet }
      if (pRecipes[I].SkillLevel mod 100) < FHowOrange then
        pTmpItem.FStartSkillAt := (pRecipes[I].SkillLevel div 100) * 100
      else
        pTmpItem.FStartSkillAt := pRecipes[I].SkillLevel - FHowOrange;
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

end.

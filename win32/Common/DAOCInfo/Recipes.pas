unit Recipes;

interface

uses
  SysUtils, Classes, Contnrs, LinedFileStream, CSVLineParser;

type
  TMaterialReq = class(TObject)
  private
    FCount: integer;
    FName:  string;
  public
    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromReader(AReader: TReader);

    property Name: string read FName write FName;
    property Count: integer read FCount write FCount;
  end;

  TMaterialReqList = class(TObjectList)
  private
    function GetItems(Index: integer): TMaterialReq;
  public
    function FindOrAdd(const AMaterialName: string) : TMaterialReq;
    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromReader(AReader: TReader);

    procedure SortByCount(AAscending: boolean);

    property Items[Index: integer]: TMaterialReq read GetItems; default;
  end;

  TRecipeCraftCon = (rccGray, rccGreen, rccBlue, rccYellow, rccOrange, rccRed, rccPurple);

  TTradeSkillRecipe = class(TObject)
  private
    FID: integer;
    FGroup: integer;
    FSkillLevel: integer;
    FName: string;
    FMaterials: TMaterialReqList;
    FCraftName: string;
    FTier: integer;
    function GetDisplayName: string;
  public
    constructor Create;
    destructor Destroy; override;

    function CraftCon(AAtSkill: integer) : TRecipeCraftCon;
    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromReader(AReader: TReader);

    property Name: string read FName;
    property ID: integer read FID;
    property Group: integer read FGroup;
    property Tier: integer read FTier;
    property SkillLevel: integer read FSkillLevel;
    property Materials: TMaterialReqList read FMaterials;
    property CraftName: string read FCraftName;
    property DisplayName: string read GetDisplayName;
  end;

  TCraftRealm = (crNone, crAlbion, crMidgard, crHibernia);
  TRecipeSortOrder = (rsoNone, rsoSkill, rsoGroupTier);

  TCraftRecipeCollection = class(TObjectList)
  private
    FCraftID: integer;
    FRealm: TCraftRealm;
    FCraftName: string;
    FSortOrder: TRecipeSortOrder;
    function GetItems(Index: integer): TTradeSkillRecipe;
    procedure SetSortOrder(const Value: TRecipeSortOrder);
  public
    procedure Clear; override;

    procedure SortBySkill;
    procedure SortByGroupAndTier;

    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromReader(AReader: TReader);

    function FindDisplayName(const AName: string) : TTradeSkillRecipe;
    function MaterialClassToString(const AMClass: string) : string;
    function MaterialPrefix(const AMaterial: string) : string;
    function MaterialSufffix(const AMaterial: string) : string;
    function MaterialFixupName(const AMaterial: string) : string;

    function OrdinalOfGroup(AGroup: integer) : integer;
    function OrdinalOfTierInGroup(AGroup, ATier: integer) : integer;
    function VisibleRecipesInGroup(AGroup: integer; AAtSkill: integer) : integer;

    property Realm: TCraftRealm read FRealm;
    property SortOrder: TRecipeSortOrder read FSortOrder write SetSortOrder;
    property CraftID: integer read FCraftID;
    property CraftName: string read FCraftName;
    property Items[Index: integer]: TTradeSkillRecipe read GetItems; default;
  end;

  TTDLLineParser = class(TCSVLineParser)
  private
    function GetCraftID: integer;
    function GetCraftName: string;
    function GetGroup: integer;
    function GetID: integer;
    function GetSkillLevel: integer;
    function GetRealm: TCraftRealm;
    function GetRecipeName: string;
    function GetMaterialClass: string;
    function GetMaterialCount(I: integer): integer;
    function GetMaterialName(I: integer): string;
    function GetMaterialsClass(I: integer): string;
    function GetTier: integer;
  public
    property ID: integer read GetID;
    property CraftID: integer read GetCraftID;
    property CraftName: string read GetCraftName;
    property RecipeName: string read GetRecipeName;
    property SkillLevel: integer read GetSkillLevel;
    property Group: integer read GetGroup;
    property Tier: integer read GetTier;
    property Realm: TCraftRealm read GetRealm;
    property MaterialClass: string read GetMaterialClass;
    property MaterialsName[I: integer]: string read GetMaterialName;
    property MaterialsCount[I: integer]: integer read GetMaterialCount;
    property MaterialsClass[I: integer]: string read GetMaterialsClass;
  end;

  TUniversalRecipeCollection = class(TObject)
  private
    FCraftList:   TObjectList;
    function GetItems(Index: integer): TCraftRecipeCollection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromFileIRF(const AName: string; AOnlyRealm: TCraftRealm = crNone;
      const AOnlyCraft: string = '');
    procedure LoadFromStreamIRF(AStrm: TStream; AOnlyRealm: TCraftRealm = crNone;
      const AOnlyCraft: string = '');
    procedure SaveToFileBIN(const AFileName: string);
    procedure LoadFromFileBIN(const AFileName: string);
    procedure SaveToStreamBIN(AStream: TStream);
    procedure LoadFromStreamBIN(AStream: TStream);

    function FindRealmAndCraft(ARealm: TCraftRealm; const ACraft: string) : TCraftRecipeCollection;
    function FindSubComponent(ARealm: TCraftRealm; const AName: string;
      var ACraft: TCraftRecipeCollection; var AComponent: TTradeskillRecipe) : boolean;

    property Items[Index: integer]: TCraftRecipeCollection read GetItems;
  end;

function RecipeCraftCon(ARecipeSkill, AAtSkill: integer): TRecipeCraftCon;

implementation

function RecipeCraftCon(ARecipeSkill, AAtSkill: integer): TRecipeCraftCon;
var
  iSkillDelta:  integer;
begin
    { things in the next tier are not available just like purples }
  if (ARecipeSkill div 100) > (AAtSkill div 100) then begin
    Result := rccPurple;
    exit;
  end;

  iSkillDelta := ARecipeSkill - AAtSkill;
  case iSkillDelta of
    -49..-30: Result := rccGreen;
    -29..-10: Result := rccBlue;
    -9..0:    Result := rccYellow;
    1..19:    Result := rccOrange;
    20..45:   Result := rccRed;
    else
      if iSkillDelta <= -50 then
        Result := rccGray
      else
        Result := rccPurple;
  end;  { case iSkillDelta }
end;

function SortMaterialReqCountASC(Item1, Item2: Pointer): Integer;
begin
  Result := TMaterialReq(Item1).Count - TMaterialReq(Item2).Count;
end;

function SortMaterialReqCountDESC(Item1, Item2: Pointer): Integer;
begin
  Result := TMaterialReq(Item2).Count - TMaterialReq(Item1).Count;
end;

{ TMaterialList }

function TMaterialReqList.FindOrAdd(const AMaterialName: string): TMaterialReq;
var
  I:  integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].Name, AMaterialName) then begin
      Result := Items[I];
      exit;
    end;

  Result := TMaterialReq.Create;
  Add(Result);
  Result.FName := AMaterialName;
end;

function TMaterialReqList.GetItems(Index: integer): TMaterialReq;
begin
  Result := TMaterialReq(inherited Items[Index]);
end;

{ TTradeSkillRecipe }

function TTradeSkillRecipe.CraftCon(AAtSkill: integer): TRecipeCraftCon;
begin
  Result := RecipeCraftCon(FSkillLevel, AAtSkill);
end;

constructor TTradeSkillRecipe.Create;
begin
  inherited Create;
  FMaterials := TMaterialReqList.Create;
end;

destructor TTradeSkillRecipe.Destroy;
begin
  FMaterials.Free;
  inherited Destroy;
end;

function TTradeSkillRecipe.GetDisplayName: string;
var
  sTmpCraftName:  string;
begin
  if FCraftName <> '' then begin
      { remove the s from the end if there is one }
    if FCraftName[Length(FCraftName)] = 's' then
      sTmpCraftName := copy(FCraftName, 1, Length(FCraftName) - 1)
    else
      sTmpCraftName := FCraftName;

    sTmpCraftName := LowerCase(sTmpCraftName);
    
    if (Pos('dye', sTmpCraftName) <> 0) or (Pos('enamel', sTmpCraftName) <> 0) then
      Result := FName + ' ' + sTmpCraftName
    else
      Result := FName;
  end
  else
    Result := FName;
end;

procedure TTradeSkillRecipe.LoadFromReader(AReader: TReader);
begin
  FID := AReader.ReadInteger;
  FGroup := AReader.ReadInteger;
  FSkillLevel := AReader.ReadInteger;
  FName := AReader.ReadString;
  FCraftName := AReader.ReadString;
  FTier := AReader.ReadInteger;

  FMaterials.LoadFromReader(AReader);
end;

procedure TTradeSkillRecipe.SaveToWriter(AWriter: TWriter);
begin
  AWriter.WriteInteger(FID);
  AWriter.WriteInteger(FGroup);
  AWriter.WriteInteger(FSkillLevel);
  AWriter.WriteString(FName);
  AWriter.WriteString(FCraftName);
  AWriter.WriteInteger(FTier);

  FMaterials.SaveToWriter(AWriter);
end;

{ TCraftRecipeCollection }

procedure TCraftRecipeCollection.Clear;
begin
  inherited Clear;
  FSortOrder := rsoNone;
end;

function TCraftRecipeCollection.FindDisplayName(
  const AName: string): TTradeSkillRecipe;
var
  I:  integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AName, Items[I].DisplayName) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TCraftRecipeCollection.GetItems(Index: integer): TTradeSkillRecipe;
begin
  Result := TTradeSkillRecipe(inherited Items[Index]);
end;

procedure TCraftRecipeCollection.LoadFromReader(AReader: TReader);
var
  tmpItem: TTradeSkillRecipe;
begin
  Clear;
  FCraftID := AReader.ReadInteger;
  FRealm := TCraftRealm(AReader.ReadInteger);
  FCraftName := AReader.ReadString;
  AReader.ReadListBegin;
  while not AReader.EndOfList do begin
    tmpItem := TTradeSkillRecipe.Create;
    Add(tmpItem);
    tmpItem.LoadFromReader(AReader);
  end;
  AReader.ReadListEnd;
end;

function TCraftRecipeCollection.MaterialClassToString(const AMClass: string): string;
begin
  Result := '';

  if AnsiSameText(AMClass, 'BRON') then
    Result := 'bronze '
  else if AnsiSameText(AMClass, 'IRON') then
    Result := 'iron '
  else if AnsiSameText(AMClass, 'STEE') then
    Result := 'steel '
  else if AnsiSameText(AMClass, 'ALLO') then
    Result := 'alloy '
  else if AnsiSameText(AMClass, 'FALL') then
    Result := 'fine alloy '
  else if AnsiSameText(AMClass, 'MITH') then
    Result := 'mithril '
  else if AnsiSameText(AMClass, 'ADAM') then
    Result := 'adamantium '
  else if AnsiSameText(AMClass, 'ASTE') then
    Result := 'asterite '
  else if AnsiSameText(AMClass, 'MET9') then
    Result := 'netherium '
  else if AnsiSameText(AMClass, 'ME10') then
    Result := 'arcanium '
  else if AnsiSameText(AMClass, 'GEM0') then
    Result := 'raw '
  else if AnsiSameText(AMClass, 'GEM1') then
    Result := 'uncut '
  else if AnsiSameText(AMClass, 'GEM2') then
    Result := 'rough '
  else if AnsiSameText(AMClass, 'GEM3') then
    Result := 'flawed '
  else if AnsiSameText(AMClass, 'GEM4') then
    Result := 'imperfect '
  else if AnsiSameText(AMClass, 'GEM5') then
    Result := 'polished '
  else if AnsiSameText(AMClass, 'GEM6') then
    Result := 'faceted '
  else if AnsiSameText(AMClass, 'GEM7') then
    Result := 'precious '
  else if AnsiSameText(AMClass, 'GEM8') then
    Result := 'flawless '
  else if AnsiSameText(AMClass, 'GEM9') then
    Result := 'perfect '
  else if AnsiSameText(AMClass, 'LEA1') then
    Result := 'rawhide '
  else if AnsiSameText(AMClass, 'LEA2') then
    Result := 'tanned '
  else if AnsiSameText(AMClass, 'LEA3') then
    Result := 'cured '
  else if AnsiSameText(AMClass, 'LEA4') then
    Result := 'hard '
  else if AnsiSameText(AMClass, 'LEA5') then
    Result := 'rigid '
  else if AnsiSameText(AMClass, 'LEA6') then
    Result := 'embossed '
  else if AnsiSameText(AMClass, 'LEA7') then
    Result := 'imbued '
  else if AnsiSameText(AMClass, 'LEA8') then
    Result := 'runed '
  else if AnsiSameText(AMClass, 'LEA9') then
    Result := 'eldrich '
  else if AnsiSameText(AMClass, 'LE10') then
    Result := 'tempered '
  else if AnsiSameText(AMClass, 'CLO1') then
    Result := 'woolen '
  else if AnsiSameText(AMClass, 'CLO2') then
    Result := 'linen '
  else if AnsiSameText(AMClass, 'CLO3') then
    Result := 'brocade '
  else if AnsiSameText(AMClass, 'CLO4') then
    Result := 'silk '
  else if AnsiSameText(AMClass, 'CLO5') then
    Result := 'gossamer '
  else if AnsiSameText(AMClass, 'CLO6') then
    Result := 'sylvan '
  else if AnsiSameText(AMClass, 'CLO7') then
    Result := 'seamist '
  else if AnsiSameText(AMClass, 'CLO8') then
    Result := 'nightshade '
  else if AnsiSameText(AMClass, 'CLO9') then
    Result := 'wyvernskin '
  else if AnsiSameText(AMClass, 'CL10') then
    Result := 'silksteel '
  else if AnsiSameText(AMClass, 'WOO1') then
    Result := 'rowan '
  else if AnsiSameText(AMClass, 'WOO2') then
    Result := 'elm '
  else if AnsiSameText(AMClass, 'WOO3') then
    Result := 'oaken '
  else if AnsiSameText(AMClass, 'WOO4') then
    Result := 'ironwood '
  else if AnsiSameText(AMClass, 'WOO5') then
    Result := 'heartwood '
  else if AnsiSameText(AMClass, 'WOO6') then
    Result := 'runewood '
  else if AnsiSameText(AMClass, 'WOO7') then
    Result := 'stonewood '
  else if AnsiSameText(AMClass, 'WOO8') then
    Result := 'ebonwood '
  else if AnsiSameText(AMClass, 'WOO9') then
    Result := 'dyrwood '
  else if AnsiSameText(AMClass, 'WO10') then
    Result := 'duskwood '
  else if AnsiSameText(AMClass, 'ARO1') then
    Result := 'rough clout '
  else if AnsiSameText(AMClass, 'ARO2') then
    Result := 'rough '
  else if AnsiSameText(AMClass, 'ARO3') then
    Result := 'clout '
  else if AnsiSameText(AMClass, 'ARO4') then
    Result := 'rough flight '
  else if AnsiSameText(AMClass, 'ARO5') then
    Result := 'standard '
  else if AnsiSameText(AMClass, 'ARO6') then
    Result := 'footed clout '
  else if AnsiSameText(AMClass, 'ARO7') then
    Result := 'flight '
  else if AnsiSameText(AMClass, 'ARO8') then
    Result := 'footed '
  else if AnsiSameText(AMClass, 'ARO9') then
    Result := 'footed flight '
  else if AnsiSameText(AMClass, 'AR10') then
    Result := 'keen footed flight '
  else if AnsiSameText(AMClass, 'AR11') then
    Result := 'blunt footed flight '
  else if AnsiSameText(AMClass, 'AR12') then
    Result := 'barbed footed flight '
  else if AnsiSameText(AMClass, 'ORE1') then
    Result := 'copper '
  else if AnsiSameText(AMClass, 'ORE2') then
    Result := 'ferrite '
  else if AnsiSameText(AMClass, 'ORE3') then
    Result := 'quartz '
  else if AnsiSameText(AMClass, 'ORE4') then
    Result := 'dolomite '
  else if AnsiSameText(AMClass, 'ORE5') then
    Result := 'cobalt '
  else if AnsiSameText(AMClass, 'ORE6') then
    Result := 'carbide '
  else if AnsiSameText(AMClass, 'ORE7') then
    Result := 'sapphire '
  else if AnsiSameText(AMClass, 'ORE8') then
    Result := 'diamond '
  else if AnsiSameText(AMClass, 'ORE9') then
    Result := 'netherite '
  else if AnsiSameText(AMClass, 'OR10') then
    Result := 'arcanite '
  else if AnsiSameText(AMClass, 'GENE') then
    Result := ''
  else
    Result := AMClass + ' ';
end;

function TCraftRecipeCollection.MaterialFixupName(const AMaterial: string): string;
(*** Just a function to fixup some of Mythic's inconsistencies between what the
  vendor sells and what the recipies call for ***)
begin
  if AnsiSameText(AMaterial, 'wood boards') then
    Result := 'wood'
  else if AnsiSameText(AMaterial, 'fox glove') then
    Result := 'foxglove'
  else
    Result := AMaterial;
end;

function TCraftRecipeCollection.MaterialPrefix(
  const AMaterial: string): string;
begin
  if AnsiSameText(AMaterial, 'thread') then
    Result := 'heavy '
  else
    Result := '';
end;

function TCraftRecipeCollection.MaterialSufffix(
  const AMaterial: string): string;
begin
  if AnsiSameText(AMaterial, 'leather') then
    Result := ' square'
  else if AnsiSameText(AMaterial, 'metal') then
    Result := ' bars'
  else if AnsiSameText(AMaterial, 'wooden') then
    Result := ' boards'
  else if AnsiSameText(AMaterial, 'wood') then
    Result := 'en boards'
  else if AnsiSameText(AMaterial, 'cloth') then
    Result := ' square'
  else
    Result := '';
end;

function TCraftRecipeCollection.OrdinalOfGroup(AGroup: integer): integer;
(*** Returns how far down in a recipe list this group would be (assuming
  groups had been expanded.  Returns -1 if not found ***)
var
  I:    integer;
  iLastGroup:   integer;
begin
  SetSortOrder(rsoGroupTier);

  iLastGroup := -1;
  Result := -1;
  for I := 0 to Count - 1 do begin
    if Items[I].Group <> iLastGroup then begin
      inc(Result);
      iLastGroup := Items[I].Group;
    end;

    if Items[I].Group = AGroup then
      exit;
  end;  { for I }
end;

function TCraftRecipeCollection.OrdinalOfTierInGroup(AGroup,
  ATier: integer): integer;
(*** Returns how far down in a group's recipe list this recipe would be. Returns -1 if not found ***)
var
  iIdx:   integer;
begin
  SetSortOrder(rsoGroupTier);
  Result := 0;

  iIdx := 0;

    { scroll while we're not in the right group }
  while (iIdx < Count) and (Items[iIdx].Group <> AGroup) do
    inc(iIdx);

    { scroll while we're in the right group }
  while (iIdx < Count) and (Items[iIdx].Group = AGroup) do begin
    if Items[iIdx].Tier < ATier then
      inc(Result);
    inc(iIdx);
  end;
end;

procedure TCraftRecipeCollection.SaveToWriter(AWriter: TWriter);
var
  I:    integer;
begin
  AWriter.WriteInteger(FCraftID);
  AWriter.WriteInteger(ord(FRealm));
  AWriter.WriteString(FCraftName);
  AWriter.WriteListBegin;
  for I := 0 to Count - 1 do
    Items[I].SaveToWriter(AWriter);
  AWriter.WriteListEnd;
end;

procedure TCraftRecipeCollection.SetSortOrder(const Value: TRecipeSortOrder);
begin
  case Value of
    rsoSkill: SortBySkill;
    rsoGroupTier: SortByGroupAndTier;
    else
      FSortOrder := rsoNone;
  end;
end;

procedure TCraftRecipeCollection.SortByGroupAndTier;
var
  I:    integer;
  J:    integer;
  iMinIdx: integer;
begin
  if FSortOrder = rsoGroupTier then
    exit;

  for I := 0 to Count - 2 do begin
    iMinIdx := I;

    for J := I + 1 to Count - 1 do
        { if group is lower, or group is same and tier is lower }
      if (Items[J].Group < Items[iMinIdx].Group) or
        ((Items[J].Group = Items[iMinIdx].Group) and (Items[J].Tier < Items[iMinIdx].Tier))
        then
        iMinIdx := J;

    if iMinIdx <> I then
      Exchange(I, iMinIdx);
  end;

  FSortOrder := rsoGroupTier;
end;

procedure TCraftRecipeCollection.SortBySkill;
var
  I:    integer;
  J:    integer;
  iMinIdx: integer;
begin
  if FSortOrder = rsoSkill then
    exit;

  for I := 0 to Count - 2 do begin
    iMinIdx := I;

    for J := I + 1 to Count - 1 do
      if Items[J].SkillLevel < Items[iMinIdx].SkillLevel then
        iMinIdx := J;

    if iMinIdx <> I then
      Exchange(I, iMinIdx);
  end;

  FSortOrder := rsoSkill;
end;

function TCraftRecipeCollection.VisibleRecipesInGroup(AGroup,
  AAtSkill: integer): integer;
(*** Return the number of recipes which would be visible in a specified
  group for the specifed skill level.  This is the number of recipes in
  the group which are not purple for AAtSkillLevel ***)
var
  iIdx:   integer;
begin
  SetSortOrder(rsoGroupTier);
  Result := 0;

  iIdx := 0;

    { scroll while we're not in the right group }
  while (iIdx < Count) and (Items[iIdx].Group <> AGroup) do
    inc(iIdx);

    { scroll while we're in the right group }
  while (iIdx < Count) and (Items[iIdx].Group = AGroup) do begin
    if Items[iIdx].CraftCon(AAtSkill) <> rccPurple then
      inc(Result);
    inc(iIdx);
  end;
end;

{ TUniversalRecipeCollection }

procedure TUniversalRecipeCollection.Clear;
begin
  FCraftList.Clear;
end;

constructor TUniversalRecipeCollection.Create;
begin
  inherited Create;
  FCraftList := TObjectList.Create;
end;

destructor TUniversalRecipeCollection.Destroy;
begin
  FCraftList.Free;
  inherited Destroy;
end;

function TUniversalRecipeCollection.FindRealmAndCraft(ARealm: TCraftRealm;
  const ACraft: string): TCraftRecipeCollection;
var
  I:    integer;
begin
  for I := 0 to FCraftList.Count - 1 do
    if (Items[I].Realm = ARealm) and AnsiSameText(Items[I].CraftName, ACraft) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TUniversalRecipeCollection.FindSubComponent(ARealm: TCraftRealm;
  const AName: string; var ACraft: TCraftRecipeCollection;
  var AComponent: TTradeskillRecipe): boolean;
var
  I:    integer;
begin
  for I := 0 to FCraftList.Count - 1 do
    if Items[I].Realm = ARealm then begin
      ACraft := Items[I];
      AComponent := ACraft.FindDisplayName(AName);
      if Assigned(AComponent) then begin
        Result := true;
        exit;
      end;
    end;  { if right realm }

  ACraft := nil;
  AComponent := nil;
  Result := false;
end;

function TUniversalRecipeCollection.GetItems(Index: integer): TCraftRecipeCollection;
begin
  Result := TCraftRecipeCollection(FCraftList[Index]);
end;

procedure TUniversalRecipeCollection.LoadFromFileIRF(const AName: string;
  AOnlyRealm: TCraftRealm = crNone; const AOnlyCraft: string = '');
var
  FS:   TFileStream;
begin
  try
    FS := TFileStream.Create(AName, fmOpenRead or fmShareDenyNone);
  except
    on E: Exception do
      raise Exception.Create('Cannot open recipe file: ' + AName);
  end;

  try
    LoadFromStreamIRF(FS, AOnlyRealm, AOnlyCraft);
  finally
    FS.Free;
  end;
end;

procedure TUniversalRecipeCollection.LoadFromFileBIN(const AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStreamBIN(FS);
  finally
    FS.Free;
  end;
end;

procedure TUniversalRecipeCollection.LoadFromStreamBIN(AStream: TStream);
var
  tmpItem:  TCraftRecipeCollection;
  Reader:   TReader;
begin
  Clear;
  Reader := TReader.Create(AStream, 512);
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do begin
      tmpItem := TCraftRecipeCollection.Create;
      FCraftList.Add(tmpItem);
      tmpItem.LoadFromReader(Reader);
    end;
    Reader.ReadListEnd;
  finally
    Reader.Free;
  end;
end;

procedure TUniversalRecipeCollection.LoadFromStreamIRF(AStrm: TStream;
  AOnlyRealm: TCraftRealm; const AOnlyCraft: string);
var
  CSVP: TTDLLineParser;
  FS:   TLinedStreamWrapper;
  s:    string;
  pCraft: TCraftRecipeCollection;
  pRecipe:  TTradeSkillRecipe;
  iMaterialPos:   integer;
  sMaterial:      string;
  pMaterial:      TMaterialReq;
  iLastGroup:     integer;
  sCraftName:     string;
begin
  FS := TLinedStreamWrapper.Create(AStrm);
  
  CSVP := TTDLLineParser.Create;
  pCraft := nil;
  iLastGroup := -1;
  sCraftName := '';

  try
    while not FS.EOF do begin
      s := Trim(FS.ReadLn);
      if (s = '') or (s[1] = ';') then
        continue;

      CSVP.DataString := s;

        { ID = 0 is a header line }
      if CSVP.ID = 0 then begin
//        if Assigned(pCraft) then
//          pCraft.SortBySkill;

        if ((AOnlyRealm = crNone) or (CSVP.Realm = AOnlyRealm))
          and ((AOnlyCraft = '') or AnsiSameText(CSVP.CraftName, AOnlyCraft)) then begin
          pCraft := TCraftRecipeCollection.Create;
          FCraftList.Add(pCraft);
          pCraft.FCraftID := CSVP.CraftID;
          pCraft.FRealm := CSVP.Realm;
          pCraft.FCraftName := CSVP.CraftName;
        end
        else
          pCraft := nil;

        iLastGroup := -1;
        sCraftName := '';
      end

      else if Assigned(pCraft) then begin
        if CSVP.Group <> iLastGroup then begin
          iLastGroup := CSVP.Group;
          if CSVP.SkillLevel = 9999 then begin
            sCraftName := CSVP.CraftName;
            continue;
          end
          else
            sCraftName := '';
        end;

        pRecipe := TTradeSkillRecipe.Create;
        pCraft.Add(pRecipe);
        pRecipe.FID := CSVP.ID;
        pRecipe.FGroup := CSVP.Group;
        pRecipe.FTier := CSVP.Tier;
        pRecipe.FSkillLevel := CSVP.SkillLevel;
        pRecipe.FName := pCraft.MaterialClassToString(CSVP.MaterialClass) +
          CSVP.RecipeName;
        pRecipe.FCraftName := sCraftName; 

        iMaterialPos := 0;
        while CSVP.MaterialsName[iMaterialPos] <> '0' do begin
          sMaterial := pCraft.MaterialFixupName(CSVP.MaterialsName[iMaterialPos]);
          pMaterial := pRecipe.Materials.FindOrAdd(
            pCraft.MaterialClassToString(CSVP.MaterialsClass[iMaterialPos]) +
            pCraft.MaterialPrefix(sMaterial) +
            sMaterial +
            pCraft.MaterialSufffix(sMaterial));
          pMaterial.Count := pMaterial.Count + CSVP.MaterialsCount[iMaterialPos];
          inc(iMaterialPos);
        end;
      end;  { if pCraft }
    end;  { while !EOF }
  finally
    CSVP.Free;
    FS.Free;
  end;
end;

procedure TUniversalRecipeCollection.SaveToFileBIN(const AFileName: string);
var
  FS:   TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStreamBIN(FS);
  finally;
    FS.Free;
  end;
end;

procedure TUniversalRecipeCollection.SaveToStreamBIN(AStream: TStream);
var
  I:  integer;
  Writer:   TWriter;
begin
  Writer := TWriter.Create(AStream, 512);
  try
    Writer.WriteListBegin;
    for I := 0 to FCraftList.Count - 1 do
      Items[I].SaveToWriter(Writer);
    Writer.WriteListEnd;
  finally
    Writer.Free;
  end;
end;

{ TTDLLineParser }

function TTDLLineParser.GetCraftID: integer;
begin
  Result := FieldAsInt(1, 0);
end;

function TTDLLineParser.GetCraftName: string;
begin
  Result := Trim(Fields[7]);
end;

function TTDLLineParser.GetGroup: integer;
begin
  Result := FieldAsInt(2, 0);
end;

function TTDLLineParser.GetID: integer;
begin
  Result := FieldAsInt(0, 0);
end;

function TTDLLineParser.GetMaterialClass: string;
begin
  Result := Trim(Fields[8]);
end;

function TTDLLineParser.GetMaterialCount(I: integer): integer;
begin
  Result := FieldAsInt(11 + (5*I) + 2, 0);
end;

function TTDLLineParser.GetMaterialName(I: integer): string;
begin
  Result := Trim(Fields[11 + (5*I)]);
end;

function TTDLLineParser.GetMaterialsClass(I: integer): string;
begin
  Result := Trim(Fields[11 + (5*I) + 1]);
end;

function TTDLLineParser.GetRealm: TCraftRealm;
begin
  Result := TCraftRealm(FieldAsInt(3, 0));
end;

function TTDLLineParser.GetRecipeName: string;
begin
  Result := Trim(Fields[7]);
end;

function TTDLLineParser.GetSkillLevel: integer;
begin
  Result := FieldAsInt(10, 0);
end;

function TTDLLineParser.GetTier: integer;
begin
  Result := FieldAsInt(6, 0);
end;

procedure TMaterialReqList.LoadFromReader(AReader: TReader);
var
  tmpItem:    TMaterialReq;
begin
  Clear;
  AReader.ReadListBegin;
  while not AReader.EndOfList do begin
    tmpItem := TMaterialReq.Create;
    Add(tmpItem);
    tmpItem.LoadFromReader(AReader);
  end;
  AReader.ReadListEnd;
end;

procedure TMaterialReqList.SaveToWriter(AWriter: TWriter);
var
  I:    integer;
begin
  AWriter.WriteListBegin;
  for I := 0 to Count - 1 do
    Items[I].SaveToWriter(AWriter);
  AWriter.WriteListEnd;
end;

{ TMaterialReq }

procedure TMaterialReq.LoadFromReader(AReader: TReader);
begin
  FName := AReader.ReadString;
  FCount := AReader.ReadInteger;
end;

procedure TMaterialReq.SaveToWriter(AWriter: TWriter);
begin
  AWriter.WriteString(FName);
  AWriter.WriteInteger(FCount);
end;

procedure TMaterialReqList.SortByCount(AAscending: boolean);
begin
  if AAscending then
    Sort(SortMaterialReqCountASC)
  else
    Sort(SortMaterialReqCountDESC);
end;

end.

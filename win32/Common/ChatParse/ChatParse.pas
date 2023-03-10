unit ChatParse;

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
{$IFDEF LINUX}
  Libc, Types,
{$ELSE}
  Windows,
{$ENDIF !LINUX}
  SysUtils, Classes, Contnrs, INIFiles;

type
  TDAOCChatParser = class;
  TDAOCCurrencyChangeReason = (ccrUnknown, ccrLootSolo, ccrLootGroup, ccrTask,
    ccrSellItem, ccrBuyCon, ccrBuyItem, ccrRepairItem);

  TDAOCParserNotify = procedure (Sender: TDAOCChatParser) of object;
  TDAOCParserIntNotify = procedure (Sender: TDAOCChatParser; AVal: integer) of object;
  TDAOCParseStrNotify = procedure (Sender: TDAOCChatParser; const AVal: string) of object;
  TDAOCParserCurrencyChangeNotify = procedure(Sender: TDAOCChatParser; AChangeReason: TDAOCCurrencyChangeReason) of object;

  (****
    ZoneMap
        Top - Y loc coordinate of image top
        Left - X loc coordinte of image left
  ****)
  TZoneMap = class
  private
    FTop: integer;
    FLeft: integer;
    FUnitsPerPixelY: integer;
    FUnitsPerPixelX: integer;
    FFileName: string;
    FDescription: string;
  protected
    procedure Reset;
  public
    procedure Load(const AZoneName: string; AMapNo: integer);

    function TranslateLocX(AX: integer) : integer;
    function TranslateLocY(AY: integer) : integer;

    property Left: integer read FLeft;
    property Top: integer read FTop;
    property UnitsPerPixelX: integer read FUnitsPerPixelX;
    property UnitsPerPixelY: integer read FUnitsPerPixelY;
    property FileName: string read FFileName;
    property Description: string read FDescription;
  end;

  TZoneMapList = class(TObjectList)
  private
    FZoneName: string;
    FSelected: integer;
    function GetItems(Index: integer): TZoneMap;
    procedure SetZoneName(const Value: string);
  public
    procedure Clear; override;
    procedure Load(const AZoneName: string);

    property Items[Index: integer]: TZoneMap read GetItems; default;
    property ZoneName: string read FZoneName write SetZoneName;
    property Selected: integer read FSelected write FSelected;
  end;

  PIncomingDamage = ^TIncomingDamage;
  TIncomingDamage = record
    Count:  integer;
    Damage: integer
  end;

  TOutgoingDmgList = class;
  TAttackStats = class;
  TAttackStatsList = class;

  TOutgoingDamageType = (dtUnknown, dtBow, dtBowCritical, dtMelee);
  TEffectDamageType = (edtUnknown, edtSpell, edtProc, edtBleed, edtRProc);

  TOutgoingDamage = class(TObject)
  private
    FMinDamage: integer;
    FTotalDamage:   integer;
    FCount: integer;
    FMaxDamage: integer;
    FHitCount: integer;
    FStyle: string;
    FParent:  TOutgoingDmgList;
    FDamageType: TOutgoingDamageType;
    FLastSwingSetMinDmg: boolean;
    FLastSwingSetMaxDmg: boolean;
    FPreviousMinDmg:  integer;
    FLastSwingDmg: integer;
    function GetAvgDamage: integer;
    function GetHitPct: double;
    function GetDPS: double;
  public
    property Style: string read FStyle;
    property DamageType: TOutgoingDamageType read FDamageType;
    property Count: integer read FCount;
    property HitCount: integer read FHitCount;
    property HitPct: double read GetHitPct;
    property LastSwingDmg: integer read FLastSwingDmg;
    property LastSwingSetMinDmg: boolean read FLastSwingSetMinDmg;
    property LastSwingSetMaxDmg: boolean read FLastSwingSetMaxDmg;
    property MinDamage: integer read FMinDamage;
    property MaxDamage: integer read FMaxDamage;
    property TotalDamage: integer read FTotalDamage;
    property AvgDamage: integer read GetAvgDamage;
    property DPS: double read GetDPS;
  end;

  TOutgoingDmgList = class(TObjectList)
  private
    FLastStyle:     TOutgoingDamage;
    FUpcomingStyle: TOutgoingDamage;
    FParent:        TAttackStats;
    function GetItems(Index: integer): TOutgoingDamage;
  public
    function IndexOfStyle(const AStyle: string) : integer;
    function FindOrAddStyle(const AStyle: string) : TOutgoingDamage;
    procedure AddAttack(iDamage: integer; ADamageType: TOutgoingDamageType);
    procedure Clear; override;
    procedure ResetUpcomingStyle;
    procedure SortByStyleName;

    property Items[Index: integer]: TOutgoingDamage read GetItems; default;
    property LastStyle: TOutgoingDamage read FLastStyle;
    property UpcomingStyle: TOutgoingDamage read FUpcomingStyle;
  end;

  TAttackStats = class(TObject)
  private
    FEvades: integer;
    FHits: integer;
    FBlocks: integer;
    FFumbles: integer;
    FParries: integer;
    FInFoot: TIncomingDamage;
    FInLeg: TIncomingDamage;
    FInHand: TIncomingDamage;
    FInHead: TIncomingDamage;
    FInTorso: TIncomingDamage;
    FInArm: TIncomingDamage;
    FInMisses: integer;
    FOutDamage: TOutgoingDmgList;
    FMOB: string;
    FIncludedInDelayCnt:  integer;
    FMOBAvgDelay: integer;
    FMOBLastAttackDT: TDateTime;
    FMOBLastAttackTicks: Cardinal;
    FKillCount: integer;
    FLastExperience: integer;
    FParent:  TAttackStatsList;

    procedure IncludeInAttackDelay(ADelay: integer);
  protected
    function GetInPct(iWhich: integer) : double;
    function GetInAvg(iWhich: integer) : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function TotalInAttacks : integer;
    function TotalInHits : integer;
    function TotalInHitPct : double;
    function InMissPct : double;
    function EvadePct : double;
    function BlockPct : double;
    function ParryPct : double;
    function TotalInDamage: integer;

    property MOB: string read FMOB;
    property MOBAvgDelay: integer read FMOBAvgDelay;
    property MOBLastAttackDT: TDateTime read FMOBLastAttackDT;
    property MOBLastAttackTicks: Cardinal read FMOBLastAttackTicks;
    property LastExperience: integer read FLastExperience;

      { outgoing }
    property OutDamage: TOutgoingDmgList read FOutDamage;
    property KillCount: integer read FKillCount;
      { incoming }
    property InMisses: integer read FInMisses;
    property Evades: integer read FEvades;
    property Blocks: integer read FBlocks;
    property Parries: integer read FParries;
    property InHead: TIncomingDamage read FInHead;
    property InHeadPct: double index 0 read GetInPct;
    property InHeadAvg: integer index 0 read GetInAvg;
    property InTorso: TIncomingDamage read FInTorso;
    property InTorsoPct: double index 1 read GetInPct;
    property InTorsoAvg: integer index 1 read GetInAvg;
    property InHand: TIncomingDamage read FInHand;
    property InHandPct: double index 2 read GetInPct;
    property InHandAvg: integer index 2 read GetInAvg;
    property InArm: TIncomingDamage read FInArm;
    property InArmPct: double index 3 read GetInPct;
    property InArmAvg: integer index 3 read GetInAvg;
    property InLeg: TIncomingDamage read FInLeg;
    property InLegPct: double index 4 read GetInPct;
    property InLegAvg: integer index 4 read GetInAvg;
    property InFoot: TIncomingDamage read FInFoot;
    property InFootPct: double index 5 read GetInPct;
    property InFootAvg: integer index 5 read GetInAvg;
  end;

  TAttackStatsList = class(TObjectList)
  private
    FLastMob: TAttackStats;
    FIncludedOutDelayCnt:  integer;
    FOutAvgDelay:  integer;
    function GetItems(Index: integer) : TAttackStats;
    procedure IncludeOutAttackDelay(ADelay: integer);
  protected
    function IndexOfMob(AMobName: string) : TAttackStats;
  public
    function FindOrAddMob(const AMobName: string) : TAttackStats;

    procedure Clear; override;

    property Items[Index: integer]: TAttackStats read GetItems; default;
    property LastMOB: TAttackStats read FLastMob;
    property OutAvgDelay: integer read FOutAvgDelay;
  end;

  TDropItem = class(TObject)
  private
    FMagical: boolean;
    FSellValue: integer;
    FName: string;
    FDropMob: string;

    procedure SaveSellValue;
    procedure LoadSellValue;

    procedure SetName(const AName: string);
    procedure SetSellValue(AValue: integer);
  public
    property Name: string read FName;
    property DropMOB: string read FDropMob;
    property Magical: boolean read FMagical;
    property SellValue: integer read FSellValue;
  end;

  TDropItemList = class(TObjectList)
  private
    FLastDrop: TDropItem;

    function GetItems(iIndex: integer): TDropItem;
  public
    procedure Clear; override;

    function IndexOfItem(const AItemName, ADropMob: string) : integer;
    function FindOrAddItem(const AItemName, ADropMob: string) : TDropItem;
    property Items[iIndex: integer]: TDropItem read GetItems; default;

    property LastDrop: TDropItem read FLastDrop;
  end;

  TLocationClass = (lcNone, lcFriendly, lcNPCOther, lcStructure, lcTrainer, lcMaster);

  TLocation = class(TObject)
  private
    FX: integer;
    FY: integer;
    FDescription: string;
    FName: string;
    FZone: string;
    FLocClass: TLocationClass;
  public
    property X: integer read FX;
    property Y: integer read FY;
    property Zone: string read FZone;
    property Name: string read FName;
    property Description: string read FDescription;
    property LocClass: TLocationClass read FLocClass;
  end;

  TLocationList = class(TObjectList)
  private
    function GetItems(iIndex: integer): TLocation;
  public
    property Items[iIndex: integer]: TLocation read GetItems; default;
  end;

  TDAOCChatParser = class
  private
    FZoneValid:       boolean;
    FPrevLineBuffer:  string;
    FZone: string;
    FLocation: TPoint;
    FDate: TDateTime;
    FTime: TDateTime;
    FOnEnterZone: TDAOCParserNotify;
    FOnLeaveZone: TDAOCParserNotify;
    FOnLocationChange: TDAOCParserNotify;
    FTargetValid: boolean;
    FTarget: string;
    FOnTargetChange: TDAOCParserNotify;
    FZoneMapList: TZoneMapList;
    FRealm: string;
    FAttackStats:   TAttackStatsList;
    FOnAttackStatsChange: TDAOCParserNotify;
    FTradeSkillNPC: string;
    FTradeSkillItem: string;
    FOnTradeSkillTask: TDAOCParserNotify;
    FExperience:  Int64;
    FBountyPoints:  integer;
    FRealmPoints:   integer;
    FOnExperienceChange: TDAOCParserNotify;
    FDropItems:   TDropItemList;
    FOnSessionClose: TDAOCParserNotify;
    FOnSessionOpen: TDAOCParserNotify;
    FOnTradeSkillSuccess: TDAOCParserIntNotify;
    FOnDeath: TDAOCParserNotify;
    FOnYouSay: TDAOCParserNotify;
    FLastYouSay: string;
    FOnInParry: TDAOCParserNotify;
    FOnInBlock: TDAOCParserNotify;
    FOnInAttack: TDAOCParserNotify;
    FOnOutAttack: TDAOCParserNotify;
    FOnItemDropped: TDAOCParserNotify;
    FOnStandUp: TDAOCParserNotify;
    FOnItemPickUp: TDAOCParserNotify;
    FOnStyleExecuteFailure: TDAOCParserNotify;
    FOnStyleExecuteSuccess: TDAOCParserNotify;
    FOnStylePrepared: TDAOCParserNotify;
    FCurrentLine: string;
    FOnTargetNotInView: TDAOCParserNotify;
    FOnNotInCombatMode: TDAOCParserNotify;
    FInSession: boolean;
    FOnTradeSkillFailure: TDAOCParserNotify;
    FOnLinkDead: TDAOCParserNotify;
    FOnEnterCombatMode: TDAOCParserNotify;
    FOnTooFatigued: TDAOCParserNotify;
    FOnMOBDeath: TDAOCParserNotify;
    FOnExtraOutDamage: TDAOCParserNotify;
    FLastOutAttackDT:  TDateTime;
    FLastOutAttackTicks: Cardinal;
    FOnBountyPointsChange: TDAOCParserIntNotify;
    FOnRealmPointsChange: TDAOCParserIntNotify;
    FOnEquipWeapon: TDAOCParserNotify;
    FOnInEvade: TDAOCParserNotify;
    FOnInMiss: TDAOCParserNotify;
    FOnInAttackHit: TDAOCParserNotify;
    FOnStylePrereqFailure: TDAOCParserNotify;
    FOnHealed: TDAOCParserIntNotify;
    FOnBracketCommand: TDAOCParseStrNotify;
    FOnTradeskillMasterpiece: TDAOCParserNotify;
    FOnTradeskillCapped: TDAOCParserNotify;
    FTradeSkillQualityDistribution: array[94..101] of integer;
    FTradeSkillStreaks: array[94..100] of integer;
    FEffectDamageType:  TEffectDamageType;
    FOnTradeSkillFailureWithLoss: TDAOCParserNotify;
    FOnCurrencyChange: TDAOCParserCurrencyChangeNotify;

    procedure ParseFailedTask;
    procedure ParseBuyItem;
    procedure ParseBuyBackCon;
    procedure ParseRepairs;
    procedure ParseCoinSolo;
    procedure ParseCoinGroup;
    procedure ParseCastSpell;
    procedure ParseEffectDamage;
    procedure ParseTradeskillCap;
    procedure ParseBracketCommand;
    procedure ParseGuildChat;
    procedure ParseHealed;
    procedure ParseYouSend;
    procedure ParseStylePrereqFailure;
    procedure ParseWeaponEquiped;
    procedure ParseBowDamage;
    procedure ParseExtraOutDamage;
    procedure ParseMOBDies;
    procedure ParseTooFatigued;
    procedure ParseEnterCombatMode;
    procedure ParseYouSay;
    procedure ParseConsider;
    procedure ParseTradeskillSuccess;
    procedure ParseTradeskillFailure;
    procedure ParseDrop;
    procedure ParseSellItem;
    procedure ParseStyleDamage;
    procedure ParseItemPickup;
    procedure ParseExp;
    procedure ParseAttackDamage;
    procedure ParsePrepareStyle;
    procedure ParseTradeskillTask;
    procedure ParseTradeskillTask2;
    procedure ParseTarget;
    procedure DoEnterZone(const AZone: string);
    procedure ParseLoc;
    procedure ParseEnterLeaveRegion;
    procedure ParseTimeStamp;
    function MonthToInt(const sMonth : string) : integer;
    procedure ParseChatLogOpenClose;
    function GetTimeStamp: TDateTime;
    procedure ParseInParry;
    procedure ParseInBlock;
    procedure ParseInEvade;
    procedure ParseInMiss;
    procedure ParseOutMiss;
    procedure ParseInAttack;
    procedure DoOnAttackStatsChg;
    procedure DoOnDeath;
    procedure DoTarget(const ATarget: string);
    procedure DoOnInAttack;
    procedure DoOnOutAttack;
    procedure DoOnStandUp;
    procedure DoStyleFailure;
    procedure DoOnNotInView;
    procedure DoOnNotInCombatMode;
    procedure DoOnLinkDead;
    function GetTradeSkillQualityDistribution(I: integer): integer;
    function GetTradeSkillStreaks(I: integer): integer;
  protected
    function LineBeginsWith(const ABegins: string): boolean;
    function LineContains(const AContains: string): boolean;
    function LineEndsWith(const AEnds: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessLine(const ALine: string);
    procedure Reset; virtual;
    procedure ResetQualityDistribution;
    
    function ZoneFileName : string;

    property Realm: string read FRealm;
    property ZoneValid: boolean read FZoneValid;
    property Zone: string read FZone;
    property Location: TPoint read FLocation;
    property Date: TDateTime read FDate;
    property Time: TDateTime read FTime;
    property TimeStamp: TDateTime read GetTimeStamp;
    property TargetValid: boolean read FTargetValid write FTargetValid;
    property Target: string read FTarget write FTarget;
    property AttackStats: TAttackStatsList read FAttackStats;
    property TradeskillNPC: string read FTradeskillNPC;
    property TradeskillItem: string read FTradeskillItem;
    property Experience: Int64 read FExperience;
    property RealmPoints: integer read FRealmPoints;
    property BountyPoints: integer read FBountyPoints;
    property DropItems: TDropItemList read FDropItems;
    property LastYouSay: string read FLastYouSay;
    property LastOutAttackDT: TDateTime read FLastOutAttackDT;
    property LastOutAttackTicks: Cardinal read FLastOutAttackTicks;
    property CurrentLine: string read FCurrentLine;
    property InSession: boolean read FInSession;
    property TradeSkillQualityDistribution[I: integer]: integer read GetTradeSkillQualityDistribution;
    property TradeSkillStreaks[I: integer]: integer read GetTradeSkillStreaks;

    property ZoneMaps: TZoneMapList read FZoneMapList;

    property OnEnterZone: TDAOCParserNotify read FOnEnterZone write FOnEnterZone;
    property OnLeaveZone: TDAOCParserNotify read FOnLeaveZone write FOnLeaveZone;
    property OnLocationChange: TDAOCParserNotify read FOnLocationChange write FOnLocationChange;
    property OnTargetChange: TDAOCParserNotify read FOnTargetChange write FOnTargetChange;
    property OnAttackStatsChange: TDAOCParserNotify read FOnAttackStatsChange
      write FOnAttackStatsChange;
    property OnTradeSkillTask: TDAOCParserNotify read FOnTradeSkillTask write FOnTradeSkillTask;
    property OnExperienceChange: TDAOCParserNotify read FOnExperienceChange write FOnExperienceChange;
    property OnRealmPointsChange: TDAOCParserIntNotify read FOnRealmPointsChange write FOnRealmPointsChange;
    property OnBountyPointsChange: TDAOCParserIntNotify read FOnBountyPointsChange write FOnBountyPointsChange;
    property OnSessionOpen: TDAOCParserNotify read FOnSessionOpen write FOnSessionOpen;
    property OnSessionClose: TDAOCParserNotify read FOnSessionClose write FOnSessionClose;
    property OnTradeSkillSuccess: TDAOCParserIntNotify read FOnTradeSkillSuccess write FOnTradeSkillSuccess;
    property OnTradeSkillFailure: TDAOCParserNotify read FOnTradeSkillFailure write FOnTradeSkillFailure;
    property OnTradeSkillFailureWithLoss: TDAOCParserNotify read FOnTradeSkillFailureWithLoss write FOnTradeSkillFailureWithLoss;
    property OnTradeskillMasterpiece: TDAOCParserNotify read FOnTradeskillMasterpiece write FOnTradeskillMasterpiece;
    property OnDeath: TDAOCParserNotify read FOnDeath write FOnDeath;
    property OnYouSay: TDAOCParserNotify read FOnYouSay write FOnYouSay;
    property OnInMiss: TDAOCParserNotify read FOnInMiss write FOnInMiss;
    property OnInEvade: TDAOCParserNotify read FOnInEvade write FOnInEvade;
    property OnInAttackHit: TDAOCParserNotify read FOnInAttackHit write FOnInAttackHit;
    property OnInParry: TDAOCParserNotify read FOnInParry write FOnInParry;
    property OnInBlock: TDAOCParserNotify read FOnInBlock write FOnInBlock;
    property OnInAttack: TDAOCParserNotify read FOnInAttack write FOnInAttack;
    property OnOutAttack: TDAOCParserNotify read FOnOutAttack write FOnOutAttack;
    property OnItemDropped: TDAOCParserNotify read FOnItemDropped write FOnItemDropped;
    property OnItemPickUp: TDAOCParserNotify read FOnItemPickUp write FOnItemPickUp;
    property OnStandUp: TDAOCParserNotify read FOnStandUp write FOnStandUp;
    property OnStyleExecuteSuccess: TDAOCParserNotify read FOnStyleExecuteSuccess
      write FOnStyleExecuteSuccess;
    property OnStyleExecuteFailure: TDAOCParserNotify read FOnStyleExecuteFailure
      write FOnStyleExecuteFailure;
    property OnStylePrereqFailure: TDAOCParserNotify read FOnStylePrereqFailure
      write FOnStylePrereqFailure;
    property OnStylePrepared: TDAOCParserNotify read FOnStylePrepared write FOnStylePrepared;
    property OnTargetNotInView: TDAOCParserNotify read FOnTargetNotInView write FOnTargetNotInView;
    property OnNotInCombatMode: TDAOCParserNotify read FOnNotInCombatMode write FOnNotInCombatMode;
    property OnLinkDead: TDAOCParserNotify read FOnLinkDead write FOnLinkDead;
    property OnEnterCombatMode: TDAOCParserNotify read FOnEnterCombatMode write FOnEnterCombatMode;
    property OnTooFatigued: TDAOCParserNotify read FOnTooFatigued write FOnTooFatigued;
    property OnMOBDeath: TDAOCParserNotify read FOnMOBDeath write FOnMOBDeath;
    property OnExtraOutDamage: TDAOCParserNotify read FOnExtraOutDamage write FOnExtraOutDamage;
    property OnEquipWeapon: TDAOCParserNotify read FOnEquipWeapon write FOnEquipWeapon;
    property OnHealed: TDAOCParserIntNotify read FOnHealed write FOnHealed;
    property OnBracketCommand: TDAOCParseStrNotify read FOnBracketCommand write FOnBracketCommand;
    property OnTradeskillCapped: TDAOCParserNotify read FOnTradeskillCapped write FOnTradeskillCapped;
    property OnCurrencyChange: TDAOCParserCurrencyChangeNotify read FOnCurrencyChange write FOnCurrencyChange; 
  end;

implementation

{$IFDEF LINUX}
function GetTickCount : Cardinal;
var
  tod:  timeval;
  tz:   timezone;
begin
  Result := (tod.tv_sec * 1000) + (tod.tv_usec div 1000);
end;
{$ENDIF}

function ParseWord(const sLine: string; var iStartPos: integer) : string;
begin
  Result := '';
  while (iStartPos <= Length(sLine)) and
    not (sLine[iStartPos] in ['0'..'9', 'A'..'Z', 'a'..'z']) do
    inc(iStartPos);

  while (iStartPos <= Length(sLine)) and
    (sLine[iStartPos] in ['0'..'9', 'A'..'Z', 'a'..'z']) do begin
    Result := Result + sLine[iStartPos];
    inc(iStartPos);
  end;
end;

function ParseInt(const sLine: string; var iPos: integer) : integer;
begin
  Result := 0;
  while (iPos <= Length(sLine)) and
    not (sLine[iPos] in ['0'..'9']) do
    inc(iPos);

  while (iPos <= Length(sLine)) and
    (sLine[iPos] in ['0'..'9']) do begin
    Result := (Result * 10) + (ord(sLine[iPos]) - ord('0'));
    inc(iPos);
  end;    { while }
end;

function ParseIntWCommas(const sLine: string; var iPos: integer) : integer;
begin
  Result := 0;
  while (iPos <= Length(sLine)) and
    not (sLine[iPos] in ['0'..'9']) do
    inc(iPos);

  while (iPos <= Length(sLine)) and
    (sLine[iPos] in ['0'..'9', ',']) do begin
    if sLine[iPos] <> ',' then
      Result := (Result * 10) + (ord(sLine[iPos]) - ord('0'));
    inc(iPos);
  end;    { while }
end;

function RemoveThe(const sLine: string) : string;
begin
  if StrLIComp(PChar(Pointer(sLine)), 'the ', 4) = 0 then
    Result := copy(sLine, 5, Length(sLine) - 4)
  else
    Result := sLine;
end;

function ParseCurrencyList(const ALine: string; var APos: integer) : integer;
const
  COPPER_PER_SILVER = 100;
  COPPER_PER_GOLD = 10000;
  COPPER_PER_PLATINUM = 10000000;
  COPPER_PER_MITHRIL = 10000000000;
var
  iValue:   integer;
  sCoin:    string;
  sWord:    string;
  bRecognizedWord: boolean;
begin
  Result := 0;
  bRecognizedWord := true;

  repeat
      { can't use parseint because when we get past the currency list, parseint
        will parse to the end of the line }
    sWord := ParseWord(ALine, APos);
    iValue := StrToIntDef(sWord, -1);
    if iValue = -1 then
      bRecognizedWord := false
    else begin
      sCoin := ParseWord(ALine, APos);
      if AnsiSameText(sCoin, 'copper') then
        inc(Result, iValue)
      else if AnsiSameText(sCoin, 'silver') then
        inc(Result, iValue * COPPER_PER_SILVER)
      else if AnsiSameText(sCoin, 'gold') then
        inc(Result, iValue * COPPER_PER_GOLD)
      else if AnsiSameText(sCoin, 'platinum') then
        inc(Result, iValue * COPPER_PER_PLATINUM)
      else if AnsiSameText(sCoin, 'mithril') then
        inc(Result, iValue * COPPER_PER_MITHRIL)
      else
        bRecognizedWord := false;
    end;
  until not bRecognizedWord;
end;

{ TDAOCChatParser }

function TDAOCChatParser.LineBeginsWith(const ABegins: string) : boolean;
begin
  Result := StrLComp(PChar(Pointer(FCurrentLine)) + 11, PChar(Pointer(ABegins)),
    Length(ABegins)) = 0;
end;

function TDAOCChatParser.LineEndsWith(const AEnds: string) : boolean;
begin
  Result := (Length(FCurrentLine) >= Length(AEnds)) and
    (StrLComp(PChar(Pointer(FCurrentLine)) + Length(FCurrentLine) - Length(AEnds),
      PChar(Pointer(AEnds)), Length(AEnds)) = 0);
end;

function TDAOCChatParser.LineContains(const AContains: string) : boolean;
begin
  Result := Pos(AContains, FCurrentLine) <> 0;
end;

procedure TDAOCChatParser.ParseInBlock;
var
  sMob:  string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 12, Length(FCurrentLine) - 12 - 35));
  FAttackStats.FindOrAddMob(sMob);
  inc(FAttackStats.LastMob.FBlocks);

  if Assigned(FOnInBlock) then
    FOnInBlock(Self);

  DoOnInAttack;
  DoOnAttackStatsChg;
end;

procedure TDAOCChatParser.ParseInEvade;
var
  sMob:  string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 12, Length(FCurrentLine) - 12 - 35));
  FAttackStats.FindOrAddMob(sMob);
  inc(FAttackStats.LastMob.FEvades);

  if Assigned(FOnInEvade) then
    FOnInEvade(Self);

  DoOnInAttack;
  DoOnAttackStatsChg;
end;

procedure TDAOCChatParser.ParseInMiss;
var
  sMob:  string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 12, Length(FCurrentLine) - 12 - 23));
  FAttackStats.FindOrAddMob(sMob);
  inc(FAttackStats.LastMob.FInMisses);

  if Assigned(FOnInMiss) then
    FOnInMiss(Self);
    
  DoOnInAttack;
  DoOnAttackStatsChg;
end;

procedure TDAOCChatParser.ParseInParry;
var
  sMob:  string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 12, Length(FCurrentLine) - 12 - 35));
  FAttackStats.FindOrAddMob(sMob);
  inc(FAttackStats.LastMob.FParries);

  if Assigned(FOnInParry) then
    FOnInParry(Self);

  DoOnInAttack;
  DoOnAttackStatsChg;
end;

constructor TDAOCChatParser.Create;
begin
  inherited Create;

  FZoneMapList := TZoneMapList.Create;
  FAttackStats := TAttackStatsList.Create;
  FDropItems := TDropItemList.Create;
end;

destructor TDAOCChatParser.Destroy;
begin
  FZoneMapList.Free;
  FZoneMapList := nil;

  FAttackStats.Free;
  FAttackStats := nil;

  FDropItems.Free;
  FDropItems := nil;
  
  inherited Destroy;
end;

procedure TDAOCChatParser.DoEnterZone(const AZone: string);
begin
  FZone := AZone;
  FZoneValid := true;

  FZoneMapList.Load(ZoneFileName);

  if Assigned(FOnEnterZone) then
    FOnEnterZone(Self);
end;

procedure TDAOCChatParser.DoOnAttackStatsChg;
begin
  if Assigned(FOnAttackStatsChange) then
    FOnAttackStatsChange(Self);
end;

procedure TDAOCChatParser.DoOnDeath;
begin
  if Assigned(FOnDeath) then
    FOnDeath(Self);
end;

procedure TDAOCChatParser.DoOnInAttack;
var
  dtAttack:         TDateTime;
  dtDiff:           TDateTime;
  iInAttackDelay:   integer;
begin
    { AttackStats.LastMOB MUST be set before entering }
  dtAttack := TimeStamp;

  with FAttackStats.LastMOB do begin
    FMOBLastAttackTicks := GetTickCount;

    dtDiff := dtAttack - FMOBLastAttackDT;
      { if we have a last attack, the attack was not right now, and it was less
        than 10s ago figure out the delay }
    if (FMOBLastAttackDT <> 0) and (dtDiff > 0) and (dtDiff < (1 / 24 / 60 / 6)) then
       { iInAttackDelay is in seconds }
      iInAttackDelay := Round((dtAttack - FMOBLastAttackDT) / (1 / 24 / 60 / 60))
    else
      iInAttackDelay := 0;

    FMOBLastAttackDT := dtAttack;

    if iInAttackDelay <> 0 then
      IncludeInAttackDelay(iInAttackDelay);
  end;  { with LastMob }

  // if FAttackStats.LastMOB.FMOBLastAttackTicks = 0 then
  //   FAttackStats.LastMOB.FMOBLastAttackTicks := dwTickCount
  // else if dwTickCount - FAttackStats.LastMOB.FMOBLastAttackTicks > 100 then begin
  //  FAttackStats.LastMOB.FMOBAttackDelay := dwTickCount -
  //    FAttackStats.LastMOB.FMOBLastAttackTicks;
  //   FAttackStats.LastMOB.FMOBLastAttackTicks := dwTickCount;
  // end;

  if Assigned(FOnInAttack) then
    FOnInAttack(Self);
end;

procedure TDAOCChatParser.DoOnOutAttack;
var
  dtAttack:         TDateTime;
  iOutAttackDelay:  integer;
begin
  dtAttack := TimeStamp;
  FLastOutAttackTicks := GetTickCount;

    { we update our attack delay if we have a previous attack and that attack
      was within 10 seconds of now (needed if we started a new battle since then }
  if (FLastOutAttackDT <> 0) and (dtAttack - FLastOutAttackDT < (1 / 24 / 60 / 6)) then
      { iOutAttackDelay is in seconds }
    iOutAttackDelay := Round((dtAttack - FLastOutAttackDT) / (1 / 24 / 60 / 60))
  else
    iOutAttackDelay := 0;
  FLastOutAttackDT := dtAttack;

  if iOutAttackDelay <> 0 then
    FAttackStats.IncludeOutAttackDelay(iOutAttackDelay);

  if Assigned(FOnOutAttack) then
    FOnOutAttack(Self);
end;

procedure TDAOCChatParser.DoOnStandUp;
begin
  if Assigned(FOnStandUp) then
    FOnStandUp(Self);
end;

procedure TDAOCChatParser.DoStyleFailure;
begin
  if Assigned(FOnStyleExecuteFailure) then
    FOnStyleExecuteFailure(Self);

  FAttackStats.LastMob.OutDamage.ResetUpcomingStyle;
end;

procedure TDAOCChatParser.DoTarget(const ATarget: string);
begin
  FTarget := ATarget;
  if Assigned(FOnTargetChange) then
    FOnTargetChange(Self);

  FAttackStats.FindOrAddMob(FTarget);
end;

function TDAOCChatParser.GetTimeStamp: TDateTime;
begin
  Result := FDate + FTime;
end;

function TDAOCChatParser.MonthToInt(const sMonth: string): integer;
const
  MONTH_STRINGS: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );
begin
  for Result := low(MONTH_STRINGS) to high(MONTH_STRINGS) do
    if sMonth = MONTH_STRINGS[Result] then
      exit;

  Result := 12;
end;

procedure TDAOCChatParser.ParseAttackDamage;
var
  iPos:   integer;
  sMob:   string;
begin
    { [23:41:17] You attack the werewolf guard with your sword and hit for 28 (+3) damage! }
  iPos := Pos(' with your ', FCurrentLine);
  if iPos > 0 then begin
    sMob := RemoveThe(copy(FCurrentLine, 23, iPos - 23));
    FAttackStats.FindOrAddMob(sMob);
  end;

  iPos := Pos(' and hit for ', FCurrentLine);
  if iPos > 0 then begin
    inc(iPos, 13);
    FAttackStats.LastMob.OutDamage.AddAttack(ParseInt(FCurrentLine, iPos), dtMelee);
  end;

  DoOnOutAttack;

    { if effect damage follows this, it is a Proc }
  FEffectDamageType := edtProc;
end;

procedure TDAOCChatParser.ParseChatLogOpenClose;
begin
    { *** Chat Log Opened: Sat Oct 20 15:27:35 2001 }
  FDate := EncodeDate(
    StrToIntDef(copy(FCurrentLine, Length(FCurrentLine) - 3, 4), 1899),  // yyyy
    MonthToInt(copy(FCurrentLine, Length(FCurrentLine) - 19, 3)),  // mmm
    StrToIntDef(copy(FCurrentLine, Length(FCurrentLine) - 15, 2), 31)  // dd
  );

  FTime := EncodeTime(
    StrToIntDef(copy(FCurrentLine, Length(FCurrentLine) - 12, 2), 0),  // hh
    StrToIntDef(copy(FCurrentLine, Length(FCurrentLine) - 9, 2), 0),  // mm
    StrToIntDef(copy(FCurrentLine, Length(FCurrentLine) - 6, 2), 0),  // ss
    0
  );

  if copy(FCurrentLine, 14, 6) = 'Opened' then begin
    FInSession := true;
    if Assigned(FOnSessionOpen) then
      FOnSessionOpen(Self)
  end
  else begin
    FInSession := false;
    if Assigned(FOnSessionClose) then
      FOnSessionClose(Self)
  end;
end;

procedure TDAOCChatParser.ParseConsider;
var
  pPeriod:  PChar;
  sTarget:  string;
begin
    { [23:37:32] You examine the werewolf warder.  It is aggressive towards you! }
    { we can't just do a Pos('.') on the whole string because some people use
      the period as a time separator }
  pPeriod := StrScan(@FCurrentLine[11], '.');
  if not Assigned(pPeriod) then
    exit;

    { some targets don't have a 'the' (ie Lords in Varul.) }
  sTarget := RemoveThe(copy(FCurrentLine, 24, pPeriod - PChar(FCurrentLine) - 23));
  DoTarget(sTarget);
end;

procedure TDAOCChatParser.ParseDrop;
var
  sMob:   string;
  sItem:  string;
  iDropPos: integer;
begin
  iDropPos := Pos(' drops ', FCurrentLine);
  sMob := RemoveThe(copy(FCurrentLine, 12, iDropPos - 12));

  sItem := copy(FCurrentLine, iDropPos + 7, Length(FCurrentLine) - iDropPos - 7);
    { if the item begins with a cut that out }
  if (sItem <> '') and (sItem[1] = 'a') then
    Delete(sItem, 1, 2);

    { sometimes there is an extra space at the beginning }
  sItem := Trim(sItem);

  FDropItems.FindOrAddItem(sItem, sMob);

  if Assigned(FOnItemDropped) then
    FOnItemDropped(Self);
end;

procedure TDAOCChatParser.ParseEnterLeaveRegion;
begin
    { [15:32:20] (Region) You have left Huginfell. }
  if StrLComp(PChar(Pointer(FCurrentLine)) + 29, 'left', 4) = 0 then begin
    if Assigned(FOnLeaveZone) then
      FOnLeaveZone(Self);
    FZone := '';
    FZoneValid := true;
  end

    { [15:32:10] (Region) You have entered Huginfell. }
  else
    DoEnterZone(copy(FCurrentLine, 38, Length(FCurrentLine) - 38));
end;

procedure TDAOCChatParser.ParseExp;
var
  iExp:   integer;
  iPos:   integer;
begin
    { You get nnnnn experience points. (mmmm bonus) }
  iPos := 11 + 8;
  iExp := ParseIntWCommas(FCurrentLine, iPos);

  if iExp = 0 then begin
    ParseItemPickup;
    exit;
  end;

  if Pos(' experience points', FCurrentLine) > 0 then begin
    if Assigned(FAttackStats.LastMOB) then
      FAttackStats.LastMOB.FLastExperience := iExp;

    inc(FExperience, iExp);

      { this can't go in the death handler, because we see other peoples's mob death lines }
    if Assigned(FAttackStats.LastMOB) then begin
      FAttackStats.LastMOB.FMOBLastAttackTicks := 0;
      inc(FAttackStats.LastMOB.FKillCount);
    end;

    if Assigned(FOnExperienceChange) then
      FOnExperienceChange(Self);
  end

  else if Pos(' realm points', FCurrentLine) > 0 then begin
    inc(FRealmPoints, iExp);

    if Assigned(FOnRealmPointsChange) then
      FOnRealmPointsChange(Self, iExp)
  end

  else if Pos(' Specialization Points ', FCurrentLine) > 0 then 

  else begin { ' bounty points' }
    inc(FBountyPoints, iExp);

    if Assigned(FOnBountyPointsChange) then
      FOnBountyPointsChange(Self, iExp);
  end
end;

procedure TDAOCChatParser.ParseItemPickup;
var
  iPos:   integer;
  sItem:  string;
begin
    { [15:53:31] You get a pine tree amulet and put it in your backpack. }
  iPos := Pos(' and put it in your backpack.', FCurrentLine);
  if iPos = 0 then
    exit;

  sItem := copy(FCurrentLine, 22, iPos - 22);
  FDropItems.FindOrAddItem(sItem, '*');

  if Assigned(FOnItemPickUp) then
    FOnItemPickUp(Self);
end;

procedure TDAOCChatParser.ParseLoc;
var
  iPos:   integer;
  sZoneLine:  string;
  sTmpLine:   string;
begin
  sTmpLine := copy(FCurrentLine, 16, Length(FCurrentLine));

  iPos := Pos(':', sTmpLine);
    { [19:37:41]  In Myrkwood Forest: loc=3813,16756,5317 dir=178 }
  sZoneLine := copy(sTmpLine, 1, iPos - 1);

  inc(iPos, 6);
  FLocation.X := ParseInt(sTmpLine, iPos);
  inc(iPos, 1);
  FLocation.Y := ParseInt(sTmpLine, iPos);

  if sZoneLine <> FZone then
    DoEnterZone(sZoneLine);

  if Assigned(FOnLocationChange) then
    FOnLocationChange(Self);
end;

procedure TDAOCChatParser.ParseOutMiss;
begin
    { TODO: parse the mob name from this line, this will throw an AV if log
      starts with this line }
    { Note that the outwasblocked and outwasparried handlers use this code
      so that will have to be changed if you're just looking for misses here }
  FAttackStats.LastMob.OutDamage.AddAttack(0, dtMelee);

  DoStyleFailure;
  DoOnOutAttack;
end;

procedure TDAOCChatParser.ParsePrepareStyle;
begin
  if not Assigned(FAttackStats.LastMob) then
    exit;
    
    { [23:37:44] You prepare to perform a Assault! }
  FAttackStats.LastMob.OutDamage.FindOrAddStyle(copy(FCurrentLine, 37, Length(FCurrentLine) - 37));

  if Assigned(FOnStylePrepared) then
    FOnStylePrepared(Self);
end;

procedure TDAOCChatParser.ParseSellItem;
var
  iPos:   integer;
  iValue: integer;
  sItem:  string;
  I:      integer;
  bFoundItem:   boolean;
begin
    { [18:36:14] Wolgrun gives you 1130 copper for the lapis lazuli stone. }
    { [13:07:11] xxx gives you 5 gold, 74 silver, 68 copper pieces for the yyy }
  iPos := Pos(' gives you ', FCurrentLine);
  if iPos = -1 then
    exit;

  inc(iPos, 11);
  iValue := ParseCurrencyList(FCurrentLine, iPos);
    { ParseCurrencyList will parse up to and including 'pieces' }
  if ParseWord(FCurrentLine, iPos) <> 'for' then
    exit;
  if ParseWord(FCurrentLine, iPos) <> 'the' then
    exit;

  sItem := copy(FCurrentLine, iPos + 1, Length(FCurrentLine) - iPos - 1);

  bFoundItem := false;
  for I := 0 to FDropItems.Count - 1 do
    if AnsiSameText(FDropItems[I].Name, sItem) then begin
      FDropItems[I].SetSellValue(iValue);
      bFoundItem := true;
    end;

  if not bFoundItem then
    FDropItems.FindOrAddItem(sItem, '').SetSellValue(iValue);

  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrSellItem);
end;

procedure TDAOCChatParser.ParseStyleDamage;
var
  iPos:   integer;
  sStyle: string;
begin
  if not Assigned(FAttackStats.LastMob) then
    exit;

    { [19:51:15] You perform your Assault perfectly. (+8) }
  iPos := Pos(' perfectly.', FCurrentLine);
  if iPos = 0 then
    iPos := Pos(' perfectly!', FCurrentLine);
  if iPos > 0 then begin
    sStyle := copy(FCurrentLine, 29, iPos - 29);
    FAttackStats.LastMOB.OutDamage.FindOrAddStyle(sStyle);
    // ParseWord(FCurrentLine, iPos);  // perfectly

    if Assigned(FOnStyleExecuteSuccess) then
      FOnStyleExecuteSuccess(Self);
  end;
end;

procedure TDAOCChatParser.ParseTarget;
var
  sTmpLine:   string;
begin
    { [23:53:15] You target [Clug] }
  sTmpLine := copy(FCurrentLine, 23, Length(FCurrentLine));
    { the line might include a "the" }
  sTmpLine := RemoveThe(sTmpLine);

    { TODO: This throws off the autohunt, when you click a mob, it goes
      asFoundMob -> [pull] -> asPulled
      then we get the line you examine the ... and it does it again }
  // DoTarget(copy(sTmpLine, 2, Length(sTmpLine) - 2));
end;

procedure TDAOCChatParser.ParseInAttack;
var
  iPos:   integer;
  sMob:   string;
  pStats: TAttackStats;

    procedure UpdateInDmg(var pDmg: TIncomingDamage; iStart: integer);
    var
      iDmg:   integer;
    begin
      inc(pDmg.Count);
      iDmg := 0;
      while FCurrentLine[iStart] in ['0'..'9'] do begin
        iDmg := (iDmg * 10) + ord(FCurrentLine[iStart]) - ord('0');
        inc(iStart);
      end;
      inc(pDmg.Damage, iDmg);
    end;

begin
    { [15:31:23] The svartalf watcher hits your torso for 19 (+1) damage! }
  iPos := Pos(' hits your ', FCurrentLine);
  if iPos <> 0 then begin
    sMob := RemoveThe(copy(FCurrentLine, 12, iPos - 12));
    pStats := FAttackStats.FindOrAddMob(sMob);
    case FCurrentLine[iPos + 11] of
      'a':  { arm }
          UpdateInDmg(pStats.FInArm, iPos + 19);
      'f':  { foot }
          UpdateInDmg(pStats.FInFoot, iPos + 20);
      'h':  { hand / head }
        if FCurrentLine[iPos + 12] = 'a' then  { hand }
          UpdateInDmg(pStats.FInHand, iPos + 20)
        else  { head }
          UpdateInDmg(pStats.FInHead, iPos + 20);
      'l':  { leg }
          UpdateInDmg(pStats.FInLeg, iPos + 19);
      't':  { torso }
          UpdateInDmg(pStats.FInTorso, iPos + 21);
    end;  { case }

    if Assigned(FOnInAttackHit) then
      FOnInAttackHit(Self);

    DoOnInAttack;
    DoOnAttackStatsChg;

      { if effect damage follows this, it is a Reverse Proc } 
    FEffectDamageType := edtRProc;
  end;  { attack hit }
end;

procedure TDAOCChatParser.ParseTimeStamp;
var
  timeLine: TDateTime;
begin
    { [15:27:35] Charname was just killed by a grass viper! }
  timeLine := EncodeTime(
    StrToIntDef(copy(FCurrentLine, 2, 2), 0),  // hh
    StrToIntDef(copy(FCurrentLine, 5, 2), 0),  // mm
    StrToIntDef(copy(FCurrentLine, 8, 2), 0),  // ss
    0
  );

    { check to see if we rolled over a day }
  if timeLine < FTime then
    FDate := FDate + 1;

  FTime := timeLine;
end;

(*****
function TDAOCChatParser.ReadLn: string;
var
  FCurrentLine:  string;
  cIn:    char;
  cBuff:  array[0..1] of char;
  iBufPos:  integer;
begin
  if not Assigned(FChatStream) then
    exit;

  Result := '';
  cIn := #0;
  iBufPos := 0;

  while FChatStream.Position < (FChatStream.Size - 1) do begin
    FChatStream.Read(cBuff, sizeof(cBuff));
    for iBufPos := low(cBuff) to high(cBuff) do begin
      cIn := cBuff[iBufPos];
      if cIn = #13 then
        break;
      AppendStr(FCurrentLine, cIn);
    end;    { for }

    if cIn = #13 then
      break;
  end;    { while }

  if cIn = #13 then begin
    Result := FPrevLineBuffer + FCurrentLine;
    FPrevLineBuffer := '';
      { get the LF too, we have the LF in cBuff if our pos is < high, else
        read it from the file }
    if iBufPos = high(cBuff) then
      FChatStream.Read(cIn, sizeof(cIn));
  end
  else
    FPrevLineBuffer := FCurrentLine;
end;
****)

procedure TDAOCChatParser.ParseTradeskillSuccess;
var
  iPos:     integer;
  iQuality: integer;
begin
    { [10:41:55] You successfully make the Dusty Essence Jewel!  (95) }
  iPos := Pos('!  (', FCurrentLine);
  if (iPos > 0) then begin
    inc(iPos, 4);
    iQuality := ParseInt(FCurrentLine, iPos);
  end
  else
    iQuality := 0;

  inc(FTradeSkillQualityDistribution[iQuality]);
  inc(FTradeSkillQualityDistribution[101]);  // 101 holds total tries

    { FTradeSkillStreaks contains the number of successes of below that quality
      since the last success of that quality or above }
  for iPos := 94 to 100 do begin
    if iQuality >= iPos then
      FTradeSkillStreaks[iPos] := 0
    else
      Inc(FTradeSkillStreaks[iPos]);
  end;

  if Assigned(FOnTradeSkillSuccess) then
    FOnTradeSkillSuccess(Self, iQuality);

  if iQuality = 100 then
    if Assigned(FOnTradeskillMasterpiece) then
      FOnTradeskillMasterpiece(Self);
end;

procedure TDAOCChatParser.ParseTradeskillTask;
var
  iPos:   integer;
  sWord:  string;
begin
    { Asre says, "Gothi of Modi wants a fine alloy bearded axe made for him. ..." }
  FTradeSkillNPC := '';
  FTradeSkillItem := '';

  iPos := Pos('"', FCurrentLine);
  sWord := '';

  sWord := ParseWord(FCurrentLine, iPos);
  while sWord <> 'wants' do begin
    if FTradeSkillNPC <> '' then
      FTradeSkillNPC := FTradeSkillNPC + ' ' + sWord
    else
      FTradeSkillNPC := sWord;
    sWord := ParseWord(FCurrentLine, iPos);
  end;

    { this should be 'a' }
  ParseWord(FCurrentLine, iPos);

  sWord := ParseWord(FCurrentLine, iPos);
  while sWord <> 'made' do begin
    if FTradeSkillItem <> '' then
      FTradeSkillItem := FTradeSkillItem + ' ' + sWord
    else
      FTradeSkillItem := sWord;
    sWord := ParseWord(FCurrentLine, iPos);
  end;

  if Assigned(FOnTradeSkillTask) then
    FOnTradeSkillTask(Self);
end;

procedure TDAOCChatParser.ParseTradeskillTask2;
var
  iPos:   integer;
  sWord:  string;
begin
    { You have been asked to make a fine alloy bearded axe for Synna. " }
  FTradeSkillNPC := '';
  FTradeSkillItem := '';

  iPos := 42;
  sWord := '';

  sWord := ParseWord(FCurrentLine, iPos);
  while sWord <> 'for' do begin
    if FTradeSkillItem <> '' then
      FTradeSkillItem := FTradeSkillItem + ' ' + sWord
    else
      FTradeSkillItem := sWord;
    sWord := ParseWord(FCurrentLine, iPos);
  end;

  FTradeSkillNPC := copy(FCurrentLine, iPos + 1, Length(FCurrentLine) - iPos - 1);
  
  if Assigned(FOnTradeSkillTask) then
    FOnTradeSkillTask(Self);
end;

procedure TDAOCChatParser.ParseYouSay;
begin
    { [23:55:30] @@You say, "hey you're right" }
  FLastYouSay := copy(FCurrentLine, 24, Length(FCurrentLine) - 23 - 1);
  if Assigned(FOnYouSay) then
    FOnYouSay(Self);
end;

procedure TDAOCChatParser.Reset;
begin
  FPrevLineBuffer := '';
  FZoneValid := false;
  FZone := '';
  FDate := 0;
  FTime := 0;
  FExperience := 0;
  FLastOutAttackDT := 0;
  FLastOutAttackTicks := 0;
  FAttackStats.Clear;
  FDropItems.Clear;
  ResetQualityDistribution;
end;

procedure TDAOCChatParser.ProcessLine(const ALine: string);
begin
  FCurrentLine := ALine;
  if ALine = '' then
    exit;

    { *** Chat Log Opened: Sat Oct 20 15:27:35 2001 }
  if LineBeginsWith('*** ') then
    ParseChatLogOpenClose
  else begin
      { [15:27:35] Charname was just killed by a grass viper! }
    ParseTimeStamp;

      { [15:32:10] (Region) You have entered Huginfell. }
    if LineBeginsWith('(Region)') then
      ParseEnterLeaveRegion
      { [23:53:06]  In Myrkwood Forest: loc=32614,5406,4740 dir=282 }
    else if LineBeginsWith(' In ') then
      ParseLoc
      { [23:55:30] @@You say, "xxx" }
    else if LineBeginsWith('@@You say, "') then
      ParseYouSay
      { [09:14:22] @@You send, "I can have that mith cleaver made for 110g" to Charname }
    else if LineBeginsWith('@@You send, "') then
      ParseYouSend
      { [15:12:09] @@[Guild] Charname: "*gives you the finger as he passes*" }
    else if LineBeginsWith('@@[Guild] ') then
      ParseGuildChat
      { [19:17:38] You target the [Kobold Helen] }
    else if LineBeginsWith('You target ') then
      ParseTarget
    else if LineEndsWith(' attacks you and you evade the blow!') then
      ParseInEvade
    else if LineEndsWith(' attacks you and you parry the blow!') then
      ParseInParry
    else if LineEndsWith(' attacks you and you block the blow!') then
      ParseInBlock
    else if LineEndsWith(' attacks you and misses!') then
      ParseInMiss
    else if LineEndsWith(' evades your attack!') then
      ParseOutMiss
    else if LineEndsWith(' parries your attack!') then
      ParseOutMiss
    else if LineBeginsWith('You have been asked to make a ') then
      ParseTradeskillTask2
    else if LineBeginsWith('You prepare to perform a ') then
      ParsePrepareStyle
    else if LineBeginsWith('You fail to execute your ') then
      DoStyleFailure
    else if LineBeginsWith('You perform your ') then
      ParseStyleDamage
    else if LineBeginsWith('You miss!') then
      ParseOutMiss
    else if LineBeginsWith('You attack ') then
      ParseAttackDamage
    else if LineBeginsWith('You shoot ') then
      ParseBowDamage
    else if LineBeginsWith('You get ') then
      ParseExp
    else if LineBeginsWith('You have failed your task!') then
      ParseFailedTask
    else if LineBeginsWith('You successfully make ') then
      ParseTradeskillSuccess
    else if LineBeginsWith('You fail to make ') then
      ParseTradeskillFailure
    else if LineBeginsWith('You must talk to your Order Trainer to raise ') then
      ParseTradeskillCap
      { [15:24:02] You hit for 225 (+45) damage! }
    else if LineBeginsWith('You hit for ') then
      ParseEffectDamage
      { [15:24:02] You cast a Greater Rune of Obscurity Spell! }
    else if LineBeginsWith('You cast a ') then
      ParseCastSpell
      { [18:14:54] You have died.  Type /release to return to your last bind point. }
    else if LineBeginsWith('You have died. ') then
      DoOnDeath
      { [11:13:13] Your target is not in view! }
    else if LineBeginsWith('Your target is not in view!') then
      DoOnNotInView
      { [23:20:51] You must be in combat mode to select your next attack style! }
    else if LineBeginsWith('You must be in combat mode to select your next attack style!') then
      DoOnNotInCombatMode
      { [23:37:32] You examine the werewolf warder.  It is aggressive towards you! }
    else if LineBeginsWith('You examine ') then
      ParseConsider
      { [19:35:33] You stand up. }
    else if LineBeginsWith('You stand up.') then
      DoOnStandUp
      { [08:50:39]  Hit ESCAPE to exit. }
    else if LineBeginsWith(' Hit ESCAPE to exit.') then
      DoOnLinkDead
      { [08:22:02] The wolfaur quixot dies! }
    else if LineEndsWith(' dies!') then
      ParseMOBDies
      { [19:10:56] You enter combat mode and target [the hagbui squire] }
    else if LineBeginsWith('You enter combat mode and target [') then
      ParseEnterCombatMode
      { [21:14:13] You are too fatigued to perform the Assault style! }
    else if LineBeginsWith('You are too fatigued to perform ') then
      ParseTooFatigued
      { [21:55:51] You wield the Forged Darksteel Bastard Sword in your right hand. }
    else if LineBeginsWith('You wield the ') then
      ParseWeaponEquiped
      { [09:12:19] You must perform the Assault style before this one! }
    else if LineBeginsWith('You must perform the ') then
      ParseStylePrereqFailure
      { [21:51:54] You just bought a steel bladed claw greave for 2 gold, 8 silver pieces. }
    else if LineBeginsWith('You just bought ') then
      ParseBuyItem
      { [22:46:53] You give Morthwyl 1 gold, 77 silver, 64 copper pieces. }
    else if LineBeginsWith('You give ') and LineEndsWith(' pieces.') then
      ParseRepairs
      { [23:51:40] You give him a donation of 5 gold, 91 silver, 95 copper pieces. }
    else if LineBeginsWith('You give him a donation of ') then
      ParseBuyBackCon
      { [10:50:16] Your share of the loot is 67 silver, and 91 copper pieces. }
    else if LineBeginsWith('Your share of the loot is ') then
      ParseCoinGroup
      { [22:03:13] You pick up 1 silver, and 59 copper pieces. }
    else if LineBeginsWith('You pick up ') and LineEndsWith(' pieces.') then
      ParseCoinSolo
    else if LineBeginsWith('[ALL]:  ]') then
      ParseBracketCommand
      { [08:23:49] You hit wolfaur quixot for 27 extra damage! }
    else if LineEndsWith(' extra damage!') then
      ParseExtraOutDamage
      { [10:14:40] You are healed by Snu for 20 hit points. }
    else if LineBeginsWith('You are healed by ') then
      ParseHealed
//    else if LineEndsWith(' and return to me for your reward.  Good luck!"') then
//      ParseKillTask
//    else if LineBeginsWith('You must not return to ') and LineEndsWith(' to receive your reward!') then
//      ParseKillTaskComplete
//    else if LineContains(', it''s good to see adventurers willing to help out the realm in such times.  Search to the ') then
//      ParseKillTask
    else if LineContains(' gives you ') then
      ParseSellItem
    else if LineContains(' made for ') then
      ParseTradeskillTask
      { [21:17:35] The wolfaur quixot hits your arm for 63 (-20) damage! }
    else if LineContains(' hits your ') then
      ParseInAttack
      { [15:31:20] The svartalf watcher drops a onyx. }
    else if LineContains(' drops ') then
      ParseDrop
  end;
end;

function TDAOCChatParser.ZoneFileName: string;
var
  I:    integer;
begin
  SetLength(Result, Length(FZone));
  
  for I := 1 to Length(FZone) do
    if FZone[I] = ' ' then
      Result[I] := '_'
    else
      Result[I] := FZone[I];
end;

procedure TDAOCChatParser.DoOnNotInView;
begin
  if Assigned(FOnTargetNotInView) then
    FOnTargetNotInView(Self);
end;

procedure TDAOCChatParser.DoOnNotInCombatMode;
begin
  if Assigned(FOnNotInCombatMode) then
    FOnNotInCombatMode(Self);
end;

procedure TDAOCChatParser.ParseTradeskillFailure;
begin
  if LineEndsWith(' and lose some materials!') then begin
    if Assigned(FOnTradeSkillFailureWithLoss) then
      FOnTradeSkillFailureWithLoss(Self);
  end
  else
    if Assigned(FOnTradeSkillFailure) then
      FOnTradeSkillFailure(Self);
end;

procedure TDAOCChatParser.DoOnLinkDead;
begin
  if Assigned(FOnLinkDead) then
    FOnLinkDead(Self);
end;

procedure TDAOCChatParser.ParseEnterCombatMode;
var
  sMob:   string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 46, Length(FCurrentLine) - 46));
  FAttackStats.FindOrAddMob(sMob);

  if Assigned(FOnEnterCombatMode) then
    FOnEnterCombatMode(Self);
end;

procedure TDAOCChatParser.ParseTooFatigued;
begin
  if Assigned(FAttackStats.LastMOB) then
    FAttackStats.LastMOB.OutDamage.ResetUpcomingStyle;
    
  if Assigned(FOnTooFatigued) then
    FOnTooFatigued(Self);
end;

procedure TDAOCChatParser.ParseMOBDies;
var
  sMob:   string;
begin
  sMob := RemoveThe(copy(FCurrentLine, 12, Length(FCurrentLine) - 11 - 6));
  FAttackStats.FindOrAddMob(sMob);

  if Assigned(FOnMOBDeath) then
    FOnMOBDeath(Self);
end;

procedure TDAOCChatParser.ParseExtraOutDamage;
var
  sMob: string;
  iPos: integer;
  iDmg: integer;
begin
  iPos := Pos(' for ', FCurrentLine);
  if iPos <> 0 then begin
    sMob := RemoveThe(copy(FCurrentLine, 20, iPos - 20));
    FAttackStats.FindOrAddMob(sMob);
    inc(iPos, 5);
    iDmg := ParseInt(FCurrentLine, iPos);

    if Assigned(FAttackStats.LastMOB.OutDamage.LastStyle) then
      with FAttackStats.LastMOB.OutDamage.LastStyle do begin
        inc(FTotalDamage, iDmg);
        if FLastSwingDmg + iDmg > FMaxDamage then
          FMaxDamage := FLastSwingDmg + iDmg;
          { If we had a mindmg of say 100, and the last hit was a 99, it would
            set MinDmg and then we'd bump it up here above the previous MinDmg.
            Restore previous MinDmg in that case }
        if FLastSwingSetMinDmg then begin
          FMinDamage := FLastSwingDmg + iDmg;
          if (FPreviousMinDmg <> 0) and (FPreviousMinDmg < FMinDamage) then
            FMinDamage := FPreviousMinDmg;
        end;
      end;  { if LastStyle / with LastStyle }
  end;  { if iPos ' for ' }

  if Assigned(FOnExtraOutDamage) then
    FOnExtraOutDamage(Self);
end;

procedure TDAOCChatParser.ParseBowDamage;
var
  iDmg:   integer;
  iPos:   integer;
  sWord:  string;
  sMob:   string;
begin
  iPos := 22;
  sWord := ParseWord(FCurrentLine, iPos);
  sMob := '';
    { [13:53:41] You shoot Charnmae with your bow and hit for 160 damage! }
  while not AnsiSameText(sWord, 'with') do begin
    if sMob <> '' then
      sMob := sMob + ' ';
    if not AnsiSameText(sWord, 'the') then
      sMob := sMob + sWord;
    sWord := ParseWord(FCurrentLine, iPos);
  end;
  FAttackStats.FindOrAddMob(sMob);

  inc(iPos, 6);  // your
  ParseWord(FCurrentLine, iPos);  // bow / crossbow
  inc(iPos, 13);  // and hit for

  iDmg := ParseInt(FCurrentLine, iPos);
  FAttackStats.LastMOB.OutDamage.AddAttack(iDmg, dtBow);
end;

procedure TDAOCChatParser.ParseWeaponEquiped;
begin
  FAttackStats.FIncludedOutDelayCnt := 0;
  FAttackStats.FOutAvgDelay := 0;

  if Assigned(FOnEquipWeapon) then
    FOnEquipWeapon(Self);
end;

procedure TDAOCChatParser.ParseStylePrereqFailure;
begin
  if Assigned(FOnStylePrereqFailure) then
    FOnStylePrereqFailure(Self);
end;

procedure TDAOCChatParser.ParseYouSend;
begin
  { [09:14:22] @@You send, "I can have that mith cleaver made for 110g" to Charname }
  ;
end;

procedure TDAOCChatParser.ParseHealed;
var
  iPos:   integer;
  iHP:    integer;
begin
    { [10:14:40] You are healed by Charname for 20 hit points. }
  iPos := 30;
  ParseWord(FCurrentLine, iPos);  // who healed
  ParseWord(FCurrentLine, iPos);  // for
  iHP := ParseInt(FCurrentLine, iPos);

  if Assigned(FOnHealed) then
    FOnHealed(Self, iHP);
end;

procedure TDAOCChatParser.ParseGuildChat;
begin
  ; // nothing yet
end;

procedure TDAOCChatParser.ParseBracketCommand;
var
  sCommandText: string;
begin
  sCommandText := copy(FCurrentLine, 21, Length(FCurrentLine) - 20);
  if Assigned(FOnBracketCommand) then
    FOnBracketCommand(Self, sCommandText);
end;

procedure TDAOCChatParser.ParseTradeskillCap;
begin
    { [16:29:25] You must talk to your Order Trainer to raise Skillname again! }
    { [11:14:32] You must talk to your Order Trainer to raise Skillname! }
  if Assigned(FOnTradeskillCapped) then
    FOnTradeskillCapped(Self);
end;

function TDAOCChatParser.GetTradeSkillQualityDistribution(
  I: integer): integer;
begin
  if I in [low(FTradeSkillQualityDistribution)..high(FTradeSkillQualityDistribution)] then
    Result := FTradeSkillQualityDistribution[I]
  else
    Result := 0;
end;

procedure TDAOCChatParser.ResetQualityDistribution;
begin
  FillChar(FTradeSkillQualityDistribution, sizeof(FTradeSkillQualityDistribution), 0);
end;

function TDAOCChatParser.GetTradeSkillStreaks(I: integer): integer;
begin
  if I in [low(FTradeSkillStreaks)..high(FTradeSkillStreaks)] then
    Result := FTradeskillStreaks[I]
  else
    Result := 0;
end;

procedure TDAOCChatParser.ParseCastSpell;
var
  sSpell:   string;
begin
    { [15:24:02] You cast a Greater Rune of Obscurity Spell! }
  sSpell := copy(FCurrentLine, 22, Length(FCurrentLine) - 22 - 1);

    { TODO: Maintain a list of cast spells.  Set LastSpell to sSpell }

    { when effect damage follows this, it is a Spell }
  FEffectDamageType := edtSpell;
end;

procedure TDAOCChatParser.ParseEffectDamage;
//var
//  iPos:     integer;
//  iDamage:  integer;
begin
      { [15:24:02] You hit for 225 (+45) damage! }
//  iPos := 24;
//  iDamage := ParseInt(FCurrentLine, iPos);

    { TODO: Update the spell / proc / proc / bleed damage with effect damage }

    { Reset the effect damage type }
  FEffectDamageType := edtUnknown;
end;

procedure TDAOCChatParser.ParseBuyBackCon;
begin
  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrBuyCon);
end;

procedure TDAOCChatParser.ParseBuyItem;
begin
  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrBuyItem);
end;

procedure TDAOCChatParser.ParseCoinGroup;
begin
  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrLootGroup);
end;

procedure TDAOCChatParser.ParseCoinSolo;
begin
  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrLootSolo);
end;

procedure TDAOCChatParser.ParseRepairs;
begin
  if Assigned(FOnCurrencyChange) then
    FOnCurrencyChange(Self, ccrRepairItem);
end;

procedure TDAOCChatParser.ParseFailedTask;
begin
  if FTradeSkillItem <> '' then begin
    FTradeSkillItem := '';
    if Assigned(FOnTradeskillTask) then
      FOnTradeskillTask(Self);
  end;
end;

{ TZoneMap }

procedure TZoneMap.Load(const AZoneName: string; AMapNo: integer);
var
  sPrefix:   string;
begin
  if AZoneName = '' then begin
    Reset;
    exit;
  end;

  with TINIFile.Create('maps\maps.ini') do begin
    sPrefix := 'Map' + IntToStr(AMapNo) + '_';

    FLeft := ReadInteger(AZoneName, sPrefix + 'Left', 0);
    FTop := ReadInteger(AZoneName, sPrefix + 'Top', 0);
    FUnitsPerPixelX := ReadInteger(AZoneName, sPrefix + 'UPPX', 1);
    FUnitsPerPixelY := ReadInteger(AZoneName, sPrefix + 'UPPY', 1);
    FFileName := ReadString(AZoneName, sPrefix + 'File', '');
    FDescription := ReadString(AZoneName, sPrefix + 'Desc', '');
    Free;
  end;    { with }
end;

procedure TZoneMap.Reset;
begin
  FTop := 0;
  FLeft := 0;
  FUnitsPerPixelY := 0;
  FUnitsPerPixelX := 0;
  FFileName := '';
end;

function TZoneMap.TranslateLocX(AX: integer): integer;
begin
  Result := FLeft + ((AX * 100) div FUnitsPerPixelX);
end;

function TZoneMap.TranslateLocY(AY: integer): integer;
begin
  Result := FTop + ((AY * 100) div FUnitsPerPixelY);
end;

{ TZoneMapList }

procedure TZoneMapList.Clear;
begin
  inherited Clear;
  FZoneName := '';
end;

function TZoneMapList.GetItems(Index: integer): TZoneMap;
begin
  result := TZoneMap(inherited Get(Index));
end;

procedure TZoneMapList.Load(const AZoneName: string);
var
  iCount:   Integer;
  tmpMap:   TZoneMap;
  I: Integer;
begin
  Clear;

  if AZoneName = '' then
    exit;

  with TINIFile.Create('maps\maps.ini') do begin
    FZoneName := ReadString('ZoneAliases', AZoneName, AZoneName);

    iCount := ReadInteger(FZoneName, 'MapCount', 0);
    for I := 0 to iCount - 1 do begin
      tmpMap := TZoneMap.Create;
      Add(tmpMap);
      tmpMap.Load(FZoneName, I);
    end;    { for I }

    Free;
  end;    { with }
end;

procedure TZoneMapList.SetZoneName(const Value: string);
begin
  Load(FZoneName);
end;

{ TAttackStats }

function TAttackStats.BlockPct: double;
var
  iDenom:   integer;
begin
  iDenom := FBlocks + FInMisses + TotalInHits;
  if iDenom > 0 then
    Result := (FBlocks * 100) / iDenom
  else
    Result := 0;
end;

procedure TAttackStats.Clear;
begin
  FEvades := 0;
  FHits := 0;
  FBlocks := 0;
  FFumbles := 0;
  FParries := 0;
  FInMisses := 0;
  FillChar(FInHead, sizeof(TIncomingDamage), 0);
  FillChar(FInHand, sizeof(TIncomingDamage), 0);
  FillChar(FInLeg, sizeof(TIncomingDamage), 0);
  FillChar(FInTorso, sizeof(TIncomingDamage), 0);
  FillChar(FInArm, sizeof(TIncomingDamage), 0);
  FillChar(FInFoot, sizeof(TIncomingDamage), 0);
  FOutDamage.Clear;
end;

constructor TAttackStats.Create;
begin
  FOutDamage := TOutgoingDmgList.Create;
  FOutDamage.FParent := Self;
end;

destructor TAttackStats.Destroy;
begin
  FOutDamage.Free;
  FOutDamage := nil;
  
  inherited Destroy;
end;

function TAttackStats.EvadePct: double;
var
  iDenom:   integer;
begin
  iDenom := FEvades + FBlocks + FParries + FInMisses + TotalInHits;
  if iDenom > 0 then
    Result := (FEvades * 100) / iDenom
  else
    Result := 0;
end;

function TAttackStats.GetInAvg(iWhich: integer): integer;
var
  pDmg:   PIncomingDamage;
begin
  case iWhich of
    0:  pDmg := @FInHead;
    1:  pDmg := @FInTorso;
    2:  pDmg := @FInHand;
    3:  pDmg := @FInArm;
    4:  pDmg := @FInLeg;
    5:  pDmg := @FInFoot;
    else
      pDmg := nil;
  end;  { case }

  if Assigned(pDmg) and (pDmg^.Count > 0) then
    Result := pDmg^.Damage div pDmg^.Count
  else
    Result := 0;
end;

function TAttackStats.TotalInDamage: integer;
begin
  Result := InHead.Damage + InTorso.Damage + InHand.Damage + InArm.Damage +
    InLeg.Damage + InFoot.Damage;
end;

function TAttackStats.GetInPct(iWhich: integer): double;
var
  iDenom:   integer;
begin
  iDenom := TotalInHits;
  if iDenom > 0 then
    case iWhich of
      0:  Result := (FInHead.Count * 100) / iDenom;
      1:  Result := (FInTorso.Count * 100) / iDenom;
      2:  Result := (FInHand.Count * 100) / iDenom;
      3:  Result := (FInArm.Count * 100) / iDenom;
      4:  Result := (FInLeg.Count * 100) / iDenom;
      5:  Result := (FInFoot.Count * 100) / iDenom;
      else
        Result := 0;
    end  { case }
  else
    Result := 0;
end;

function TAttackStats.InMissPct: double;
var
  iDenom:   integer;
begin
  iDenom := FInMisses + TotalInHits;
  if iDenom > 0 then
    Result := (FInMisses * 100) / iDenom
  else
    Result := 0;
end;

function TAttackStats.ParryPct: double;
var
  iDenom:   integer;
begin
  iDenom := FParries + FBlocks + FInMisses + TotalInHits;
  if iDenom > 0 then
    Result := (FParries * 100) / iDenom
  else
    Result := 0;
end;

function TAttackStats.TotalInAttacks: integer;
begin
  Result := FInMisses + FEvades + FBlocks + FParries + TotalInHits;
end;

function TAttackStats.TotalInHitPct: double;
var
  iDenom: integer;
begin
  iDenom := TotalInAttacks;
  if iDenom > 0 then
    Result := (TotalInHits * 100) / iDenom
  else
    Result := 0;
end;

function TAttackStats.TotalInHits: integer;
begin
  Result := FInFoot.Count + FInLeg.Count + FInHand.Count + FInHead.Count +
    FInTorso.Count + FInArm.Count;
end;

procedure TAttackStats.IncludeInAttackDelay(ADelay: integer);
begin
    { a delay is in Secs, outavgdelay is Millisec }
  FMOBAvgDelay := ((FMOBAvgDelay * FIncludedInDelayCnt) + (ADelay * 1000)) div
    (FIncludedInDelayCnt + 1);
  inc(FIncludedInDelayCnt);
end;

{ TOutgoingDamage }

function TOutgoingDamage.GetAvgDamage: integer;
begin
  if FHitCount > 0 then
    Result := FTotalDamage div FHitCount
  else
    Result := 0;
end;

function TOutgoingDamage.GetDPS: double;
var
  pDmgList:   TOutgoingDmgList;
  pAStats:    TAttackStats;
  pAStatsLst: TAttackStatsList;
begin
  Result := 0;

  pDmgList := FParent;
  if not Assigned(pDmgList) then
    exit;

  pAStats := pDmgList.FParent;
  if not Assigned(pAStats) then
    exit;

  pAStatsLst := pAStats.FParent;
  if not Assigned(pAStatsLst) then
    exit;

  if pAStatsLst.FOutAvgDelay <> 0 then
    Result := GetAvgDamage / (pAStatsLst.FOutAvgDelay / 1000)
end;

function TOutgoingDamage.GetHitPct: double;
begin
  if FCount > 0 then
    Result := (FHitCount * 100) / FCount
  else
    Result := 0;
end;

{ TOutgoingDmgList }

procedure TOutgoingDmgList.AddAttack(iDamage: integer; ADamageType: TOutgoingDamageType);
begin
  if not Assigned(FUpcomingStyle) then
    FindOrAddStyle('');

  with FUpcomingStyle do begin
    FDamageType := ADamageType;
    FLastSwingDmg := iDamage;
    FLastSwingSetMinDmg := false;
    FLastSwingSetMaxDmg := false;
    if iDamage > 0 then begin
      if FHitCount = 0 then begin
        FPreviousMinDmg := 0;
        FMinDamage := iDamage;
        FMaxDamage := iDamage;
        FLastSwingSetMinDmg := true;
        FLastSwingSetMaxDmg := true;
      end
      else begin
        if iDamage < FMinDamage then begin
          FPreviousMinDmg := FMinDamage;
          FMinDamage := iDamage;
          FLastSwingSetMinDmg := true;
        end
        else
        if iDamage > FMaxDamage then begin
          FMaxDamage := iDamage;
          FLastSwingSetMaxDmg := true;
        end
      end;

      inc(FHitCount);
      inc(FTotalDamage, iDamage);
    end;
    inc(FCount);
  end;  { with FStyleObj }

  ResetUpcomingStyle;
end;

procedure TOutgoingDmgList.Clear;
begin
  ResetUpcomingStyle;
  FUpcomingStyle := nil;

  inherited Clear;
end;

function TOutgoingDmgList.FindOrAddStyle(const AStyle: string): TOutgoingDamage;
var
  iIndex: integer;
begin
  iIndex := IndexOfStyle(AStyle);
  if iIndex = -1 then begin
    Result := TOutgoingDamage.Create;
    Result.FParent := Self;
    Add(Result);
    Result.FStyle := AStyle;
    SortByStyleName;
  end
  else
    Result := Items[iIndex];

  FLastStyle := FUpcomingStyle;
  FUpcomingStyle := Result;
end;

function TOutgoingDmgList.GetItems(Index: integer): TOutgoingDamage;
begin
  Result := TOutgoingDamage(inherited Items[Index]);
end;

function TOutgoingDmgList.IndexOfStyle(const AStyle: string): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Style = AStyle then
      exit;

  Result := -1;
end;

procedure TOutgoingDmgList.ResetUpcomingStyle;
begin
  FindOrAddStyle('');
end;

function SortStyleNameCompare(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TOutgoingDamage(Item1).Style, TOutgoingDamage(Item2).Style);
end;

procedure TOutgoingDmgList.SortByStyleName;
begin
  Sort(SortStyleNameCompare);
end;

{ TAttackStatsList }

procedure TAttackStatsList.Clear;
begin
  FLastMob := nil;
  FIncludedOutDelayCnt := 0;
  FOutAvgDelay := 0;
   
  inherited Clear;
end;

function TAttackStatsList.FindOrAddMob(const AMobName: string): TAttackStats;
begin
  Result := IndexOfMob(AMobName);
  if not Assigned(Result) then begin
    Result := TAttackStats.Create;
    Result.FMob := AMobName;
    Result.FParent := Self;
    Add(Result);
  end;

  FLastMob := Result;
end;

function TAttackStatsList.GetItems(Index: integer): TAttackStats;
begin
  Result := TAttackStats(inherited Items[Index]);
end;

procedure TAttackStatsList.IncludeOutAttackDelay(ADelay: integer);
begin
    { a delay is in Secs, outavgdelay is Millisec }
  FOutAvgDelay := ((FOutAvgDelay * FIncludedOutDelayCnt) + (ADelay * 1000)) div
    (FIncludedOutDelayCnt + 1);
  inc(FIncludedOutDelayCnt);
end;

function TAttackStatsList.IndexOfMob(AMobName: string): TAttackStats;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].MOB, AMobName) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

{ TDropItemList }

procedure TDropItemList.Clear;
begin
  FLastDrop := nil;
  
  inherited Clear;
end;

function TDropItemList.FindOrAddItem(const AItemName,
  ADropMob: string): TDropItem;
var
  iIdx:   integer;
begin
  iIdx := IndexOfItem(AItemName, ADropMob);
  if iIdx = -1 then begin
    Result := TDropItem.Create;
    Add(Result);
    Result.FDropMob := ADropMob;
    Result.SetName(AItemName);
  end
  else
    Result := Items[iIdx];

  FLastDrop := Result;
end;

function TDropItemList.GetItems(iIndex: integer): TDropItem;
begin
  Result := TDropItem(inherited Items[iIndex]);
end;

function TDropItemList.IndexOfItem(const AItemName,
  ADropMob: string): integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(Items[Result].Name, AItemName) and (
       (ADropMob = '*') or AnsiSameText(Items[Result].DropMOB, ADropMob)
      ) then
      exit;

  Result := -1;
end;

{ TDropItem }

procedure TDropItem.LoadSellValue;
begin
  with TINIFile.Create('.\Items.ini') do begin
    FSellValue := ReadInteger('SellValue', FName, FSellValue);
    Free;
  end;
end;

procedure TDropItem.SaveSellValue;
begin
  with TINIFile.Create('.\Items.ini') do begin
    WriteInteger('SellValue', FName, FSellValue);
    Free;
  end;
end;

procedure TDropItem.SetName(const AName: string);
  function ItemIsMagic(const sItemName: string) : boolean;
  var
    iPos:   integer;
    sWord:  string;
  begin
    iPos := 1;
    while iPos < Length(sItemName) do begin
      sWord := ParseWord(sItemName, iPos);
      if (sWord <> '') and not (sWord[1] in ['A'..'Z']) then begin
        Result := false;
        exit;
      end;
    end;

    Result := true;
  end;

begin
  FName := AName;
  FMagical := ItemIsMagic(AName);
  LoadSellValue;
end;

procedure TDropItem.SetSellValue(AValue: integer);
begin
  if FSellValue = AValue then
    exit;

  FSellValue := AValue;
  SaveSellValue;
end;

{ TLocationList }

function TLocationList.GetItems(iIndex: integer): TLocation;
begin
  Result := TLocation(inherited Items[iIndex]);
end;

end.

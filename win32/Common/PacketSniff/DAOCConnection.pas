unit DAOCConnection;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
** Portions based on code from Excalibur / Odin's Eye
**   http://excalibar.sourceforge.net
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows, WinSock, Dialogs, ExtCtrls,
{$ENDIF !LINUX}
  SysUtils, Classes, Contnrs,
{$IFDEF DAOC_AUTO_SERVER}
  ComObj,
{$ENDIF}
  GameNetPackets, NamedPacketHandler, DAOCAccountCharInfo, DAOCObjs, DAOCRegion,
  DAOCInventory, DAOCPlayerAttributes, StringParseHlprs, VendorItems,
  ChatParse, FrameFns, MapNavigator, INIFiles;

type
  TStringEvent = procedure (Sender: TObject; const AMsg: string) of Object;
  TIntegerEvent = procedure (Sender: TObject; AVal: integer) of Object;
  TSheduledCallback = procedure (Sender: TObject; AParm: Cardinal) of Object;
  TChatMessageEvent = procedure (Sender: TObject; const AWho, AMsg: string) of Object;
  TVersionEvent = procedure (Sender: TObject; AMajor, AMinor, ARelease: BYTE) of Object;
  TCurrencyChangeEvent = procedure (Sender: TObject; AReason: TDAOCCurrencyChangeReason;
    ADeltaAsCopper: integer) of object;

  PCallbackEventInfo = ^TCallbackEventInfo;
  TCallbackEventInfo = record
    dwTime:   Cardinal;
    fn:       TSheduledCallback;
    parm:     Cardinal;
  end;

{$IFDEF DAOC_AUTO_SERVER} TDAOCConnection = class(TAutoObject) {$ELSE} TDAOCConnection = class(TObject) {$ENDIF}
  private
    FActive:    boolean;
    FVersionMajor: byte;
    FVersionMinor: byte;
    FVersionRelease: byte;
    FAccountCharacters:  TDAOCAccountCharInfoList;
    FServerProtocol:  byte;

    FSelectedID:    WORD;
    FTradeCommissionItem: string;
    FTradeCommissionNPC: string;
    FScheduledCallbacks:  TList;
    FMaxObjectDistSqr:    double;
    FMaxObjectDist:       double;
    FPingRequestSentTime: Cardinal;
    FLastPingTime:        integer;
    FRealmRanks:          TStringList;
    FSelectedObjectCached:  TDAOCObject;
    FPacketHandlerDefFile: string;
    FLastUDPSeq:    WORD;

    FOnPlayerPosUpdate: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FOnLog:  TStringEvent;
    FOnVendorWindow: TNotifyEvent;
    FOnNewDAOCObject: TDAOCObjectNotify;
    FOnZoneChange: TNotifyEvent;
    FOnCharacterLogin: TNotifyEvent;
    FOnPacket: TGNPacketEvent;
    FOnInventoryChanged: TNotifyEvent;
    FOnSkillLevelChanged: TNameValueModifiedNotify;
    FOnAfterPacket: TGNPacketEvent;
    FOnCurrencyChanged: TCurrencyChangeEvent;
    FOnDeleteDAOCObject: TDAOCObjectNotify;
    FOnSelectedObjectChange: TDAOCObjectNotify;
    FOnTradeSkillSuccess: TIntegerEvent;
    FOnTradeSkillFailure: TNotifyEvent;
    FOnTradeSkillFailureWithLoss: TNotifyEvent;
    FOnTradeSkillCapped: TNotifyEvent;
    FOnSetGroundTarget: TNotifyEvent;
    FOnChatSayIncoming: TChatMessageEvent;
    FOnChatSendIncoming: TChatMessageEvent;
    FOnChatBroadcast: TStringEvent;
    FOnChatSayOutgoing: TStringEvent;
    FOnChatSendOutgoing: TStringEvent;
    FOnChatLog: TStringEvent;
    FOnDAOCObjectMoved: TDAOCObjectNotify;
    FOnCombatStyleFailure: TNotifyEvent;
    FOnCombatStyleSuccess: TStringEvent;
    FOnVersionNumsSet: TVersionEvent;
    FOnRegionChanged: TNotifyEvent;
    FOnPingReply: TIntegerEvent;
    FOnMobTargetChanged: TDAOCMobNotify;
    FOnUnknownStealther: TDAOCObjectNotify;
    FOnDelveItem: TDAOCInventoryItemNotify;
    FOnBountyPointsChanged: TIntegerEvent;
    FOnRealmPointsChanged: TIntegerEvent;
    FOnLocalHealthUpdate: TNotifyEvent;
    FOnGroupMembersChanged: TNotifyEvent;
    FOnDoorPositionUpdate: TDAOCObjectNotify;

    function GetClientIP: string;
    function GetServerIP: string;
    procedure SetClientIP(const Value: string);
    procedure SetServerIP(const Value: string);
    procedure ClearCallbackList;
    procedure CheckScheduledTimeoutCallback;
    function GetSelectedObject: TDAOCObject;
    procedure SetSelectedObject(const Value: TDAOCObject);
    procedure ClearDAOCObjectList;
    procedure CheckObjectsOutOfRange;
    procedure SetMaxObjectDistance(const Value: double);
    function CheckZoneChanged : boolean;
    function SetActiveCharacterByName(const ACharacterName: string) : TDAOCAccountCharInfo;
    function CheckAndMoveFromStaleListByInfoID(wID: WORD) : TDAOCObject;
    function CheckAndMoveFromStaleListByPlayerID(wID: WORD): TDAOCObject;
    procedure SafeAddDAOCObjectAndNotify(ADAOCObject: TDAOCObject);
    procedure UpdateRealmRank(AObj: TDAOCPlayer);
    procedure SetAggressorTarget(AAggressor: TDAOCObject; wTargetID: WORD);
    procedure OBJWALKResetGroup(AObj: TDAOCObject; AParam: Integer; var AContinue: boolean);
    procedure OBJWALKResetGuild(AObj: TDAOCObject; AParam: Integer; var AContinue: boolean);
    procedure OBJWALKRemoveTarget(AObj: TDAOCObject; AParam: Integer; var AContinue: boolean);
    procedure LinkPacketHandlers(AHandlerList: TNamedPacketHandlerList);
  protected
    FSource:        TObject;  // usually a dstream server
    FConnectionID:  Cardinal;
    FClientAddr:    Cardinal;
    FServerAddr:    Cardinal;
    FChatParser:    TDAOCChatParser;
    FLocalPlayer:   TDAOCLocalPlayer;
    FZoneList:  TDAOCZoneInfoList;
    FZone:      TDAOCZoneInfo;
    FRegionID:  integer;
    FDAOCObjs:  TDAOCObjectLinkedList;
    FDAOCObjsStale: TDAOCObjectLinkedList;
    FUnknownStealthers: TDAOCObjectLinkedList;
    FVendorItems: TDAOCVendorItemList;
    FMasterVendorList: TDAOCMasterVendorList;
    FGroundTarget: TMapNode;
    FMaxObjectStaleTime: Cardinal;
    FLastDelveRequestPos: BYTE;
    FLastCurrencyChangeReason: TDAOCCurrencyChangeReason;
    FGroupMembers:  TDAOCObjectList;
    FServerPacketHandlers: TNamedPacketHandlerList;
    FClientPacketHandlers: TNamedPacketHandlerList;

    procedure CPARSETradeSkillSuccess(ASender: TDAOCChatParser; AQuality: integer);
    procedure CPARSETradeSkillFailure(ASender: TDAOCChatParser);
    procedure CPARSETradeSkillFailureWithLoss(ASender: TDAOCChatParser);
    procedure CPARSETradeSkillCapped(ASender: TDAOCChatParser);
    procedure CPARSECombatStyleSuccess(ASender: TDAOCChatParser);
    procedure CPARSECombatStyleFailure(ASender: TDAOCChatParser);
    procedure CPARSETargetChanged(ASender: TDAOCChatParser);
    procedure CPARSERealmPointsChange(ASender: TDAoCChatParser; APoints: Integer);
    procedure CPARSEBountyPointsChange(ASender: TDAoCChatParser; APoints: Integer);
    procedure CPARSECurrencyChanged(ASender: TDAOCChatParser; AReason: TDAOCCurrencyChangeReason);

    procedure ParseSetEncryptionKey(pPacket: TGameNetPacket);
    procedure ParseSetPlayerRegion(pPacket: TGameNetPacket);
    procedure ParseAccountCharacters(pPacket: TGameNetPacket);
    procedure ParsePlayerDetails(pPacket: TGameNetPacket);
    procedure ParseLocalPosUpdateFromClient(pPacket: TGameNetPacket);
    procedure ParseInventoryList(pPacket: TGameNetPacket);
    procedure ParsePlayerStatsUpdate(pPacket: TGameNetPacket);
    procedure ParsePlayerSpecsSpellsAbils(pPacket: TGameNetPacket);
    procedure ParsePlayerSkills(pPacket: TGameNetPacket);
    procedure ParseLocalHeadUpdateFromClient(pPacket: TGameNetPacket);
    procedure ParsePlayerPosUpdate(pPacket: TGameNetPacket);
    procedure ParsePlayerHeadUpdate(pPacket: TGameNetPacket);
    procedure ParseMobUpdate(pPacket: TGameNetPacket);
    procedure ParseLogUpdate(pPacket: TGameNetPacket);
    procedure ParseLocalHealthUpdate(pPacket: TGameNetPacket);
    procedure ParseCharacterLoginInit(pPacket: TGameNetPacket);
    procedure ParseNewObjectCommon(pPacket: TGameNetPacket; AClass: TDAOCObjectClass);
    procedure ParseNewObject(pPacket: TGameNetPacket);
    procedure ParseNewMob(pPacket: TGameNetPacket);
    procedure ParseNewPlayer(pPacket: TGameNetPacket);
    procedure ParseNewVehicle(pPacket: TGameNetPacket);
    procedure ParseObjectEquipment(pPacket: TGameNetPacket);
    procedure ParseMoneyUpdate(pPacket: TGameNetPacket);
    procedure ParseRequestBuyItem(pPacket: TGameNetPacket);
    procedure ParseSelectedIDUpdate(pPacket: TGameNetPacket);
    procedure ParseProgressMeter(pPacket: TGameNetPacket);
    procedure ParseSpellPulse(pPacket: TGameNetPacket);
    procedure ParsePopupMessage(pPacket: TGameNetPacket);
    procedure ParseCommandFromClient(pPacket: TGameNetPacket);
    procedure ParseVendorWindow(pPacket: TGameNetPacket);
    procedure ParseRegionServerInfomation(pPacket: TGameNetPacket);
    procedure ParseDeleteObject(pPacket: TGameNetPacket);
    procedure ParseSetGroundTarget(pPacket: TGameNetPacket);
    procedure ParseCharacterStealthed(pPacket: TGameNetPacket);
    procedure ParseCharacterActivationRequest(pPacket: TGameNetPacket);
    procedure ParseServerProtocolInit(pPacket: TGameNetPacket);
    procedure ParseRequestPlayerByPlayerID(pPacket: TGameNetPacket);
    procedure ParseRequestObjectByInfoID(pPacket: TGameNetPacket);
    procedure ParsePlayerCenteredSpellEffect(pPacket: TGameNetPacket);
    procedure ParseServerPingResponse(pPacket: TGameNetPacket);
    procedure ParseServerPingRequest(pPacket: TGameNetPacket);
    procedure ParseGroupMembersUpdate(pPacket: TGameNetPacket);
    procedure ParseGroupWindowUpdate(pPacket: TGameNetPacket);
    procedure ParseAggroIndicator(pPacket: TGameNetPacket);
    procedure ParseAccountLoginRequest(pPacket: TGameNetPacket);
    procedure ParseDelveRequest(pPacket: TGameNetPacket);
    procedure ParseDelveInformation(pPacket: TGameNetPacket);
    procedure ParseVendorWindowRequest(pPacket: TGameNetPacket);
    procedure ParseDoorPositionUpdate(pPacket: TGameNetPacket);

    procedure ProcessDAOCPacketFromServer(pPacket: TGameNetPacket);
    procedure ProcessDAOCPacketFromClient(pPacket: TGameNetPacket);

    procedure DoOnChatSayIncoming(const AWho, AMessage: string); virtual;
    procedure DoOnChatSayOutgoing(const AMessage: string); virtual;
    procedure DoOnChatSendIncoming(const AWho, AMessage: string); virtual;
    procedure DoOnChatSendOutgoing(const AMessage: string); virtual;
    procedure DoOnChatBroadcast(const AMessage: string); virtual;
    procedure DoOnSetGroundTarget; virtual;
    procedure DoOnTradeSkillSuccess(AQuality: integer); virtual;
    procedure DoOnTradeSkillFailure; virtual;
    procedure DoOnTradeSkillFailureWithLoss; virtual;
    procedure DoOnTradeSkillCapped; virtual;
    procedure DoOnSelectedObjectChanged(AObject: TDAOCObject); virtual;
    procedure DoOnNewDAOCObject(AObject: TDAOCObject); virtual;
    procedure DoOnDeleteDAOCObject(AObject: TDAOCObject); virtual;
    procedure DoOnTradeskillTaskCompleted; virtual;
    procedure DoOnTradeCommissionAssigned; virtual;
    procedure DoOnChatLogLine(const ALine: string); virtual;
    procedure DoOnPopupMessage(const AMessage: string); virtual;
    procedure DoOnProgressMeterClose; virtual;
    procedure DoOnProgressMeterOpen(const AMessage: string); virtual;
    procedure DoOnCurrencyChanged(ADeltaAsCopper: integer); virtual;
    procedure DoOnSkillLevelChanged(AItem: TDAOCNameValuePair); virtual;
    procedure DoOnPlayerPosUpdate; virtual;
    procedure DoOnInventoryChanged; virtual;
    procedure DoOnZoneChange; virtual;
    procedure DoOnCharacterLogin; virtual;
    procedure DoOnConnect; virtual;
    procedure DoOnDisconnect; virtual;
    procedure Log(const s: string); virtual;
    procedure DoSetRegionID(ARegion: integer); virtual;
    procedure DoOnDAOCObjectMoved(AObject: TDAOCObject); virtual;
    procedure DoOnCombatStyleSuccess(AStyle: string); virtual;
    procedure DoOnCombatStyleFailure; virtual;
    procedure DoVersionNumsSet; virtual;
    procedure DoOnPingReply; virtual;
    procedure DoOnMobTargetChanged(AMob: TDAOCMob); virtual;
    procedure DoOnUnknownStealther(AUnk: TDAOCUnknownStealther); virtual;
    procedure DoOnDelveItem(AItem: TDAOCInventoryItem); virtual;
    procedure DoOnVendorWindowRequest(AMob: TDAOCMob); virtual;
    procedure DoOnLocalHealthUpdate; virtual;
    procedure DoMobInventoryUpdate(AMob: TDAOCMovingObject; AItem: TDAOCInventoryItem); virtual;
    procedure DoOnGroupMembersChanged; virtual;
    procedure DoOnDoorPositionUpdate(AObj: TDAOCObject); virtual; 

    procedure ChatSay(const ALine: string);
    procedure ChatSend(const ALine: string);
    procedure HookChatParseCallbacks;
    procedure MergeVendorItemsToMaster;
    procedure UpdatePlayersInGuild;
    procedure ResetPlayersInGroup;
    procedure AdjustObjLocForZone(AObj: TDAOCObject; AZoneNum: integer);
    procedure AdjustObjDestForZone(AObj: TDAOCMovingObject; AZoneNum: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure InitPacketHandlers;
    procedure ProcessDAOCPacket(pPacket: TGameNetPacket);
    procedure Clear; virtual;
    procedure CheckForStaleObjects;
    procedure LoadRealmRanks(const AFileName: string);
    procedure ScheduleCallback(ATimeout: Cardinal; ACallback: TSheduledCallback; AParm: Cardinal);

      { Functions to use zone information to find relative player coords }
    function PlayerZoneHead : integer;
    function PlayerZoneX : Cardinal;
    function PlayerZoneY : Cardinal;
    function PlayerZoneZ : Cardinal;

      { network properties }
    property Active: boolean read FActive;
    property ClientAddr: Cardinal read FClientAddr write FClientAddr;
    property ClientIP: string read GetClientIP write SetClientIP;
    property ServerAddr: Cardinal read FServerAddr write FServerAddr;
    property ServerIP: string read GetServerIP write SetServerIP;
    property Source: TObject read FSource write FSource;

      { properties }
    property AccountCharacterList: TDAOCAccountCharInfoList read FAccountCharacters;
    property ConnectionID: Cardinal read FConnectionID write FConnectionID;
    property DAOCObjects: TDAOCObjectLinkedList read FDAOCObjs;
    property UnknownStealthers: TDAOCObjectLinkedList read FUnknownStealthers;
    property GroundTarget: TMapNode read FGroundTarget;
    property GroupMembers: TDAOCObjectList read FGroupMembers;
    property MaxObjectDistance: double read FMaxObjectDist write SetMaxObjectDistance;
    property MaxObjectStaleTime: Cardinal read FMaxObjectStaleTime write FMaxObjectStaleTime;
    property MasterVendorList: TDAOCMasterVendorList read FMasterVendorList;
    property LastPingTime: integer read FLastPingTime;
    property LocalPlayer: TDAOCLocalPlayer read FLocalPlayer;
    property PacketHandlerDefFile: string read FPacketHandlerDefFile write FPacketHandlerDefFile;
    property RegionID: integer read FRegionID write FRegionID;
    property SelectedID: WORD read FSelectedID;
    property ServerProtocol: BYTE read FServerProtocol write FServerProtocol;
    property SelectedObject: TDAOCObject read GetSelectedObject write SetSelectedObject;
    property TradeCommissionNPC: string read FTradeCommissionNPC;
    property TradeCommissionItem: string read FTradeCommissionItem;
    property VendorItems: TDAOCVendorItemList read FVendorItems;
    property Zone: TDAOCZoneInfo read FZone;
    property ZoneList: TDAOCZoneInfoList read FZoneList;

      { events }
    property OnAfterPacket: TGNPacketEvent read FOnAfterPacket write FOnAfterPacket;
    property OnBountyPointsChanged: TIntegerEvent read FOnBountyPointsChanged write FOnBountyPointsChanged;
    property OnCharacterLogin: TNotifyEvent read FOnCharacterLogin write FOnCharacterLogin;
    property OnChatLog: TStringEvent read FOnChatLog write FOnChatLog;
    property OnChatSendIncoming: TChatMessageEvent read FOnChatSendIncoming write FOnChatSendIncoming;
    property OnChatSendOutgoing: TStringEvent read FOnChatSendOutgoing write FOnChatSendOutgoing;
    property OnChatSayIncoming: TChatMessageEvent read FOnChatSayIncoming write FOnChatSayIncoming;
    property OnChatSayOutgoing: TStringEvent read FOnChatSayOutgoing write FOnChatSayOutgoing;
    property OnChatBroadcast: TStringEvent read FOnChatBroadcast write FOnChatBroadcast;
    property OnCombatStyleSuccess: TStringEvent read FOnCombatStyleSuccess write FOnCombatStyleSuccess;
    property OnCombatStyleFailure: TNotifyEvent read FOnCombatStyleFailure write FOnCombatStyleFailure;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnCurrencyChanged: TCurrencyChangeEvent read FOnCurrencyChanged write FOnCurrencyChanged;
    property OnDAOCObjectMoved: TDAOCObjectNotify read FOnDAOCObjectMoved write FOnDAOCObjectMoved;
    property OnDeleteDAOCObject: TDAOCObjectNotify read FOnDeleteDAOCObject write FOnDeleteDAOCObject;
    property OnDelveItem: TDAOCInventoryItemNotify read FOnDelveItem write FOnDelveItem;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnDoorPositionUpdate: TDAOCObjectNotify read FOnDoorPositionUpdate write FOnDoorPositionUpdate;
    property OnGroupMembersChanged: TNotifyEvent read FOnGroupMembersChanged write FOnGroupMembersChanged;
    property OnInventoryChanged: TNotifyEvent read FOnInventoryChanged write FOnInventoryChanged;
    property OnLocalHealthUpdate: TNotifyEvent read FOnLocalHealthUpdate write FOnLocalHealthUpdate;
    property OnLog: TStringEvent read FOnLog write FOnLog;
    property OnMobTargetChanged: TDAOCMobNotify read FOnMobTargetChanged write FOnMobTargetChanged;
    property OnNewDAOCObject: TDAOCObjectNotify read FOnNewDAOCObject write FOnNewDAOCObject;
    property OnPacket: TGNPacketEvent read FOnPacket write FOnPacket;
    property OnPingReply: TIntegerEvent read FOnPingReply write FOnPingReply;
    property OnPlayerPosUpdate: TNotifyEvent read FOnPlayerPosUpdate write FOnPlayerPosUpdate;
    property OnRealmPointsChanged: TIntegerEvent read FOnRealmPointsChanged write FOnRealmPointsChanged;
    property OnRegionChanged: TNotifyEvent read FOnRegionChanged write FOnRegionChanged;
    property OnSelectedObjectChange: TDAOCObjectNotify read FOnSelectedObjectChange write FOnSelectedObjectChange;
    property OnSetGroundTarget: TNotifyEvent read FOnSetGroundTarget write FOnSetGroundTarget;
    property OnSkillLevelChanged: TNameValueModifiedNotify read FOnSkillLevelChanged write FOnSkillLevelChanged;
    property OnTradeSkillSuccess: TIntegerEvent read FOnTradeSkillSuccess write FOnTradeSkillSuccess;
    property OnTradeSkillFailure: TNotifyEvent read FOnTradeSkillFailure write FOnTradeSkillFailure;
    property OnTradeSkillFailureWithLoss: TNotifyEvent read FOnTradeSkillFailureWithLoss write FOnTradeSkillFailureWithLoss;
    property OnTradeSkillCapped: TNotifyEvent read FOnTradeSkillCapped write FOnTradeSkillCapped;
    property OnUnknownStealther: TDAOCObjectNotify read FOnUnknownStealther write FOnUnknownStealther;
    property OnVendorWindow: TNotifyEvent read FOnVendorWindow write FOnVendorWindow;
    property OnVersionNumsSet: TVersionEvent read FOnVersionNumsSet write FOnVersionNumsSet;
    property OnZoneChange: TNotifyEvent read FOnZoneChange write FOnZoneChange;
  end;

implementation

uses
  GlobalTickCounter;

{ TDAOCConnection }

procedure TDAOCConnection.Clear;
begin
  FActive := false;
  FMasterVendorList.Clear;
  FAccountCharacters.Clear;
  ResetPlayersInGroup;
  ClearDAOCObjectList;
  FVendorItems.Clear;
  FLocalPlayer.Clear;
  FTradeCommissionNPC := '';
  FTradeCommissionItem := '';
  ClearCallbackList;
  FRegionID := 0;
  FSelectedID := 0;
  FChatParser.Reset;
  FGroundTarget.Clear;
  FConnectionID := 0;
end;

constructor TDAOCConnection.Create;
begin
  inherited Create;

  FLocalPlayer := TDAOCLocalPlayer.Create;
  FGroundTarget := TMapNode.Create;
  FZoneList := TDAOCZoneInfoList.Create;
  // FZoneList.LoadFromFile('mapinfo.txt');

  FPacketHandlerDefFile := 'packethandlers.ini';
  FServerPacketHandlers := TNamedPacketHandlerList.Create;
  FClientPacketHandlers := TNamedPacketHandlerList.Create;

  FAccountCharacters := TDAOCAccountCharInfoList.Create;
  FScheduledCallbacks := TList.Create;
  FVendorItems := TDAOCVendorItemList.Create;
  FMasterVendorList := TDAOCMasterVendorList.Create;
  FDAOCObjs := TDAOCObjectLinkedList.Create;
  FDAOCObjsStale := TDAOCObjectLinkedList.Create;
  FUnknownStealthers := TDAOCObjectLinkedList.Create;
  FChatParser := TDAOCChatParser.Create;
  FGroupMembers := TDAOCObjectList.Create(false);
  HookChatParseCallbacks;

  FServerProtocol := $01;
  SetMaxObjectDistance(8500);
  FMaxObjectStaleTime := 240000;  // 240,000ms = 4min

  LoadRealmRanks(ExtractFilePath(ParamStr(0)) + 'RealmRanks.dat');
end;

destructor TDAOCConnection.Destroy;
begin
  ClearCallbackList;
  FScheduledCallbacks.Free;
  FAccountCharacters.Free;

  FChatParser.Free;
  FDAOCObjs.Free;
  FDAOCObjsStale.Free;
  FUnknownStealthers.Free;
  FMasterVendorList.Free;
  FVendorItems.Free;
  FZoneList.Free;
  FGroundTarget.Free;
  FLocalPlayer.Free;
  FRealmRanks.Free;
  FGroupMembers.Free;

  FServerPacketHandlers.Free;
  FClientPacketHandlers.Free;

  inherited Destroy;
end;

procedure TDAOCConnection.DoOnCharacterLogin;
begin
  if Assigned(FOnCharacterLogin) then
    FOnCharacterLogin(Self);
end;

procedure TDAOCConnection.DoOnConnect;
begin
  FActive := true;
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TDAOCConnection.DoOnDisconnect;
begin
  FActive := false;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TDAOCConnection.DoOnInventoryChanged;
begin
  if Assigned(FOnInventoryChanged) then
    FOnInventoryChanged(Self);
end;

procedure TDAOCConnection.DoOnPlayerPosUpdate;
begin
  if Assigned(FOnPlayerPosUpdate) then
    FOnPlayerPosUpdate(Self);
end;

procedure TDAOCConnection.DoOnZoneChange;
begin
  if Assigned(FOnZoneChange) then
    FOnZoneChange(Self);
end;

function TDAOCConnection.GetClientIP: string;
begin
  Result := my_inet_ntoa(FClientAddr);
end;

function TDAOCConnection.GetServerIP: string;
begin
  Result := my_inet_ntoa(FServerAddr);
end;

procedure TDAOCConnection.Log(const s: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, s);
end;

procedure TDAOCConnection.ParseAccountCharacters(pPacket: TGameNetPacket);
var
  sName:  string;
  iCharsLeft: integer;
  iRegion:  integer;
  iRealm:   integer;
  iLevel:   integer;
  pAcctChar: TDAOCAccountCharInfo;
begin
  pPacket.HandlerName := 'AccountCharacters';
  FAccountCharacters.AccountName := pPacket.getNullTermString(24);

    { parse up to 8 characters }
  iCharsLeft := 8;

  while (iCharsLeft > 0) and not pPacket.EOF do begin
    sName := pPacket.getNullTermString(48);
    pPacket.Seek(72);
    iLevel := pPacket.getByte;
    pPacket.Seek(1);
    iRealm := pPacket.getByte;
    pPacket.Seek(3);
    iRegion := pPacket.getByte;
    pPacket.Seek(57);
    if (iRegion <> 0) and (sName <> '') then begin
      pAcctChar := FAccountCharacters.FindOrAddChar(sName);
      pAcctChar.RegionID := iRegion;
      pAcctChar.Realm := TDAOCRealm(iRealm);
      pAcctChar.Level := iLevel;
    end;
    dec(iCharsLeft);
  end;    { for chars }
end;

procedure TDAOCConnection.ParsePlayerDetails(pPacket: TGameNetPacket);
var
  SubType: BYTE;
  iLevel:   integer;
  sName:    string;
  pAcctChar:  TDAOCAccountCharInfo;
begin
  pPacket.HandlerName := 'PlayerDetails';
  pPacket.Seek(1);  // count of items
  SubType := pPacket.getByte;
  if SubType = 0 then begin
    pPacket.Seek(1);
    iLevel := pPacket.getByte;
    sName := pPacket.getPascalString;

    pAcctChar := SetActiveCharacterByName(sName);
    pAcctChar.Level := iLevel;

    with FLocalPlayer do begin
      Level := iLevel;
      pPacket.Seek(1);
      PlayerClassStr := pPacket.getPascalString;
      pPacket.Seek(1);
      House := pPacket.getPascalString;
      pPacket.Seek(1);
      HouseTitle := pPacket.getPascalString;
      pPacket.Seek(1);
      RealmTitle := pPacket.getPascalString;
      pPacket.Seek(1);
      BaseClass := pPacket.getPascalString;
      pPacket.Seek(1);
      Guild := pPacket.getPascalString;
      pPacket.Seek(1);
      LastName := pPacket.getPascalString;
      pPacket.Seek(1);
      Race := pPacket.getPascalString;
      // pPacket.Seek(1);
      // pPacket.getPascalString;  // "Recruits"?
      // pPacket.Seek(1);
      // pPacket.getPascalString;  // "None"?
      // pPacket.Seek(1);
      // pPacket.getPascalString;  // "None"?
    end;

    UpdatePlayersInGuild;
    DoOnCharacterLogin;
  end;
end;

procedure TDAOCConnection.ParseInventoryList(pPacket: TGameNetPacket);
var
  iItemCount:   integer;
  pTmpItem: TDAOCInventoryItem;
begin
  pPacket.HandlerName := 'InventoryList';
  iItemCount := pPacket.getByte;
  pPacket.seek(3);
  while (iItemCount > 0) and not pPacket.EOF do begin
    pTmpItem := TDAOCInventoryItem.Create;
    pTmpItem.Slot := pPacket.getByte;
    pTmpItem.Level := pPacket.getByte;
    if pTmpItem.Level = 0 then
      pTmpItem.Count := pPacket.getByte
    else
      pPacket.seek(1);
    pPacket.seek(5);
    pTmpItem.Condition := pPacket.getByte;
    pTmpItem.Durability := pPacket.getByte;
    pTmpItem.Quality := pPacket.getByte;
    pTmpItem.Bonus := pPacket.getByte;
    pPacket.seek(1);
    pTmpItem.ItemID := pPacket.getShort;
    pTmpItem.Color := pPacket.getByte;
    pPacket.seek(2);
    pTmpItem.Description := pPacket.getPascalString;

    FLocalPlayer.Inventory.TakeItem(pTmpItem);
    dec(iItemCount);
  end;

  DoOnInventoryChanged;
end;

function TDAOCConnection.CheckZoneChanged : boolean;
begin
  Result := false;
  
  if Assigned(FZone) then
    if not FZone.ContainsPoint(FRegionID, FLocalPlayer.X, FLocalPlayer.Y) then begin
      FZone := nil;
      Result := true;
    end;

  if not Assigned(FZone) then begin
    FZone := FZoneList.FindZoneForPoint(FRegionID, FLocalPlayer.X, FLocalPlayer.Y);
    if Assigned(FZone) then
      Result := true;
  end;

  if Result then
    DoOnZoneChange;
end;

procedure TDAOCConnection.ParseLocalPosUpdateFromClient(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'LocalPosUpdateFromClient';
{$IFDEF PRE_168}
  pPacket.seek(2);
  FLocalPlayer.SpeedWord := pPacket.getShort;
  FLocalPlayer.Z := pPacket.getShort;
  pPacket.seek(2);
  FLocalPlayer.X := pPacket.getLong;
  FLocalPlayer.Y := pPacket.getLong;
  FLocalPlayer.HeadWord := pPacket.getShort;
  CheckZoneChanged;
{$ELSE}
  pPacket.seek(2);
  FLocalPlayer.SpeedWord := pPacket.getShort;
  FLocalPlayer.Z := pPacket.getShort;
  FLocalPlayer.X := pPacket.getShort;
  FLocalPlayer.Y := pPacket.getShort;
  AdjustObjLocForZone(FLocalPlayer, pPacket.getByte);
  pPacket.seek(1);
  FLocalPlayer.HeadWord := pPacket.getShort;
{$ENDIF}

  DoOnPlayerPosUpdate;
  CheckObjectsOutOfRange;
end;

procedure TDAOCConnection.ParsePlayerStatsUpdate(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'PlayerStatsUpdate';
  case pPacket.getByte of
    $01:  ParsePlayerSpecsSpellsAbils(pPacket);
    $03:  ParsePlayerDetails(pPacket);
    // $05: Might be stats changed due to buff?
    $06:  ParseGroupMembersUpdate(pPacket);
    $08:  ParsePlayerSkills(pPacket);
  end;
end;

procedure TDAOCConnection.ParseSetEncryptionKey(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'SetEncryptionKey';

    { version w x.yz }
//  pPacket.seek(1);
//  FServerProtocol := pPacket.getByte; // actually encryption scheme?
//  FVersionMajor := pPacket.getByte;  // bigver  (x)
//  FVersionMinor := pPacket.getByte;  // minver  (y)
//  FVersionRelease := pPacket.getByte;// release (z)

//  DoVersionNumsSet;
end;

procedure TDAOCConnection.ParseSetPlayerRegion(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'SetPlayerRegion';
  DoSetRegionID(pPacket.getShort);
end;

function TDAOCConnection.PlayerZoneHead: integer;
begin
  Result := FLocalPlayer.Head;
  if Assigned(FZone) then 
    Result := FZone.ZoneConvertHead(Result);
end;

function TDAOCConnection.PlayerZoneX: Cardinal;
begin
  Result := FLocalPlayer.X;
  if Assigned(FZone) then
    Result := FZone.WorldToZoneX(Result);
end;

function TDAOCConnection.PlayerZoneY: Cardinal;
begin
  Result := FLocalPlayer.Y;
  if Assigned(FZone) then
    Result := FZone.WorldToZoneY(Result);
end;

function TDAOCConnection.PlayerZoneZ: Cardinal;
begin
  Result := FLocalPlayer.Z;
end;

procedure TDAOCConnection.ProcessDAOCPacketFromClient(
  pPacket: TGameNetPacket);
var
  command:  BYTE;
  pHandler: TNamedPacketHandler;
begin
  // seq := pPacket.getShort;
  // srcid := pPacket.getShort;
  // pPacket.seek(2);

  pPacket.seek(7);
  command := pPacket.getByte;

//  Writeln(Format('seq 0x%4.4x  src 0x%4.4x  cmd 0x%4.4x  dst 0x%4.4x',
//    [seq, srcid, command, destid]));

  pHandler := FClientPacketHandlers.HandlerByID(command);
  if Assigned(pHandler) then
    pHandler.Handler(pPacket);
end;

procedure TDAOCConnection.ProcessDAOCPacketFromServer(pPacket: TGameNetPacket);
var
  command:  BYTE;
  iSeqNo:   integer;
  pHandler: TNamedPacketHandler;
begin
  iSeqNo := pPacket.getShort;

    { check for mislabeled UDP }
  if pPacket.IPProtocol = gnppTCP then
    if iSeqNo = (FLastUDPSeq + 1) then
      pPacket.IPProtocol := gnppUDP
    else
      pPacket.seek(-2);

  command := pPacket.getByte;
  pHandler := FServerPacketHandlers.HandlerByID(command);
  if Assigned(pHandler) then
    pHandler.Handler(pPacket);

  if pPacket.IPProtocol = gnppUDP then
    FLastUDPSeq := iSeqNo;
end;

procedure TDAOCConnection.DoSetRegionID(ARegion: integer);
var
  pAcctChar:  TDAOCAccountCharInfo;
begin
  if FRegionID = ARegion then
    exit;

  FGroundTarget.Clear;

  FRegionID := ARegion;

  if FLocalPlayer.Name <> '' then begin
    pAcctChar := FAccountCharacters.FindOrAddChar(FLocalPlayer.Name);
    pAcctChar.RegionID := FRegionID;
      { if we've got a blank acctchar, fill the values from the local player
        this is probably because we resumed connection }
    if pAcctChar.Level = 0 then
      pAcctChar.Level := FLocalPlayer.Level;
    if pAcctChar.Realm = drNeutral then
      pAcctChar.Realm := FLocalPlayer.Realm;
  end;

  FMasterVendorList.Clear;
  ClearDAOCObjectList;

  if Assigned(FOnRegionChanged) then
    FOnRegionChanged(Self);
  // Log('Player region changed to ' + IntToStr(FRegionID));
end;

procedure TDAOCConnection.ParsePlayerSpecsSpellsAbils(pPacket: TGameNetPacket);
var
  iCnt:   integer;
  iLevel: integer;
  bPage:  BYTE;
  sName:  string;
  pItem:  TDAOCNameValuePair;
begin
  pPacket.HandlerName := 'PlayerSpecsSpellsAbils';
  iCnt := pPacket.getByte;
  if pPacket.getByte <> $03 then
    exit;

  pPacket.seek(1);

  while (iCnt > 0) and not pPacket.EOF do begin
    iLevel := pPacket.getByte;
    bPage := pPacket.getByte;
    pPacket.seek(2);
    pPacket.seek(1); // iLevelAttained := pPacket.getByte;
    pPacket.seek(2); // wIcon := pPacket.getWord;
    sName := pPacket.getPascalString;

    case bPage of
      $00:  pItem := FLocalPlayer.Specializations.FindOrAdd(sName);
      $01:  pItem := FLocalPlayer.Abilities.FindOrAdd(sName);
      $02:  pItem := FLocalPlayer.Styles.FindOrAdd(sName);
      $03:  pItem := FLocalPlayer.Spells.FindOrAdd(sName);
      else
        pItem := nil;
    end;  { case bPage }

    if Assigned(pItem) then
      pItem.Value := iLevel;
      
    dec(iCnt);
  end;  { while cnt and !EOF }
end;

procedure TDAOCConnection.ParsePlayerSkills(pPacket: TGameNetPacket);
var
  iCnt:   integer;
  iLevel: integer;
  sName:  string;
  pItem:  TDAOCNameValuePair;
begin
  pPacket.HandlerName := 'PlayerSkills';
  iCnt := pPacket.getByte;
  if pPacket.getByte <> $03 then
    exit;

  pPacket.seek(1);

  while (iCnt > 0) and not pPacket.EOF do begin
    iLevel := pPacket.getShort;
    pPacket.seek(1); // iIcon := pPacket.getByte;
    pPacket.seek(4); 
    sName := pPacket.getPascalString;

    pItem := FLocalPlayer.Skills.FindOrAdd(sName);
    pItem.Value := iLevel;

    if pItem.Modified then
      DoOnSkillLevelChanged(pItem);

    dec(iCnt);
  end;  { while cnt and !EOF }
end;

procedure TDAOCConnection.DoOnSkillLevelChanged(AItem: TDAOCNameValuePair);
begin
  if Assigned(FOnSkillLevelChanged) then
    FOnSkillLevelChanged(Self, AItem);
end;

procedure TDAOCConnection.ParseLocalHeadUpdateFromClient(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'LocalHeadUpdateFromClient';
  pPacket.seek(2);
  FLocalPlayer.HeadWord := pPacket.getShort;
  DoOnPlayerPosUpdate;
end;

procedure TDAOCConnection.ParsePlayerPosUpdate(pPacket: TGameNetPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
begin
  pPacket.HandlerName := 'PlayerPosUpdate';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByPlayerID(wID);

    { check the stale list for a recent item, moving it over if necessary }
  if not Assigned(pDAOCObject) then
    pDAOCObject := CheckAndMoveFromStaleListByPlayerID(wID);

  if not Assigned(pDAOCObject) then
    exit;
(* This is removed because when the client gets this message it
   will request the NewObject message via a RequetPlayerByPlayerID.
   Since we don't know the infoid, we can't do anything until then anyway
   so just wait for the NewObject
    pDAOCObject := TDAOCPlayer.Create;
    pDAOCObject.InfoID := 0;
    pDAOCObject.PlayerID := wID;
    FDAOCObjs.AddOrReplace(pDAOCObject);
  end;
*)

  if Assigned(pDAOCObject) and (pDAOCObject is TDAOCMovingObject) then
    with TDAOCMovingObject(pDAOCObject) do begin
{$IFDEF PRE_168}
      SpeedWord := pPacket.getShort;  //+2
      Z := pPacket.getShort;  //+4
      pPacket.seek(2);  //+6
      X := pPacket.getLong;  //+8
      Y := pPacket.getLong;  //+c
      HeadWord := pPacket.getShort;  //+10
      pPacket.seek(2);  //+12 word and 0xfff
      pDAOCObject.Stealthed := (pPacket.getByte and $02) <> 0;  // stealthed but visible
      HitPoints := pPacket.getByte; //+15
{$ELSE}
      SpeedWord := pPacket.getShort;  //+2
      Z := pPacket.getShort;  //+4
      X := pPacket.getShort;
      Y := pPacket.getShort;
      AdjustObjLocForZone(pDAOCObject, pPacket.getByte);
      pPacket.seek(1);
      HeadWord := pPacket.getShort;
      pPacket.seek(2);
      pDAOCObject.Stealthed := (pPacket.getByte and $02) <> 0;  // stealthed but visible
      HitPoints := pPacket.getByte;
{$ENDIF}

      DoOnDAOCObjectMoved(pDAOCObject);
    end  { if obj found / With }
  else begin
    pDAOCObject := FDAOCObjs.FindByInfoID(wID);
    if Assigned(pDAOCObject) then
      Log('PlayerPosUpdate: MOB by PlayerID 0x' + IntToHex(wID, 4) + ' is in infoid list, not player list.  Type: ' + DAOCObjectClassToStr(pDAOCObject.ObjectClass))
    else
      Log('PlayerPosUpdate: Can not find MOB by PlayerID 0x' + IntToHex(wID, 4));
  end;
end;

procedure TDAOCConnection.ParseMobUpdate(pPacket: TGameNetPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
  iIDOffset:  integer;
  V162OrGreater:  boolean;
  bAddedObject:   boolean;
begin
  pPacket.HandlerName := 'MobUpdate';

  V162OrGreater := FServerProtocol = $01;
  if V162OrGreater then
    iIDOffset := 16
  else
    iIDOffset := 22;
  pPacket.seek(iIDOffset);
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByInfoID(wID);

    { check the stale list for a recent item, moving it over if necessary }
  if not Assigned(pDAOCObject) then
    pDAOCObject := CheckAndMoveFromStaleListByInfoID(wID);

    { here we can add the object, even though we don't know what it is.
      The client will request the object via RequestObjectByInfoID
      and we'll just replace this object with the real mob.  Make sure
      we fire the event for the add though! }
  if not Assigned(pDAOCObject) then begin
    pDAOCObject := TDAOCUnknownMovingObject.Create;
    pDAOCObject.InfoID := wID;
    pDAOCObject.PlayerID := 0;
    FDAOCObjs.Add(pDAOCObject);
    bAddedObject := true;
  end
  else
    bAddedObject := false;

  if Assigned(pDAOCObject) and (pDAOCObject is TDAOCMovingObject) then begin
    pPacket.seek(-(iIDOffset+2));

    with TDAOCMovingObject(pDAOCObject) do begin
      SpeedWord := pPacket.getShort;
      HeadWord := pPacket.getShort;
      if V162OrGreater then begin
        X := pPacket.getShort;
        DestinationX := pPacket.getShort;
        Y := pPacket.getShort;
        DestinationY := pPacket.getShort;
        Z := pPacket.getShort;
        DestinationZ := pPacket.getShort;
        pPacket.seek(2);  // ID again
        SetAggressorTarget(pDAOCObject, pPacket.getShort);
        HitPoints := pPacket.getByte;
        pPacket.seek(1);

        AdjustObjLocForZone(pDAOCObject, pPacket.getByte);
        AdjustObjDestForZone(TDAOCMovingObject(pDAOCObject), pPacket.getByte);
      end  { protocol }
      else begin
        X := pPacket.getLong;  //+0
        Y := pPacket.getLong;  //+4
        DestinationX := pPacket.getLong;  //+8
        DestinationY := pPacket.getLong;  //+c
        z := pPacket.getShort;  //+10
        pPacket.seek(2);  // ID again //+12
        SetAggressorTarget(pDAOCObject, pPacket.getShort);  // +14
        HitPoints := pPacket.getByte; //+15
      end;
    end;  { with TDAOCMovingObject(pDAOCObject) }

    if bAddedObject then
      DoOnNewDAOCObject(pDAOCObject)
    else
      DoOnDAOCObjectMoved(pDAOCObject);
  end  { if Assigned pDAOCObject }

  else if Assigned(pDAOCObject) then begin
    pPacket.seek(2);
    pDAOCObject.HitPoints := pPacket.getByte;
      { TODO: Should notify of hitpoint update }
//    if Assigned(pDAOCObject) then
//      Log('MobUpdate: MobByInfoID 0x' + IntToHex(wID, 4) + ' is not a moving object: ' + pDAOCObject.Name)
//    else begin
//      pDAOCObject := FDAOCObjs.FindByPlayerID(wID);
//      if Assigned(pDAOCObject) then
//        Log('MobUpdate: MOB by InfoID 0x' + IntToHex(wID, 4) + ' is in playerid list, not infoid list.  Type: ' + DAOCObjectClassToStr(pDAOCObject.ObjectClass));
//    end;
  end;
end;

procedure TDAOCConnection.ParsePlayerHeadUpdate(pPacket: TGameNetPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
begin
  pPacket.HandlerName := 'PlayerHeadUpdate';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByPlayerID(wID);

    { check the stale list for a recent item, moving it over if necessary }
  if not Assigned(pDAOCObject) then
    pDAOCObject := CheckAndMoveFromStaleListByPlayerID(wID);

  if not Assigned(pDAOCObject) then
    exit;
(* See PlayerPosUpdate for reason I don't do this
    pDAOCObject := TDAOCPlayer.Create;
    pDAOCObject.InfoID := 0;
    pDAOCObject.PlayerID := wID;
    FDAOCObjs.AddOrReplace(pDAOCObject);
  end;
*)

  if Assigned(pDAOCObject) and (pDAOCObject is TDAOCMovingObject) then
    with TDAOCMovingObject(pDAOCObject) do begin
      HeadWord := pPacket.getShort;
      pPacket.seek(1);
      pDAOCObject.Stealthed := (pPacket.getByte and $02) <> 0;  // stealthed but visible
      pPacket.seek(2);
      HitPoints := pPacket.getByte;

      DoOnDAOCObjectMoved(pDAOCObject);
    end  { if obj found / With }
  else begin
    pDAOCObject := FDAOCObjs.FindByInfoID(wID);
    if Assigned(pDAOCObject) then
      Log('PlayerHeadUpdate: MOB by PlayerID 0x' + IntToHex(wID, 4) + ' is in infoid list, not player list.  Type: ' + DAOCObjectClassToStr(pDAOCObject.ObjectClass))
    else
      Log('PlayerHeadUpdate: Can not find MOB by PlayerID 0x' + IntToHex(wID, 4));
  end;
end;

procedure TDAOCConnection.ParseLogUpdate(pPacket: TGameNetPacket);
var
  sLine:  string;
  bType:  BYTE;
begin
  pPacket.HandlerName := 'LogUpdate';
  pPacket.seek(4);
  bType := pPacket.getByte;
  pPacket.seek(3);
  sLine := pPacket.getNullTermString(0);

  if Assigned(FOnChatLog) then
    FOnChatLog(Self, sLine);

  case bType of
    $01:  ChatSay(sLine);
    $02:  ChatSend(sLine);
    $05:  DoOnChatBroadcast(sLine);
    else
      DoOnChatLogLine(sLine);
  end;  { case bType }
end;

procedure TDAOCConnection.ParseLocalHealthUpdate(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'LocalHealthUpdate';

  FLocalPlayer.HitPoints := pPacket.getByte;
  FLocalPlayer.ManaPct := pPacket.getByte;
  pPacket.seek(3);
  FLocalPlayer.EndurancePct := pPacket.getByte;

  DoOnLocalHealthUpdate;
end;

procedure TDAOCConnection.ParseCharacterLoginInit(pPacket: TGameNetPacket);
begin
    { the first packet we get which describes the character I think. }
  pPacket.HandlerName := 'CharacterLoginInit';
  FLocalPlayer.Clear;
  FLocalPlayer.InfoID := pPacket.getShort;
  pPacket.seek(2);
  FLocalPlayer.X := pPacket.getLong;
  FLocalPlayer.Y := pPacket.getLong;

  ClearDAOCObjectList;
  FVendorItems.Clear;

  CheckZoneChanged;
end;

procedure TDAOCConnection.ParseNewObjectCommon(pPacket: TGameNetPacket;
  AClass: TDAOCObjectClass);
var
  tmpObject:  TDAOCObject;
  pOldObject: TDAOCObject;
begin
  pPacket.HandlerName := 'NewObject (' + DAOCObjectClassToStr(AClass) + ')';

  case AClass of
    ocObject:
      begin
        tmpObject := TDAOCObject.Create;
        with TDAOCObject(tmpObject) do begin
          InfoID := pPacket.getShort;
          PlayerID := 0;
          pPacket.seek(2);
          HeadWord := pPacket.getShort;
          Z := pPacket.getShort;
          X := pPacket.getLong;
          Y := pPacket.getLong;
          pPacket.seek(4);
          Name := pPacket.getPascalString;
          if pPacket.getByte = 4 then
            DoorID := pPacket.getLong;
        end;  { with TDAOCObject }
      end;  { ocObject }

    ocMob:
      begin
        tmpObject := TDAOCMob.Create;
        with TDAOCMob(tmpObject) do begin
          InfoID := pPacket.getShort;  //+0
          PlayerID := 0;
          SpeedWord := pPacket.getShort;
          HeadWord := pPacket.getShort; //+4
          Z := pPacket.getShort;//+6
          X := pPacket.getLong;//+8
          Y := pPacket.getLong;//+c
          pPacket.seek(5);  //+10  sign-extended, stored as float
                            //+12  and ah 3
                            //+14  if !0 store 2x value instead of 0x64
          Level := pPacket.getByte;  //+15
          pPacket.getByte;  //+16  bittest 0, 1, 2, 4, 8, c0
          pPacket.getByte;  //+17  shl 2
          Name := pPacket.getPascalString;//+18
          TypeTag := pPacket.getPascalString;
            { the destination is usually set in the update packet before the client
              requests the NewObject.  This may not be a good idea, but it is the
              easy fix }
          pOldObject := FDAOCObjs.FindByInfoID(tmpObject.InfoID);
          if Assigned(pOldObject) and not pOldObject.IsStale and
            (pOldObject is TDAOCMovingObject) then begin
            DestinationX := pOldObject.DestinationX;
            DestinationY := pOldObject.DestinationY;
            DestinationZ := pOldObject.DestinationZ;
          end;
        end;  { with TDAOCMob }
      end;  { ocMob }

    ocPlayer:
      begin
        tmpObject := TDAOCPlayer.Create;
        with TDAOCPlayer(tmpObject) do begin
          PlayerID := pPacket.getShort;
          InfoID := pPacket.getShort;
{$IFDEF PRE_168}
          X := pPacket.getLong;
          Y := pPacket.getLong;
          Z := pPacket.getShort;
          HeadWord := pPacket.getShort;
          pPacket.seek(4);
          Realm := TDAOCRealm(pPacket.getByte);
          Level := pPacket.getByte;
          pPacket.seek(2);
          Name := pPacket.getPascalString;
          Guild := pPacket.getPascalString;
          LastName := pPacket.getPascalString;
{$ELSE}
          X := pPacket.getShort;
          Y := pPacket.getShort;
          AdjustObjLocForZone(tmpObject, pPacket.getByte);
          pPacket.seek(1);
          Z := pPacket.getShort;
          HeadWord := pPacket.getShort;
          pPacket.seek(4);
          Realm := TDAOCRealm(pPacket.getByte);
          Level := pPacket.getByte;
          pPacket.seek(2);
          Name := pPacket.getPascalString;
          Guild := pPacket.getPascalString;
          LastName := pPacket.getPascalString;
{$ENDIF}
          IsInGuild := (FLocalPlayer.Guild <> '') and AnsiSameText(Guild, FLocalPlayer.Guild);
          UpdateRealmRank(TDAOCPlayer(tmpObject));

            { if this guy is in our unknown stealther list, now we know who he is }
          pOldObject := FUnknownStealthers.FindByInfoID(tmpObject.InfoID);
          while Assigned(pOldObject) do begin
            FUnknownStealthers.Delete(pOldObject);
            pOldObject := FUnknownStealthers.FindByInfoID(tmpObject.InfoID);
          end;
        end;  { with TDAOCPlayer }
      end;  { ocPlayer }

      ocVehicle:
        begin
          tmpObject := TDAOCVehicle.Create;
          with TDAOCVehicle(tmpObject) do begin
            InfoID := pPacket.getShort;
            PlayerID := 0;
            pPacket.Seek(2);
            SpeedWord := pPacket.getShort;
            HeadWord := pPacket.getShort;
            X := pPacket.getLong;
            Y := pPacket.getLong;
            Z := pPacket.getShort;
            pPacket.seek(10);
            Name := pPacket.getPascalString;
          end;  { with TDAOCVehicle }
        end;  { ocVehicle }

    else
      tmpObject := nil;
  end;  { case AClass }

  if Assigned(tmpObject) then
    SafeAddDAOCObjectAndNotify(tmpObject);
end;

procedure TDAOCConnection.ParseObjectEquipment(pPacket: TGameNetPacket);
var
  ID:   integer;
  iCnt: integer;
  bSlot:    byte;
  obj_id:   WORD;
  obj_color:  byte;
  tmpInvItem: TDAOCInventoryItem;
  pMob:   TDAOCObject;
begin
  pPacket.HandlerName := 'ObjectEquipment';
  ID := pPacket.getShort;
  pMob := FDAOCObjs.FindByInfoID(ID);
  if not Assigned(pMob) or not (pMob is TDAOCMovingObject) then
    exit;
  pPacket.seek(2);  // FF FF?  bitfield 2 bits each or ff
  iCnt := pPacket.getByte;

  while (iCnt > 0) and not pPacket.EOF do begin
    obj_id := 0;
    obj_color := 0;
    bSlot := pPacket.getByte;

    case bSlot of
      $0a..$0d,  // L, R, 2h, Ranged, Thrown
      $15..$17,  // head, hands, feet
      $19..$1c:  // chest, cloak, legs, sleeves
        begin
          obj_id := pPacket.getShort;    // which object list the index is for
          if (obj_id and $4000) <> 0 then
            obj_color := pPacket.getByte;
          if (obj_id and $2000) <> 0 then
            pPacket.getShort;  // particle effect?
          if (obj_id and $8000) <> 0 then
            pPacket.getShort;  // guild emblem?
        end;  { if in a real slot }
    end;  { case bSlot }

    tmpInvItem := TDAOCInventoryItem.Create;
    tmpInvItem.Slot := bSlot;
    tmpInvItem.ItemID := obj_id and $0fff;
    tmpInvItem.Color := obj_color;
    DoMobInventoryUpdate(TDAOCMovingObject(pMob), tmpInvItem);

    dec(iCnt);
  end;  { while cnt and !EOF }

  TDAOCMovingObject(pMob).InventoryChanged;
end;

procedure TDAOCConnection.ParseMoneyUpdate(pPacket: TGameNetPacket);
var
  dwPrevious: Cardinal;
begin
  pPacket.HandlerName := 'MoneyUpdate';

  dwPrevious := FLocalPlayer.Currency.AsCopper;

  with FLocalPlayer.Currency do begin
    Copper := pPacket.getByte;
    Silver := pPacket.getByte;
    Gold := pPacket.getShort;
    Mithril := pPacket.getShort;
    Platinum := pPacket.getShort;
  end;  { with FLocalPlayer.Currency }

  DoOnCurrencyChanged(FLocalPlayer.Currency.AsCopper - dwPrevious);
end;

procedure TDAOCConnection.DoOnCurrencyChanged(ADeltaAsCopper: integer);
begin
  if Assigned(FOnCurrencyChanged) then
    FOnCurrencyChanged(Self, FLastCurrencyChangeReason, ADeltaAsCopper);
    
  FLastCurrencyChangeReason := ccrUnknown;
end;

procedure TDAOCConnection.ParseRequestBuyItem(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'RequestBuyItem NOTIMPL';
end;

procedure TDAOCConnection.ParseSelectedIDUpdate(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'SelectedIDUpdate';
  FSelectedID := pPacket.getShort;

  DoOnSelectedObjectChanged(SelectedObject);
end;

procedure TDAOCConnection.ParseProgressMeter(pPacket: TGameNetPacket);
var
  iDuration:  integer;
  sMessage:   string;
begin
  pPacket.HandlerName := 'ProgressMeter';
  iDuration := pPacket.getShort;
  pPacket.seek(2);  // message length? / line count?
  sMessage := pPacket.getNullTermString(0);
  if iDuration = 0 then
    DoOnProgressMeterClose
  else
    DoOnProgressMeterOpen(sMessage);
end;

procedure TDAOCConnection.DoOnProgressMeterClose;
begin
;
end;

procedure TDAOCConnection.DoOnProgressMeterOpen(const AMessage: string);
begin
;
end;

procedure TDAOCConnection.ParseSpellPulse(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'SpellPulse NOTIMPL';
end;

procedure TDAOCConnection.ParsePopupMessage(pPacket: TGameNetPacket);
var
  sMessage:   string;
begin
  pPacket.HandlerName := 'PopupMessage';
  pPacket.seek(12);
  sMessage := pPacket.getNullTermString(0);
  DoOnPopupMessage(sMessage);
end;

procedure TDAOCConnection.DoOnPopupMessage(const AMessage: string);
begin
;
end;

procedure TDAOCConnection.ParseCommandFromClient(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'CommandFromClient NOTIMPL';
(**
---- TCP packet  TO  server ---- 
00 9B 00 D1 FF FF 00 07 - 00 D1 00 00 00 C4 40 00  ..............@.
26 77 68 69 73 70 65 72 - 20 74 61 73 6B 00 25 06  &whisper task.%.
**)
end;

procedure TDAOCConnection.DoOnChatLogLine(const ALine: string);
begin
  FChatParser.ProcessLine(FormatDateTime('[hh:nn:ss] ', Now) + ALine);

//  if StringBeginsWith(ALine, 'You fail to make ') then
//    LogParseTradeskillFailure(ALine)
//  else if StringBeginsWith(ALine, 'You successfully make ') then
//    LogParseTradeskillSuccess(ALine)
//  else if StringBeginsWith(ALine, 'You have been asked to make a ') then
//    LogParseTradeskillTask2(ALine)
//  else if ALine = FTradeCommissionNPC + ' takes the ' + FTradeCommissionItem + '!' then
//    DoOnTradeskillTaskCompleted
//  else if StringContains(ALine, ' made for ') then
//    LogParseTradeskillTask(ALine)
end;

(***
procedure TDAOCConnection.LogParseTradeskillTask(const ALine: string);
var
  iPos:   integer;
  sWord:  string;
begin
    { Asre says, "Gothi of Modi wants a fine alloy bearded axe made for him. ..." }
  FTradeCommissionNPC := '';
  FTradeCommissionItem := '';

  iPos := Pos('"', ALine);
  sWord := '';

  sWord := ParseWord(ALine, iPos);
  while sWord <> 'wants' do begin
    if FTradeCommissionNPC <> '' then
      FTradeCommissionNPC := FTradeCommissionNPC + ' ' + sWord
    else
      FTradeCommissionNPC := sWord;
    sWord := ParseWord(ALine, iPos);
  end;

    { this should be 'a' }
  ParseWord(ALine, iPos);

  sWord := ParseWord(ALine, iPos);
  while sWord <> 'made' do begin
    if FTradeCommissionItem <> '' then
      FTradeCommissionItem := FTradeCommissionItem + ' ' + sWord
    else
      FTradeCommissionItem := sWord;
    sWord := ParseWord(ALine, iPos);
  end;

  DoOnTradeCommissionAssigned;
end;

procedure TDAOCConnection.LogParseTradeskillTask2(const ALine: string);
var
  iPos:   integer;
  sWord:  string;
begin
    { You have been asked to make a fine alloy bearded axe for Synna. " }
  FTradeCommissionNPC := '';
  FTradeCommissionItem := '';

  iPos := 30;
  sWord := '';

  sWord := ParseWord(ALine, iPos);
  while sWord <> 'for' do begin
    if FTradeCommissionItem <> '' then
      FTradeCommissionItem := FTradeCommissionItem + ' ' + sWord
    else
      FTradeCommissionItem := sWord;
    sWord := ParseWord(ALine, iPos);
  end;

  FTradeCommissionNPC := copy(ALine, iPos + 1, Length(ALine) - iPos - 1);
  DoOnTradeCommissionAssigned;
end;
***)

procedure TDAOCConnection.DoOnTradeCommissionAssigned;
begin
;
end;

procedure TDAOCConnection.ClearCallbackList;
var
  I:    integer;
begin
  for I := 0 to FScheduledCallbacks.Count - 1 do
    Dispose(PCallbackEventInfo(FScheduledCallbacks[I]));

  FScheduledCallbacks.Clear;
end;

procedure TDAOCConnection.ScheduleCallback(ATimeout: Cardinal;
  ACallback: TSheduledCallback; AParm: Cardinal);
var
  pInfo:  PCallbackEventInfo;
begin
  New(pInfo);
  FScheduledCallbacks.Add(pInfo);
  pInfo^.dwTime := GlobalTickCount + ATimeout;
  pInfo^.fn := ACallback;
  pInfo^.parm := AParm;
end;

procedure TDAOCConnection.CheckScheduledTimeoutCallback;
var
  I:      integer;
  dwTime: Cardinal;
  pItem:  PCallbackEventInfo;
begin
  I := 0;
  dwTime := GlobalTickCount;
  while I < FScheduledCallbacks.Count do begin
    pItem := PCallbackEventInfo(FScheduledCallbacks[I]);
    if pItem^.dwTime < dwTime then begin
      pItem^.fn(Self, pItem^.parm);
      FScheduledCallbacks.Delete(I);
    end
    else
      inc(I);
  end;  { while I }
end;

procedure TDAOCConnection.DoOnTradeskillTaskCompleted;
begin
  FTradeCommissionItem := '';
  FTradeCommissionNPC := '';
end;

procedure TDAOCConnection.ParseVendorWindow(pPacket: TGameNetPacket);
var
  iItemDescs: integer;
  iPage:      integer;
  pItem:      TDAOCVendorItem;
begin
  pPacket.HandlerName := 'VendorWindow';

  iItemDescs := pPacket.getByte;
  iPage := pPacket.getShort;
  pPacket.seek(1);

    { clear last vendor window if this a send of the first page }
  if iPage = 0 then begin
    MergeVendorItemsToMaster;
    FVendorItems.Clear;
  end;

  if Assigned(SelectedObject) and (SelectedObject is TDAOCMob) then
    FVendorItems.Vendor.Assign(TDAOCMob(SelectedObject));

  while iItemDescs > 0 do begin
    pItem := TDAOCVendorItem.Create;
    FVendorItems.Add(pItem);

    pItem.Page := iPage;
    pItem.Position := pPacket.getByte;
    pItem.Quantity := pPacket.getShort;
    pPacket.seek(6);
    pItem.Cost := pPacket.getLong;
    pPacket.seek(2);
    pItem.Name := pPacket.getPascalString;

    dec(iItemDescs);
  end;  { while items left to parse }

  if Assigned(FOnVendorWindow) then
    FOnVendorWindow(Self);
end;

function TDAOCConnection.GetSelectedObject: TDAOCObject;
begin
  if FSelectedID = 0 then
    Result := nil
  else if Assigned(FSelectedObjectCached) and (FSelectedObjectCached.InfoID = FSelectedID) then
    Result := FSelectedObjectCached
  else if FSelectedID = FLocalPlayer.InfoID then
    Result := FLocalPlayer
  else
    Result := FDAOCObjs.FindByInfoID(FSelectedID);

  FSelectedObjectCached := Result;
end;

procedure TDAOCConnection.ParseRegionServerInfomation(
  pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'RegionServerInformation NOTIMPL';
end;

procedure TDAOCConnection.ParseDeleteObject(pPacket: TGameNetPacket);
var
  pObj:   TDAOCObject;
  wID:    WORD;
begin
  pPacket.HandlerName := 'DeleteObject';
  wID := pPacket.getShort;

  pObj := FDAOCObjs.FindByInfoID(wID);
  if Assigned(pObj) then begin
    DoOnDeleteDAOCObject(pObj);
    FDAOCObjs.Delete(pObj);
  end

  else begin
    pObj := FDAOCObjsStale.FindByInfoID(wID);
    if Assigned(pObj) then
      FDAOCObjsStale.Delete(pObj);
  end;
end;

procedure TDAOCConnection.DoOnDeleteDAOCObject(AObject: TDAOCObject);
var
  iIdx:   integer;
begin
  if FSelectedObjectCached = AObject then
    FSelectedObjectCached := nil;
    
  FDAOCObjs.WalkList(OBJWALKRemoveTarget, Integer(AObject));

    { make sure we don't have them in our group list }
  iIdx := FGroupMembers.IndexOf(AObject);
  if iIdx <> -1 then begin
    TDAOCPlayer(AObject).IsInGroup := false;
    FGroupMembers.Delete(iIdx);
    DoOnGroupMembersChanged;
  end;

  if Assigned(FOnDeleteDAOCObject) then
    FOnDeleteDAOCObject(Self, AObject);
end;

procedure TDAOCConnection.DoOnNewDAOCObject(AObject: TDAOCObject);
begin
  if Assigned(FOnNewDAOCObject) then
    FOnNewDAOCObject(Self, AObject);
end;

procedure TDAOCConnection.DoOnSelectedObjectChanged(AObject: TDAOCObject);
begin
    { the vendor window goes away when we select another person }
  MergeVendorItemsToMaster;
  FVendorItems.Clear;

  if Assigned(FOnSelectedObjectChange) then
    FOnSelectedObjectChange(Self, AObject);
end;

procedure TDAOCConnection.MergeVendorItemsToMaster;
begin
  FMasterVendorList.AddOrUpdate(FVendorItems);
end;

procedure TDAOCConnection.DoOnTradeskillFailure;
begin
  if Assigned(FOnTradeskillFailure) then
    FOnTradeskillFailure(Self);
end;

procedure TDAOCConnection.DoOnTradeSkillFailureWithLoss;
begin
  if Assigned(FOnTradeSkillFailureWithLoss) then
    FOnTradeSkillFailureWithLoss(Self);
end;

procedure TDAOCConnection.DoOnTradeskillSuccess(AQuality: integer);
begin
  if Assigned(FOnTradeskillSuccess) then
    FOnTradeskillSuccess(Self, AQuality);
end;

procedure TDAOCConnection.HookChatParseCallbacks;
begin
  FChatParser.OnTradeSkillSuccess := CPARSETradeSkillSuccess;
  FChatParser.OnTradeSkillFailure := CPARSETradeSkillFailure;
  FChatParser.OnTradeSkillFailureWithLoss := CPARSETradeSkillFailureWithLoss;
  FChatParser.OnTradeskillCapped := CPARSETradeSkillCapped;
  FChatParser.OnTargetChange := CPARSETargetChanged;
  FChatParser.OnCurrencyChange := CPARSECurrencyChanged;
  FChatParser.OnRealmPointsChange := CPARSERealmPointsChange;
  FChatParser.OnBountyPointsChange := CPARSEBountyPointsChange;
end;

procedure TDAOCConnection.CPARSETradeSkillCapped(ASender: TDAOCChatParser);
begin
  DoOnTradeSkillCapped;
end;

procedure TDAOCConnection.CPARSETradeSkillFailure(ASender: TDAOCChatParser);
begin
  DoOnTradeSkillFailure;
end;

procedure TDAOCConnection.CPARSETradeSkillSuccess(ASender: TDAOCChatParser;
  AQuality: integer);
begin
  DoOnTradeSkillSuccess(AQuality);
end;

procedure TDAOCConnection.DoOnTradeSkillCapped;
begin
  if Assigned(FOnTradeSkillCapped) then
    FOnTradeSkillCapped(Self);
end;

procedure TDAOCConnection.ProcessDAOCPacket(pPacket: TGameNetPacket);
begin
    { if we're already active and we have a preferred connection ID, then
      make sure this packet is for the right connection }
  if FActive and (FConnectionID <> 0) and
    (pPacket.ConnectionID <> FConnectionID) then
    exit;

{$IFDEF GLOBAL_TICK_COUNTER}
  UpdateGlobalTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}

  if not FActive then begin
    Clear;
    FConnectionID := pPacket.ConnectionID;
    DoOnConnect;
  end;

  if Assigned(FOnPacket) then
    FOnPacket(Self, pPacket);

  pPacket.HandlerName := '';

  if pPacket.IsFromClient then
    ProcessDAOCPacketFromClient(pPacket)
  else
    ProcessDAOCPacketFromServer(pPacket);

  if Assigned(FOnAfterPacket) then
    FOnAfterPacket(Self, pPacket);

  CheckScheduledTimeoutCallback;
end;

procedure TDAOCConnection.ParseSetGroundTarget(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'SetGroundTarget';
  FGroundTarget.X := pPacket.getLong;
  FGroundTarget.Y := pPacket.getLong;
  FGroundTarget.Z := pPacket.getLong;
  
  DoOnSetGroundTarget;
end;

procedure TDAOCConnection.DoOnSetGroundTarget;
begin
  if Assigned(FOnSetGroundTarget) then
    FOnSetGroundTarget(Self);
end;

procedure TDAOCConnection.DoOnChatBroadcast(const AMessage: string);
begin

end;

procedure TDAOCConnection.DoOnChatSayIncoming(const AWho, AMessage: string);
begin
  if Assigned(FOnChatSayIncoming) then
    FOnChatSayIncoming(Self, AWho, AMessage);
end;

procedure TDAOCConnection.DoOnChatSayOutgoing(const AMessage: string);
begin
  if Assigned(FOnChatSayOutgoing) then
    FOnChatSayOutgoing(Self, AMessage);
end;

procedure TDAOCConnection.DoOnChatSendIncoming(const AWho, AMessage: string);
begin
  if Assigned(FOnChatSendIncoming) then
    FOnChatSendIncoming(Self, AWho, AMessage);
end;

procedure TDAOCConnection.DoOnChatSendOutgoing(const AMessage: string);
begin
  if Assigned(FOnChatSendOutgoing) then
    FOnChatSendOutgoing(Self, AMessage);
end;

procedure TDAOCConnection.ChatSay(const ALine: string);
var
  sWho:   string;
  iPos:   integer;
begin
  if StringBeginsWith(ALine, '@@You say, "') then
    DoOnChatSayOutgoing(copy(ALine, 12, Length(ALine) - 13))
  else begin
    iPos := 3;
    sWho := ParseWord(ALine, iPos);
    DoOnChatSayIncoming(sWho, copy(ALine, iPos + 9, Length(ALine) - iPos - 9));
  end;
end;

procedure TDAOCConnection.ChatSend(const ALine: string);
var
  sWho:   string;
  iPos:   integer;
begin
  if StringBeginsWith(ALine, '@@You send, "') then
    DoOnChatSendOutgoing(copy(ALine, 13, Length(ALine) - 14))
  else begin
    iPos := 3;
    sWho := ParseWord(ALine, iPos);
    DoOnChatSendIncoming(sWho, copy(ALine, iPos + 9, Length(ALine) - iPos - 9));
  end;
end;

procedure TDAOCConnection.DoOnDAOCObjectMoved(AObject: TDAOCObject);
begin
  if Assigned(FOnDAOCObjectMoved) then
    FOnDAOCObjectMoved(Self, AObject);
end;

procedure TDAOCConnection.CPARSECombatStyleFailure(ASender: TDAOCChatParser);
begin
  DoOnCombatStyleFailure;
end;

procedure TDAOCConnection.CPARSECombatStyleSuccess(ASender: TDAOCChatParser);
begin
  DoOnCombatStyleSuccess(ASender.AttackStats.LastMOB.OutDamage.UpcomingStyle.Style);
end;

procedure TDAOCConnection.DoOnCombatStyleFailure;
begin
  if Assigned(FOnCombatStyleFailure) then
    FOnCombatStyleFailure(Self);
end;

procedure TDAOCConnection.DoOnCombatStyleSuccess(AStyle: string);
begin
  if Assigned(FOnCombatStyleSuccess) then
    FOnCombatStyleSuccess(Self, AStyle);
end;

procedure TDAOCConnection.SetSelectedObject(const Value: TDAOCObject);
begin
  if FSelectedID <> Value.InfoID then begin
    FSelectedID := Value.InfoID;
    DoOnSelectedObjectChanged(Value);
  end;
end;

procedure TDAOCConnection.ParseCharacterStealthed(pPacket: TGameNetPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
  pUnkStealther:  TDAOCUnknownStealther;
begin
  pPacket.HandlerName := 'CharacterStealthed';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByInfoID(wID);
  if Assigned(pDAOCObject) then
    pDAOCObject.Stealthed := true
  else begin
    pUnkStealther := TDAOCUnknownStealther.Create;
    pUnkStealther.InfoID := wID;
    pUnkStealther.X := FLocalPlayer.XProjected;
    pUnkStealther.Y := FLocalPlayer.YProjected;
    pUnkStealther.Z := FLocalPlayer.Z;
    FUnknownStealthers.Add(pUnkStealther);

    DoOnUnknownStealther(pUnkStealther);
  end;
end;

procedure TDAOCConnection.ClearDAOCObjectList;
var
  pDAOCObject:  TDAOCObject;
begin
  pDAOCObject := FDAOCObjs.Head;
    { delete all the daocobjects, calling the delete event handler }
  while Assigned(pDAOCObject) do begin
    DoOnDeleteDAOCObject(pDAOCObject);
    pDAOCObject := FDAOCObjs.Delete(pDAOCObject);
  end;

  FDAOCObjsStale.Clear;  // we can just clear because we have notified
  FUnknownStealthers.Clear;
end;

procedure TDAOCConnection.CheckObjectsOutOfRange;
var
  fDist:  double;
  pDAOCObject:  TDAOCObject;
begin
  pDAOCObject := FDAOCObjs.Head;
  while Assigned(pDAOCObject) do
    if pDAOCObject.ObjectClass = ocObject then begin
      fDist := pDAOCObject.DistanceSqr3D(FLocalPlayer);
        { if object is just a base object }
      if fDist > FMaxObjectDistSqr then begin
        DoOnDeleteDAOCObject(pDAOCObject);
        pDAOCObject := FDAOCObjs.Delete(pDAOCObject);
      end
      else
        pDAOCObject := pDAOCObject.Next;
    end  { if object is a base non-moving DAOC object }
    else
      pDAOCObject := pDAOCObject.Next;
end;

procedure TDAOCConnection.SetMaxObjectDistance(const Value: double);
begin
  FMaxObjectDist := Value;
  FMaxObjectDistSqr := Value * Value;
end;

procedure TDAOCConnection.DoVersionNumsSet;
begin
  if Assigned(FOnVersionNumsSet) then
    FOnVersionNumsSet(Self, FVersionMajor, FVersionMinor, FVersionRelease);
end;

procedure TDAOCConnection.ParseCharacterActivationRequest(
  pPacket: TGameNetPacket);
var
  sCharName:  string;
begin
  pPacket.HandlerName := 'CharacterActivationRequest';
  pPacket.seek(4);
  sCharName := pPacket.getNullTermString(28);
  SetActiveCharacterByName(sCharName);
  { account name follows as an NTS, but who cares }
end;

function TDAOCConnection.SetActiveCharacterByName(const ACharacterName: string) : TDAOCAccountCharInfo;
begin
  Result := FAccountCharacters.FindOrAddChar(ACharacterName);

  FLocalPlayer.Level := Result.Level;
  FLocalPlayer.Realm := Result.Realm;
  FLocalPlayer.Name := Result.Name;
  DoSetRegionID(Result.RegionID);
end;

procedure TDAOCConnection.ParseServerProtocolInit(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'ServerProcotolInit';
  
  FServerProtocol := pPacket.GetByte;
  pPacket.seek(3);
  FAccountCharacters.AccountName := pPacket.getPascalString;
  FAccountCharacters.ServerName := pPacket.getPascalString;
end;

procedure TDAOCConnection.SetClientIP(const Value: string);
begin
  FClientAddr := inet_addr(PChar(Value));
end;

procedure TDAOCConnection.SetServerIP(const Value: string);
begin
  FServerAddr := inet_addr(PChar(Value));
end;

procedure TDAOCConnection.ParseRequestPlayerByPlayerID(
  pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'RequestPlayerByPlayerID';
  // pPacket.getShort;  // PlayerID
end;

procedure TDAOCConnection.ParseRequestObjectByInfoID(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'RequestObjectByInfoID';
end;

procedure TDAOCConnection.CheckForStaleObjects;
var
  pObj:   TDAOCObject;
  pNext:  TDAOCObject;
begin
    { remove really really old stuff from the stale list }
  pObj := FDAOCObjsStale.Head;
  while Assigned(pObj) do
    if pObj.TicksSinceUpdate > FMaxObjectStaleTime then
      pObj := FDAOCObjsStale.Delete(pObj)
    else
      pObj := pObj.Next;

  pObj := FDAOCObjs.Head;
  while Assigned(pObj) do begin
    pObj.CheckStale;
    if pObj.IsStale then begin
      DoOnDeleteDAOCObject(pObj);
      pNext := FDAOCObjs.Remove(pObj);
      FDAOCObjsStale.Add(pObj);
      pObj := pNext;
    end
    else
      pObj := pObj.Next;
  end;  { while pObj }

  pObj := FUnknownStealthers.Head;
  while Assigned(pObj) do begin
    pObj.CheckStale;
    if pObj.IsStale then
      pObj := FUnknownStealthers.Delete(pObj)
    else
      pObj := pObj.Next;
  end;  { while pObj }
end;

procedure TDAOCConnection.ParsePlayerCenteredSpellEffect(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'PlayerCenteredSpellEffect NOTIMPL';
  { I think this is a "Player-Centered" spell effect.  Like a spell effect
    that will follow a player as they move, like a buff or run chant effect
    B3 18 32 0E 1C 18 32 00 - 00 01 01 00 00           ..2...2......
  }
  // ID := pPacket.getShort;
end;

procedure TDAOCConnection.ParseServerPingResponse(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'ServerPingResponse';
  { the server sends us back a copy of the timer we sent }
  // FPingRequestNumber = pPacket.getLong;

  FLastPingTime := GlobalTickCount - FPingRequestSentTime;
  DoOnPingReply;
end;

procedure TDAOCConnection.ParseServerPingRequest(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'ServerPingRequest';
    { We can't use the client's tick count for the ping request time, since
      that wouldn't work when we play back.  Just approximate it }
  //pPacket.seek(4);
  //FPingRequestSentTime := pPacket.getLong;
  FPingRequestSentTime := GlobalTickCount;
end;

procedure TDAOCConnection.DoOnPingReply;
begin
  if Assigned(FOnPingReply) then
    FOnPingReply(Self, FLastPingTime);
end;

procedure TDAOCConnection.ParseGroupMembersUpdate(pPacket: TGameNetPacket);
var
  iCnt:   integer;
  wID:    WORD;
  pObj:   TDAOCObject;
  bMana:  BYTE;
begin
  pPacket.HandlerName := 'GroupMembersUpdate';
  ResetPlayersInGroup;
  iCnt := pPacket.getByte;
  if iCnt = 0 then
    exit;

  pPacket.seek(2);
  while not pPacket.EOF do begin
    pPacket.getByte; // level
    pPacket.getByte; // health
    bMana := pPacket.getByte; 
    pPacket.seek(1);
    wID := pPacket.getShort;

    pObj := FDAOCObjs.FindByInfoID(wID);
    if Assigned(pObj) and (pObj.ObjectClass = ocPlayer) then begin
      FGroupMembers.Add(pObj);
      TDAOCPlayer(pObj).IsInGroup := true;
      TDAOCPlayer(pObj).ManaPct := bMana;
    end
    else if wID <> FLocalPlayer.InfoID then
      Log('GroupMembersUpdate: Can not find player by InfoID 0x' + IntToHex(wID, 4));

    pPacket.getPascalString;  // name
    pPacket.getPascalString;  // class
  end;  { while !EOF }

  DoOnGroupMembersChanged;
end;

procedure TDAOCConnection.ParseGroupWindowUpdate(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'GroupWindowUpdate? NOTIMPL';
end;

procedure TDAOCConnection.UpdatePlayersInGuild;
begin
  FDAOCObjs.WalkList(OBJWALKResetGuild);
end;

procedure TDAOCConnection.ResetPlayersInGroup;
begin
  FDAOCObjs.WalkList(OBJWALKResetGroup);
  FGroupMembers.Clear;
end;

procedure TDAOCConnection.ParseAggroIndicator(pPacket: TGameNetPacket);
(*
var
  wTarget:  WORD;
  wAggressor: WORD;
  pTarget:    TDAOCObject;
  pAggressor: TDAOCObject;
*)
begin
  pPacket.HandlerName := 'Aggro/ChaseIndicator?';
(* BRY: removed because I'm not sure about the validity of the packet type.
  the code works though ;)
  wTarget := pPacket.getShort;
  wAggressor := pPacket.getShort;

  pAggressor := FDAOCObjs.FindByInfoID(wAggressor);
  SetAggressorTarget(pAggressor, wTarget);
*)
end;

procedure TDAOCConnection.DoOnMobTargetChanged(AMob: TDAOCMob);
begin
  if Assigned(FOnMobTargetChanged) then
    FOnMobTargetChanged(Self, AMob);
end;

function TDAOCConnection.CheckAndMoveFromStaleListByInfoID(wID: WORD): TDAOCObject;
begin
  Result := FDAOCObjsStale.FindByInfoID(wID);
  if Assigned(Result) then begin
    FDAOCObjsStale.Remove(Result);
      { we shouldn't need to do an AddOrReplace (and notify on delete of old)
        because this function should only be called if the InfoID wasn't in
        the list already }
    DAOCObjects.Add(Result);
    DoOnNewDAOCObject(Result);
  end;
end;

function TDAOCConnection.CheckAndMoveFromStaleListByPlayerID(wID: WORD): TDAOCObject;
begin
  Result := FDAOCObjsStale.FindByPlayerID(wID);
  if Assigned(Result) then begin
    FDAOCObjsStale.Remove(Result);
    SafeAddDAOCObjectAndNotify(Result);
  end;
end;

procedure TDAOCConnection.SafeAddDAOCObjectAndNotify(ADAOCObject: TDAOCObject);
{ does an add by infoid into the object list, deleting an existing one of the
  same ID if it exists, and notifying on the delete and the add }
var
  pOldObject: TDAOCObject;
begin
  pOldObject := FDAOCObjs.FindByInfoID(ADAOCObject.InfoID);
  if Assigned(pOldObject) then begin
    DoOnDeleteDAOCObject(pOldObject);
    FDAOCObjs.Delete(pOldObject);
  end;
  
  FDAOCObjs.Add(ADAOCObject);
  DoOnNewDAOCObject(ADAOCObject);
end;

procedure TDAOCConnection.ParseAccountLoginRequest(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'AccountLoginRequest';
  pPacket.seek(5);
  FAccountCharacters.AccountPassword := pPacket.getNullTermString(11);
    { there's some other information here, like the account name, but its
      at the end of some other ??? data, so we'll wait until the server
      sends back the account name }
end;

procedure TDAOCConnection.CPARSETargetChanged(ASender: TDAOCChatParser);
var
  pSelected:  TDAOCObject;
begin
  pSelected := SelectedObject;
    { if this player is an invader then update their name }
  if Assigned(pSelected) and (pSelected.Realm <> FLocalPlayer.Realm) and
    (pSelected is TDAOCPlayer) then
    if pSelected.Name <> FChatParser.Target then begin
      pSelected.Name := FChatParser.Target;
      DoOnDAOCObjectMoved(pSelected);
    end;
end;

procedure TDAOCConnection.DoOnUnknownStealther(AUnk: TDAOCUnknownStealther);
begin
  if Assigned(FOnUnknownStealther) then
    FOnUnknownStealther(Self, AUnk);
end;

procedure TDAOCConnection.ParseDelveRequest(pPacket: TGameNetPacket);
begin
  pPacket.HandlerName := 'DelveRequest';
  pPacket.seek(3);
    { save what body position we delved since we don't get that back
      with the delve info }
  FLastDelveRequestPos := pPacket.getByte;
end;

procedure TDAOCConnection.ParseDelveInformation(pPacket: TGameNetPacket);
var
  sItemName:    string;
  s:            string;
  iLineNo:      integer;
  slDelveInfo:  TStrings;
  pItem:        TDAOCInventoryItem;
begin
  pPacket.HandlerName := 'DelveInformation';
  pItem := FLocalPlayer.Inventory.ItemInSlot(FLastDelveRequestPos);
  if not Assigned(pItem) then
    exit;

  sItemName := pPacket.getPascalString;
  slDelveInfo := pItem.DelveInfo;
  if Assigned(slDelveInfo) then
    slDelveInfo.Clear
  else begin
    slDelveInfo := TStringList.Create;
    pItem.DelveInfo := slDelveInfo;
  end;

  while not pPacket.EOF do begin
    iLineNo := pPacket.getByte;
    if iLineNo <> 0 then begin
      s := pPacket.getPascalString;
      if Trim(s) <> '' then
        slDelveInfo.Add(s);
    end;
  end;  { while !EOF }

  DoOnDelveItem(pItem);
end;

procedure TDAOCConnection.DoOnDelveItem(AItem: TDAOCInventoryItem);
begin
  if Assigned(FOnDelveItem) then
    FOnDelveItem(Self, AItem);
end;

procedure TDAOCConnection.CPARSEBountyPointsChange(ASender: TDAoCChatParser; APoints: Integer);
begin
  if Assigned(FOnBountyPointsChanged) then
    FOnBountyPointsChanged(Self, APoints);
end;

procedure TDAOCConnection.CPARSECurrencyChanged(ASender: TDAOCChatParser;
  AReason: TDAOCCurrencyChangeReason);
begin
  FLastCurrencyChangeReason := AReason;
end;

procedure TDAOCConnection.CPARSERealmPointsChange(ASender: TDAoCChatParser; APoints: Integer);
begin
  if Assigned(FOnRealmPointsChanged) then
    FOnRealmPointsChanged(Self, APoints);
end;

procedure TDAOCConnection.CPARSETradeSkillFailureWithLoss(ASender: TDAOCChatParser);
begin
  DoOnTradeSkillFailureWithLoss;
end;

procedure TDAOCConnection.UpdateRealmRank(AObj: TDAOCPlayer);
var
  sRank:    string;
begin
  if Assigned(FRealmRanks) then begin
    sRank := FRealmRanks.Values[AObj.LastName];
    if sRank = '' then
      AObj.RealmRank := rrUnknown
    else
      AObj.RealmRank := TRealmRank(StrToIntDef(sRank, 0));
  end;
end;

procedure TDAOCConnection.LoadRealmRanks(const AFileName: string);
begin
  if not Assigned(FRealmRanks) then
    FRealmRanks := TStringList.Create;

  FRealmRanks.LoadFromFile(AFileName);
  FRealmRanks.Sorted := true;
end;

procedure TDAOCConnection.ParseVendorWindowRequest(pPacket: TGameNetPacket);
var
  wID:    WORD;
  pMob:   TDAOCMob;
begin
  pPacket.HandlerName := 'VendorWindowRequest';
  pPacket.Seek(10);
  wID := pPacket.getShort;
  pMob := TDAOCMob(FDAOCObjs.FindByInfoID(wID));

  if Assigned(pMob) then
    DoOnVendorWindowRequest(pMob)
  else
    Log('VendorWindowRequest for unknown InfoID 0x' + IntToHex(wID, 4));
end;

procedure TDAOCConnection.DoOnVendorWindowRequest(AMob: TDAOCMob);
begin
end;

procedure TDAOCConnection.DoOnLocalHealthUpdate;
begin
  if Assigned(FOnLocalHealthUpdate) then
    FOnLocalHealthUpdate(Self);
end;

procedure TDAOCConnection.SetAggressorTarget(AAggressor: TDAOCObject; wTargetID: WORD);
var
  pTarget:    TDAOCObject;
begin
  if AAggressor is TDAOCMob then begin
      { first check if the mob it getting unaggressive }
    if wTargetID = 0 then begin
      if Assigned(TDAOCMob(AAggressor).Target) then begin
        TDAOCMob(AAggressor).Target := nil;
        DoOnMobTargetChanged(TDAOCMob(AAggressor));
      end;

      exit;
    end;  { if targetid = 0 }

      { next see if this is just a re-send of a known target }
    if Assigned(TDAOCMob(AAggressor).Target) and
      (TDAOCMob(AAggressor).Target.InfoID = wTargetID) then
      exit;

      { finally, find the target and link em }
    if FLocalPlayer.InfoID = wTargetID then
      pTarget := FLocalPlayer
    else
      pTarget := FDAOCObjs.FindByInfoID(wTargetID);

    if Assigned(pTarget) and (pTarget is TDAOCMovingObject) then begin
      TDAOCMob(AAggressor).Target := TDAOCMovingObject(pTarget);
      DoOnMobTargetChanged(TDAOCMob(AAggressor));
    end;
  end;
end;

procedure TDAOCConnection.DoMobInventoryUpdate(AMob: TDAOCMovingObject; AItem: TDAOCInventoryItem);
begin
  AMob.Inventory.TakeItem(AItem);
end;

procedure TDAOCConnection.DoOnGroupMembersChanged;
begin
  if Assigned(FOnGroupMembersChanged) then
    FOnGroupMembersChanged(Self);
end;

procedure TDAOCConnection.ParseDoorPositionUpdate(pPacket: TGameNetPacket);
var
  pObj:   TDAOCObject;
  dwDoor: Cardinal;
  bState: Boolean;
begin
  pPacket.HandlerName := 'DoorPositionUpdate';
  dwDoor := pPacket.getLong;
  bState := pPacket.getByte = 1;

  pObj := FDAOCObjs.Head;
  while Assigned(pObj) do begin
    if pObj.DoorID = dwDoor then begin
      pObj.DoorIsOpen := bState;
      DoOnDoorPositionUpdate(pObj);
      exit;
    end;
    pObj := pObj.Next;
  end;

  Log('DoorUpdate for unknown door ' + IntToHex(dwDoor, 8));
end;

procedure TDAOCConnection.OBJWALKResetGroup(AObj: TDAOCObject; AParam: Integer;
  var AContinue: boolean);
begin
  if AObj.ObjectClass = ocPlayer then begin
    TDAOCPlayer(AObj).IsInGroup := false;
    TDAOCPlayer(AObj).ManaPct := 0;
  end;
end;

procedure TDAOCConnection.OBJWALKResetGuild(AObj: TDAOCObject; AParam: Integer;
  var AContinue: boolean);
begin
  if AObj.ObjectClass = ocPlayer then
    TDAOCPlayer(AObj).IsInGuild := (FLocalPlayer.Guild <> '') and
      AnsiSameText(TDAOCPlayer(AObj).Guild, FLocalPlayer.Guild);
end;

procedure TDAOCConnection.OBJWALKRemoveTarget(AObj: TDAOCObject;
  AParam: Integer; var AContinue: boolean);
{ AParam is the target we want to remove from everyone, cast to an int }
begin
  if (AObj is TDAOCMob) and (TDAOCMob(AObj).Target = TDAOCObject(AParam)) then begin
    TDAOCMob(AObj).Target := nil;
    DoOnMobTargetChanged(TDAOCMob(AObj));
  end;
end;

procedure TDAOCConnection.DoOnDoorPositionUpdate(AObj: TDAOCObject);
begin
  if Assigned(FOnDoorPositionUpdate) then
    FOnDoorPositionUpdate(Self, AObj);
end;

procedure TDAOCConnection.InitPacketHandlers;
begin
  FServerPacketHandlers.LoadFromFile(FPacketHandlerDefFile, 'Server');
  LinkPacketHandlers(FServerPacketHandlers);

  FClientPacketHandlers.LoadFromFile(FPacketHandlerDefFile, 'Client');
  LinkPacketHandlers(FClientPacketHandlers);
end;

procedure TDAOCConnection.LinkPacketHandlers(AHandlerList: TNamedPacketHandlerList);
var
  I:  integer;
begin
  for I := 0 to AHandlerList.Count - 1 do begin
    with AHandlerList[I] do
        { server handlers }
      if AnsiSameText(Name, 'PlayerPosUpdate') then
        Handler := ParsePlayerPosUpdate
      else if AnsiSameText(Name, 'LocalHealthUpdate') then
        Handler := ParseLocalHealthUpdate
      else if AnsiSameText(Name, 'LogUpdate') then
        Handler := ParseLogUpdate
      else if AnsiSameText(Name, 'MobUpdate') then
        Handler := ParseMobUpdate
      else if AnsiSameText(Name, 'DeleteObject') then
        Handler := ParseDeleteObject
      else if AnsiSameText(Name, 'PlayerHeadUpdate') then
        Handler := ParsePlayerHeadUpdate
      else if AnsiSameText(Name, 'AggroIndicator') then
        Handler := ParseAggroIndicator
      else if AnsiSameText(Name, 'SetPlayerRegion') then
        Handler := ParseSetPlayerRegion
      else if AnsiSameText(Name, 'PopupMessage') then
        Handler := ParsePopupMessage
      else if AnsiSameText(Name, 'DoorPositionUpdate') then
        Handler := ParseDoorPositionUpdate
      else if AnsiSameText(Name, 'RegionServerInfomation') then
        Handler := ParseRegionServerInfomation
      else if AnsiSameText(Name, 'CharacterStealthed') then
        Handler := ParseCharacterStealthed
      else if AnsiSameText(Name, 'MoneyUpdate') then
        Handler := ParseMoneyUpdate
      else if AnsiSameText(Name, 'AccountCharacters') then
        Handler := ParseAccountCharacters
      else if AnsiSameText(Name, 'ProgressMeter') then
        Handler := ParseProgressMeter
      else if AnsiSameText(Name, 'DelveInformation') then
        Handler := ParseDelveInformation
      else if AnsiSameText(Name, 'NewObject') then
        Handler := ParseNewObject
      else if AnsiSameText(Name, 'NewMob') then
        Handler := ParseNewMob
      else if AnsiSameText(Name, 'NewPlayer') then
        Handler := ParseNewPlayer
      else if AnsiSameText(Name, 'ServerPingResponse') then
        Handler := ParseServerPingResponse
      else if AnsiSameText(Name, 'ServerProtocolInit') then
        Handler := ParseServerProtocolInit
      else if AnsiSameText(Name, 'SetEncryptionKey') then
        Handler := ParseSetEncryptionKey
      else if AnsiSameText(Name, 'CharacterLoginInit') then
        Handler := ParseCharacterLoginInit
      else if AnsiSameText(Name, 'InventoryList') then
        Handler := ParseInventoryList
      else if AnsiSameText(Name, 'PlayerCenteredSpellEffect') then
        Handler := ParsePlayerCenteredSpellEffect
      else if AnsiSameText(Name, 'NewVehicle') then
        Handler := ParseNewVehicle
      else if AnsiSameText(Name, 'ObjectEquipment') then
        Handler := ParseObjectEquipment
      else if AnsiSameText(Name, 'PlayerStatsUpdate') then
        Handler := ParsePlayerStatsUpdate
      else if AnsiSameText(Name, 'VendorWindow') then
        Handler := ParseVendorWindow
      else if AnsiSameText(Name, 'SpellPulse') then
        Handler := ParseSpellPulse
      else if AnsiSameText(Name, 'GroupWindowUpdate') then
        Handler := ParseGroupWindowUpdate

        { client handlers }
      else if AnsiSameText(Name, 'LocalPosUpdateFromClient') then
        Handler := ParseLocalPosUpdateFromClient
      else if AnsiSameText(Name, 'CommandFromClient') then
        Handler := ParseCommandFromClient
      else if AnsiSameText(Name, 'ServerPingRequest') then
        Handler := ParseServerPingRequest
      else if AnsiSameText(Name, 'AccountLoginRequest') then
        Handler := ParseAccountLoginRequest
      else if AnsiSameText(Name, 'LocalHeadUpdateFromClient') then
        Handler := ParseLocalHeadUpdateFromClient
      else if AnsiSameText(Name, 'RequestObjectByInfoID') then
        Handler := ParseRequestObjectByInfoID
      else if AnsiSameText(Name, 'SelectedIDUpdate') then
        Handler := ParseSelectedIDUpdate
      else if AnsiSameText(Name, 'SetGroundTarget') then
        Handler := ParseSetGroundTarget
      else if AnsiSameText(Name, 'DelveRequest') then
        Handler := ParseDelveRequest
      else if AnsiSameText(Name, 'RequestPlayerByPlayerID') then
        Handler := ParseRequestPlayerByPlayerID
      else if AnsiSameText(Name, 'CharacterActivationRequest') then
        Handler := ParseCharacterActivationRequest
      else if AnsiSameText(Name, 'VendorWindowRequest') then
        Handler := ParseVendorWindowRequest
      else if AnsiSameText(Name, 'RequestBuyItem') then
        Handler := ParseRequestBuyItem

       { unknown }
      else
        Log('Unknown packet handler function specified: ' + Name);
  end;
end;

procedure TDAOCConnection.ParseNewMob(pPacket: TGameNetPacket);
begin
  ParseNewObjectCommon(pPacket, ocMob);
end;

procedure TDAOCConnection.ParseNewObject(pPacket: TGameNetPacket);
begin
  ParseNewObjectCommon(pPacket, ocObject);
end;

procedure TDAOCConnection.ParseNewPlayer(pPacket: TGameNetPacket);
begin
  ParseNewObjectCommon(pPacket, ocPlayer);
end;

procedure TDAOCConnection.ParseNewVehicle(pPacket: TGameNetPacket);
begin
  ParseNewObjectCommon(pPacket, ocVehicle);
end;

procedure TDAOCConnection.AdjustObjLocForZone(AObj: TDAOCObject;
  AZoneNum: integer);
var
  pZoneBase:  TDAOCZoneInfo;
begin
 if Assigned(FZone) and (FZone.ZoneNum = AZoneNum) then
    pZoneBase := FZone
  else
    pZoneBase := FZoneList.FindZone(AZoneNum);

  if Assigned(pZoneBase) then begin
    AObj.X := pZoneBase.ZoneToWorldX(AObj.X);
    AObj.Y := pZoneBase.ZoneToWorldY(AObj.Y);
  end;

    { if we're adjusting our player position and the found zone isn't the
      one we're in, then adjust our current zone to the new one }
  if (AObj = FLocalPlayer) and (pZoneBase <> FZone) then begin
    FZone := pZoneBase;
    FRegionID := FZone.Region;
    DoOnZoneChange;
  end;
end;

procedure TDAOCConnection.AdjustObjDestForZone(AObj: TDAOCMovingObject;
  AZoneNum: integer);
var
  pZoneBase:  TDAOCZoneInfo;
begin
  if (AObj.DestinationX = 0) and (AObj.DestinationY = 0) then
    exit;

  if Assigned(FZone) and (FZone.ZoneNum = AZoneNum) then
    pZoneBase := FZone
  else
    pZoneBase := FZoneList.FindZone(AZoneNum);

  if Assigned(pZoneBase) then begin
    AObj.DestinationX := pZoneBase.ZoneToWorldX(AObj.DestinationX);
    AObj.DestinationY := pZoneBase.ZoneToWorldY(AObj.DestinationY);
  end;
end;

end.


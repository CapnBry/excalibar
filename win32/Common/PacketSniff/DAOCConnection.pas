unit DAOCConnection;

interface

uses
  Windows, SysUtils, Classes, Dialogs, ExtCtrls, Contnrs, WinSock,
{$IFDEF DAOC_AUTO_SERVER}
  ComObj,
{$ENDIF}
  DAOCPackets, DAOCObjs, DAOCRegion, DAOCInventory, DAOCPlayerAttributes, StringParseHlprs,
  VendorItems, ChatParse, MapNavigator, PReader2, FrameFns, INIFiles;

type
  TStringEvent = procedure (Sender: TObject; const AMsg: string) of Object;
  TIntegerEvent = procedure (Sender: TObject; AVal: integer) of Object;
  TSheduledCallback = procedure (Sender: TObject; AParm: LPARAM) of Object;
  TChatMessageEvent = procedure (Sender: TObject; const AWho, AMsg: string) of Object;
  TVersionEvent = procedure (Sender: TObject; AMajor, AMinor, ARelease: BYTE) of Object;

  TAccountCharInfo = class(TObject)
  private
    FName:    string;
    FRegionID:integer;
    FLevel:   integer;
    FRealm:   TDAOCRealm;
  public
    function AsString : string;

    property Name: string read FName;
    property RegionID: integer read FRegionID;
    property Level: integer read FLevel;
    property Realm: TDAOCRealm read FRealm;
  end;

  TAccountCharInfoList = class(TObjectList)
  private
    FAccountName:   string;
    function GetItems(iIndex: integer): TAccountCharInfo;
  public
    function FindOrAddChar(const AName: string) : TAccountCharInfo;

    property Items[iIndex: integer]: TAccountCharInfo read GetItems; default;
    property AccountName: string read FAccountName;
  end;

  PCallbackEventInfo = ^TCallbackEventInfo;
  TCallbackEventInfo = record
    dwTime:   DWORD;
    fn:       TSheduledCallback;
    parm:     LPARAM;
  end;

{$IFDEF DAOC_AUTO_SERVER}
  TDAOCConnection = class(TAutoObject)
{$ELSE}
  TDAOCConnection = class(TObject)
{$ENDIF}
  private
    FClientAddr: DWORD;
    FServerAddr: DWORD;
    FUDPServerAddr: DWORD;
    FActive:  boolean;
    FVersionMajor: byte;
    FVersionMinor: byte;
    FVersionRelease: byte;
    FCryptKey:  TDAOCCryptKey;
    FCryptKeySet: boolean;
    FLargestDAOCPacketSeen: DWORD;
    FAccountCharacters:  TAccountCharInfoList;
    FServerProtocol:  byte;

    FTCPFromClient:   TDAOCTCPPacketAssembler;
    FTCPFromServer:   TDAOCTCPPacketAssembler;

    FSelectedID:    WORD;
    FTradeCommissionItem: string;
    FTradeCommissionNPC: string;
    FScheduledCallbacks:  TList;
    FMaxObjectDistSqr:    double;
    FMaxObjectUpdtDistSqr: double;
    FPingRequestSentTime: DWORD;
    FLastPingTime:      integer;

    FOnPlayerPosUpdate: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FOnLog:  TStringEvent;
    FOnVendorWindow: TNotifyEvent;
    FOnNewDAOCObject: TDAOCObjectNotify;
    FOnZoneChange: TNotifyEvent;
    FOnCharacterLogin: TNotifyEvent;
    FOnPacket: TPacketEvent;
    FOnInventoryChanged: TNotifyEvent;
    FOnSkillLevelChanged: TNameValueModifiedNotify;
    FOnAfterPacket: TPacketEvent;
    FOnMoneyChanged: TNotifyEvent;
    FOnDeleteDAOCObject: TDAOCObjectNotify;
    FOnSelectedObjectChange: TDAOCObjectNotify;
    FOnTradeSkillSuccess: TIntegerEvent;
    FOnTradeSkillFailure: TNotifyEvent;
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

    function GetClientIP: string;
    function GetServerIP: string;
    procedure SetClientIP(const Value: string);
    procedure SetServerIP(const Value: string);
    procedure ClearCallbackList;
    procedure CheckScheduledTimeoutCallback;
    function GetCryptKey: string;
    procedure SetCryptKey(const Value: string);
    function GetSelectedObject: TDAOCObject;
    function GetUDPServerIP: string;
    procedure SetSelectedObject(const Value: TDAOCObject);
    procedure ClearDAOCObjectList;
    procedure CheckObjectsOutOfRange;
    procedure SetMaxObjectDistance(const Value: double);
    procedure SetMaxObjectUpdtDistance(const Value: double);
    function CheckZoneChanged : boolean;
    function SetActiveCharacterByName(const ACharacterName: string) : TAccountCharInfo;
  protected
    FChatParser:    TDAOCChatParser;
    FLocalPlayer:   TDAOCLocalPlayer;
    FZoneList:  TDAOCZoneInfoList;
    FZone:      TDAOCZoneInfo;
    FRegionID:  integer;
    FDAOCObjs:  TDAOCObjectList;
    FVendorItems: TDAOCVendorItemList;
    FMasterVendorList: TDAOCMasterVendorList;
    FGroundTarget: TMapNode;

    procedure CPARSETradeSkillSuccess(ASender: TDAOCChatParser; AQuality: integer);
    procedure CPARSETradeSkillFailure(ASender: TDAOCChatParser);
    procedure CPARSETradeSkillCapped(ASender: TDAOCChatParser);
    procedure CPARSECombatStyleSuccess(ASender: TDAOCChatParser);
    procedure CPARSECombatStyleFailure(ASender: TDAOCChatParser);

    procedure ParseSetEncryptionKey(pPacket: TDAOCPacket);
    procedure ParseSetPlayerRegion(pPacket: TDAOCPacket);
    procedure ParseAccountCharacters(pPacket: TDAOCPacket);
    procedure ParsePlayerDetails(pPacket: TDAOCPacket);
    procedure ParseLocalPosUpdateFromClient(pPacket: TDAOCPacket);
    procedure ParseInventoryList(pPacket: TDAOCPacket);
    procedure ParsePlayerStatsUpdate(pPacket: TDAOCPacket);
    procedure ParsePlayerSpecsSpellsAbils(pPacket: TDAOCPacket);
    procedure ParsePlayerSkills(pPacket: TDAOCPacket);
    procedure ParseLocalHeadUpdateFromClient(pPacket: TDAOCPacket);
    procedure ParsePlayerPosUpdate(pPacket: TDAOCPacket);
    procedure ParsePlayerHeadUpdate(pPacket: TDAOCPacket);
    procedure ParseMobUpdate(pPacket: TDAOCPacket);
    procedure ParseLogUpdate(pPacket: TDAOCPacket);
    procedure ParseLocalHealthUpdate(pPacket: TDAOCPacket);
    procedure ParseCharacterLoginInit(pPacket: TDAOCPacket);
    procedure ParseNewObject(pPacket: TDAOCPacket; AClass: TDAOCObjectClass);
    procedure ParseObjectEquipment(pPacket: TDAOCPacket);
    procedure ParseMoneyUpdate(pPacket: TDAOCPacket);
    procedure ParseRequestBuyItem(pPacket: TDAOCPacket);
    procedure ParseSelectedIDUpdate(pPacket: TDAOCPacket);
    procedure ParseProgressMeter(pPacket: TDAOCPacket);
    procedure ParseSpellPulse(pPacket: TDAOCPacket);
    procedure ParsePopupMessage(pPacket: TDAOCPacket);
    procedure ParseCommandFromClient(pPacket: TDAOCPacket);
    procedure ParseVendorWindow(pPacket: TDAOCPacket);
    procedure ParseRegionServerInfomation(pPacket: TDAOCPacket);
    procedure ParseDeleteObject(pPacket: TDAOCPacket);
    procedure ParseSetGroundTarget(pPacket: TDAOCPacket);
    procedure ParseCharacterStealthed(pPacket: TDAOCPacket);
    procedure ParseCharacterActivationRequest(pPacket: TDAOCPacket);
    procedure ParseServerProcotolInit(pPacket: TDAOCPacket);
    procedure ParseRequestPlayerByPlayerID(pPacket: TDAOCPacket);
    procedure ParseRequestObjectByInfoID(pPacket: TDAOCPacket);
    procedure ParsePlayerCenteredSpellEffect(pPacket: TDAOCPacket);
    procedure ParseServerPingResponse(pPacket: TDAOCPacket);
    procedure ParseServerPingRequest(pPacket: TDAOCPacket);
    procedure ParseGroupMembersUpdate(pPacket: TDAOCPacket);
    procedure ParseGroupWindowUpdate(pPacket: TDAOCPacket);
    procedure ParseAggroIndicator(pPacket: TDAOCPacket);

    procedure ProcessDAOCPacketFromServer(pPacket: TDAOCPacket);
    procedure ProcessDAOCPacketFromClient(pPacket: TDAOCPacket);
    procedure ProcessDAOCPacket(pPacket: TDAOCPacket);
    procedure IntializeFromSegment(AServerSegment: TEthernetSegment);
    procedure ProcessUDPSegment(ASegment: TEthernetSegment);
    procedure ProcessTCPSegment(ASegment: TEthernetSegment);

    procedure DoOnChatSayIncoming(const AWho, AMessage: string); virtual;
    procedure DoOnChatSayOutgoing(const AMessage: string); virtual;
    procedure DoOnChatSendIncoming(const AWho, AMessage: string); virtual;
    procedure DoOnChatSendOutgoing(const AMessage: string); virtual;
    procedure DoOnChatBroadcast(const AMessage: string); virtual;
    procedure DoOnSetGroundTarget; virtual;
    procedure DoOnTradeSkillSuccess(AQuality: integer); virtual;
    procedure DoOnTradeSkillFailure; virtual;
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
    procedure DoOnMoneyChanged; virtual;
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

    procedure ChatSay(const ALine: string);
    procedure ChatSend(const ALine: string);
    procedure HookChatParseCallbacks;
    procedure MergeVendorItemsToMaster;
    procedure ScheduleCallback(ATimeout: DWORD; ACallback: TSheduledCallback; AParm: LPARAM);
    procedure UpdatePlayersInGuild;
    procedure ResetPlayersInGroup;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ProcessEthernetSegment(ASegment: TEthernetSegment);
    procedure Clear; virtual;
    procedure CheckForStaleObjects;
    procedure SaveConnectionState(const AFileName: string);
    procedure ResumeConnection(const AFileName: string);

      { Functions to use zone information to find relative player coords }
    function PlayerZoneHead : integer;
    function PlayerZoneX : DWORD;
    function PlayerZoneY : DWORD;
    function PlayerZoneZ : DWORD;

    property Active: boolean read FActive;
    property ClientAddr: DWORD read FClientAddr;
    property ClientIP: string read GetClientIP write SetClientIP;
    property ServerAddr: DWORD read FServerAddr;
    property ServerIP: string read GetServerIP write SetServerIP;
    property UDPServerAddr: DWORD read FUDPServerAddr;
    property UDPServerIP: string read GetUDPServerIP;

    property AccountCharacterList: TAccountCharInfoList read FAccountCharacters;
    property CryptKey: string read GetCryptKey write SetCryptKey;
    property DAOCObjects: TDAOCObjectList read FDAOCObjs;
    property GroundTarget: TMapNode read FGroundTarget;
    property LargestDAOCPacketSeen: DWORD read FLargestDAOCPacketSeen;
    property MaxObjectDistance: double write SetMaxObjectDistance;
    property MaxObjectUpdtDistance: double write SetMaxObjectUpdtDistance; 
    property MasterVendorList: TDAOCMasterVendorList read FMasterVendorList;
    property LastPingTime: integer read FLastPingTime;
    property LocalPlayer: TDAOCLocalPlayer read FLocalPlayer;
    property RegionID: integer read FRegionID write FRegionID;
    property SelectedID: WORD read FSelectedID;
    property ServerProtocol: BYTE read FServerProtocol write FServerProtocol;
    property SelectedObject: TDAOCObject read GetSelectedObject write SetSelectedObject;
    property TradeCommissionNPC: string read FTradeCommissionNPC;
    property TradeCommissionItem: string read FTradeCommissionItem;
    property VendorItems: TDAOCVendorItemList read FVendorItems;
    property Zone: TDAOCZoneInfo read FZone;
    property ZoneList: TDAOCZoneInfoList read FZoneList;

    property OnPacket: TPacketEvent read FOnPacket write FOnPacket;
    property OnAfterPacket: TPacketEvent read FOnAfterPacket write FOnAfterPacket;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnPlayerPosUpdate: TNotifyEvent read FOnPlayerPosUpdate write FOnPlayerPosUpdate;
    property OnLog: TStringEvent read FOnLog write FOnLog;
    property OnZoneChange: TNotifyEvent read FOnZoneChange write FOnZoneChange;
    property OnCharacterLogin: TNotifyEvent read FOnCharacterLogin write FOnCharacterLogin;
    property OnInventoryChanged: TNotifyEvent read FOnInventoryChanged write FOnInventoryChanged;
    property OnSkillLevelChanged: TNameValueModifiedNotify read FOnSkillLevelChanged write FOnSkillLevelChanged;
    property OnMoneyChanged: TNotifyEvent read FOnMoneyChanged write FOnMoneyChanged;
    property OnVendorWindow: TNotifyEvent read FOnVendorWindow write FOnVendorWindow;
    property OnNewDAOCObject: TDAOCObjectNotify read FOnNewDAOCObject write FOnNewDAOCObject;
    property OnDeleteDAOCObject: TDAOCObjectNotify read FOnDeleteDAOCObject write FOnDeleteDAOCObject;
    property OnDAOCObjectMoved: TDAOCObjectNotify read FOnDAOCObjectMoved write FOnDAOCObjectMoved;
    property OnRegionChanged: TNotifyEvent read FOnRegionChanged write FOnRegionChanged;
    property OnSelectedObjectChange: TDAOCObjectNotify read FOnSelectedObjectChange write FOnSelectedObjectChange;
    property OnSetGroundTarget: TNotifyEvent read FOnSetGroundTarget write FOnSetGroundTarget;
    property OnTradeSkillSuccess: TIntegerEvent read FOnTradeSkillSuccess write FOnTradeSkillSuccess;
    property OnTradeSkillFailure: TNotifyEvent read FOnTradeSkillFailure write FOnTradeSkillFailure;
    property OnTradeSkillCapped: TNotifyEvent read FOnTradeSkillCapped write FOnTradeSkillCapped;
    property OnChatLog: TStringEvent read FOnChatLog write FOnChatLog;
    property OnChatSendIncoming: TChatMessageEvent read FOnChatSendIncoming write FOnChatSendIncoming;
    property OnChatSendOutgoing: TStringEvent read FOnChatSendOutgoing write FOnChatSendOutgoing;
    property OnChatSayIncoming: TChatMessageEvent read FOnChatSayIncoming write FOnChatSayIncoming;
    property OnChatSayOutgoing: TStringEvent read FOnChatSayOutgoing write FOnChatSayOutgoing;
    property OnChatBroadcast: TStringEvent read FOnChatBroadcast write FOnChatBroadcast;
    property OnCombatStyleSuccess: TStringEvent read FOnCombatStyleSuccess write FOnCombatStyleSuccess;
    property OnCombatStyleFailure: TNotifyEvent read FOnCombatStyleFailure write FOnCombatStyleFailure;
    property OnVersionNumsSet: TVersionEvent read FOnVersionNumsSet write FOnVersionNumsSet;
    property OnPingReply: TIntegerEvent read FOnPingReply write FOnPingReply;
    property OnMobTargetChanged: TDAOCMobNotify read FOnMobTargetChanged write FOnMobTargetChanged;
  end;


implementation

{$IFDEF GLOBAL_TICK_COUNTER}
uses
  GlobalTickCounter;
{$ENDIF GLOBAL_TICK_COUNTER}

{ TAccountCharInfoList }

function TAccountCharInfoList.FindOrAddChar(const AName: string): TAccountCharInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].Name, AName) then begin
      Result := Items[I];
      exit;
    end;

  Result := TAccountCharInfo.Create;
  Result.FName := AName;
  Add(Result);
end;

function TAccountCharInfoList.GetItems(iIndex: integer): TAccountCharInfo;
begin
  Result := TAccountCharInfo(inherited Items[iIndex]);
end;

{ TAccountCharInfo }

function TAccountCharInfo.AsString: string;
begin
  Result := Format('%s level %d %s region %d', [
    FName, FLevel,  RealmToStr(FRealm), FRegionID]);
end;

{ TDAOCConnection }

procedure TDAOCConnection.Clear;
begin
  FActive := false;
  FillChar(FCryptKey, sizeof(FCryptKey), 0);
  FCryptKeySet := false;
  FMasterVendorList.Clear;
  FAccountCharacters.Clear;
  ClearDAOCObjectList;
  FVendorItems.Clear;
  FLocalPlayer.Clear;
  FTradeCommissionNPC := '';
  FTradeCommissionItem := '';
  ClearCallbackList;
  FRegionID := 0;
  FSelectedID := 0;
  FChatParser.Reset;
  FLargestDAOCPacketSeen := 0;
  FGroundTarget.Clear;
end;

constructor TDAOCConnection.Create;
begin
  inherited Create;

  FLocalPlayer := TDAOCLocalPlayer.Create;
  FGroundTarget := TMapNode.Create;
  FZoneList := TDAOCZoneInfoList.Create;
  FZoneList.LoadFromFile('mapinfo.txt');

  FTCPFromClient := TDAOCTCPPacketAssembler.Create(true);
  FTCPFromServer := TDAOCTCPPacketAssembler.Create(false);

  FTCPFromClient.OtherSide := FTCPFromServer;
  FTCPFromServer.OtherSide := FTCPFromClient;

  FAccountCharacters := TAccountCharInfoList.Create;
  FScheduledCallbacks := TList.Create;
  FVendorItems := TDAOCVendorItemList.Create;
  FMasterVendorList := TDAOCMasterVendorList.Create;
  FDAOCObjs := TDAOCObjectList.Create;
  FChatParser := TDAOCChatParser.Create;
  HookChatParseCallbacks;

  SetMaxObjectUpdtDistance(6000);
  SetMaxObjectDistance(8000);
end;

destructor TDAOCConnection.Destroy;
begin
  ClearCallbackList;
  FScheduledCallbacks.Free;
  FAccountCharacters.Free;
  FTCPFromClient.Free;
  FTCPFromServer.Free;

  FChatParser.Free;
  FDAOCObjs.Free;
  FMasterVendorList.Free;
  FVendorItems.Free;
  FZoneList.Free;
  FGroundTarget.Free;
  FLocalPlayer.Free;

  inherited Destroy;
end;

procedure TDAOCConnection.DoOnCharacterLogin;
begin
  if Assigned(FOnCharacterLogin) then
    FOnCharacterLogin(Self);
end;

procedure TDAOCConnection.DoOnConnect;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TDAOCConnection.DoOnDisconnect;
begin
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

procedure TDAOCConnection.IntializeFromSegment(AServerSegment: TEthernetSegment);
(*** Set everything up from a ethernet segment from the server (the SYN ACK frame) ***)
begin
  Clear;
  FClientAddr := AServerSegment.AsIP^.DestAddr;
  FServerAddr := AServerSegment.AsIP^.SrcAddr;
  FActive := true;

  FCryptKeySet := false;

  FTCPFromClient.Clear;
  FTCPFromClient.NextExpectedSeq := ntohl(AServerSegment.AsTCP^.AckNumber);
  FTCPFromServer.Clear;
  FTCPFromServer.NextExpectedSeq := ntohl(AServerSegment.AsTCP^.SeqNumber) + 1;
end;

procedure TDAOCConnection.Log(const s: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, s);
end;

procedure TDAOCConnection.ParseAccountCharacters(pPacket: TDAOCPacket);
var
  sName:  string;
  iCharsLeft: integer;
  iRegion:  integer;
  iRealm:   integer;
  iLevel:   integer;
  pAcctChar: TAccountCharInfo;
begin
  pPacket.HandlerName := 'AccountCharacters';
  FAccountCharacters.FAccountName := pPacket.getNullTermString(24);

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
      pAcctChar.FRegionID := iRegion;
      pAcctChar.FRealm := TDAOCRealm(iRealm);
      pAcctChar.FLevel := iLevel;
    end;
    dec(iCharsLeft);
  end;    { for chars }
end;

procedure TDAOCConnection.ParsePlayerDetails(pPacket: TDAOCPacket);
var
  SubType: BYTE;
  iLevel:   integer;
  sName:    string;
  pAcctChar:  TAccountCharInfo;
begin
  pPacket.HandlerName := 'PlayerDetails';
  pPacket.Seek(1);  // count of items
  SubType := pPacket.getByte;
  if SubType = 0 then begin
    pPacket.Seek(1);
    iLevel := pPacket.getByte;
    sName := pPacket.getPascalString;

    pAcctChar := SetActiveCharacterByName(sName);
    pAcctChar.FLevel := iLevel;

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

procedure TDAOCConnection.ParseInventoryList(pPacket: TDAOCPacket);
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
    pTmpItem.ItemIDMajor := pPacket.getByte;
    pTmpItem.ItemIDMinor := pPacket.getByte;
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

procedure TDAOCConnection.ParseLocalPosUpdateFromClient(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'LocalPosUpdateFromClient';
  pPacket.seek(2);
  FLocalPlayer.SpeedWord := pPacket.getShort;
  FLocalPlayer.Z := pPacket.getShort;
  pPacket.seek(2);
  FLocalPlayer.X := pPacket.getLong;
  FLocalPlayer.Y := pPacket.getLong;
  FLocalPlayer.HeadWord := pPacket.getShort;

  CheckZoneChanged;

  DoOnPlayerPosUpdate;
  CheckObjectsOutOfRange;
end;

procedure TDAOCConnection.ParsePlayerStatsUpdate(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseSetEncryptionKey(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'SetEncryptionKey';

    { version w x.yz }
  pPacket.seek(1);
  FServerProtocol := pPacket.getByte; // actually encryption scheme?
  FVersionMajor := pPacket.getByte;  // bigver  (x)
  FVersionMinor := pPacket.getByte;  // minver  (y)
  FVersionRelease := pPacket.getByte;// release (z)

  pPacket.getBytes(FCryptKey, 12);
  FCryptKeySet := true;

  DoVersionNumsSet;
end;

procedure TDAOCConnection.ParseSetPlayerRegion(pPacket: TDAOCPacket);
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

function TDAOCConnection.PlayerZoneX: DWORD;
begin
  Result := FLocalPlayer.X;
  if Assigned(FZone) then
    Result := FZone.WorldToZoneX(Result);
end;

function TDAOCConnection.PlayerZoneY: DWORD;
begin
  Result := FLocalPlayer.Y;
  if Assigned(FZone) then
    Result := FZone.WorldToZoneY(Result);
end;

function TDAOCConnection.PlayerZoneZ: DWORD;
begin
  Result := FLocalPlayer.Z;
end;

procedure TDAOCConnection.ProcessDAOCPacketFromClient(
  pPacket: TDAOCPacket);
var
  command:  WORD;
begin
  // seq := pPacket.getShort;
  // srcid := pPacket.getShort;
  // pPacket.seek(2);
  pPacket.seek(6);
  command := pPacket.getShort;

//  Writeln(Format('seq 0x%4.4x  src 0x%4.4x  cmd 0x%4.4x  dst 0x%4.4x',
//    [seq, srcid, command, destid]));

  case command of
    $01:  ParseLocalPosUpdateFromClient(pPacket);
    $07:  ParseCommandFromClient(pPacket);
    $0b:  ParseServerPingRequest(pPacket);
    $12:  ParseLocalHeadUpdateFromClient(pPacket);
    $16:  ParseRequestObjectByInfoID(pPacket);
    $18:  ParseSelectedIDUpdate(pPacket);
    $44:  ParseSetGroundTarget(pPacket);
    $7d:  ParseRequestPlayerByPlayerID(pPacket);
    $b8:  ParseCharacterActivationRequest(pPacket);
    $d0:  ParseRequestBuyItem(pPacket);
  end;
end;

procedure TDAOCConnection.ProcessDAOCPacketFromServer(pPacket: TDAOCPacket);
var
  command:  BYTE;
begin
  if pPacket.IPProtocol = daocpUDP then
    pPacket.seek(2);  // seqno

  command := pPacket.getByte;

  case command of
    $01:  ParsePlayerPosUpdate(pPacket);
    $05:  ParseLocalHealthUpdate(pPacket);
    $07:  ParseLogUpdate(pPacket);
    $09:  ParseMobUpdate(pPacket);
    $0a:  ParseDeleteObject(pPacket);
    $12:  ParsePlayerHeadUpdate(pPacket);
    $14:  ParseAggroIndicator(pPacket);
    $1f:  ParseSetPlayerRegion(pPacket);
    $29:  ParsePopupMessage(pPacket);
    $36:  ParseRegionServerInfomation(pPacket);
    $49:  ParseCharacterStealthed(pPacket);
    $52:  ParseMoneyUpdate(pPacket);
    $55:  ParseAccountCharacters(pPacket);
    $5b:  ParseProgressMeter(pPacket);
    $71:  ParseNewObject(pPacket, ocObject);
    $72:  ParseNewObject(pPacket, ocMob);
    $7c:  ParseNewObject(pPacket, ocPlayer);
    $81:  ParseServerPingResponse(pPacket);
    $82:  ParseServerProcotolInit(pPacket);
    $8a:  ParseSetEncryptionKey(pPacket);
    $88:  ParseCharacterLoginInit(pPacket);
    $aa:  ParseInventoryList(pPacket);
    $b3:  ParsePlayerCenteredSpellEffect(pPacket);
    $bd:  ParseObjectEquipment(pPacket);
    $be:  ParsePlayerStatsUpdate(pPacket);
    $bf:  ParseVendorWindow(pPacket);
    $d7:  ParseSpellPulse(pPacket);
    $d8:  ParseGroupWindowUpdate(pPacket);
  end;
end;

procedure TDAOCConnection.ProcessEthernetSegment(ASegment: TEthernetSegment);
begin
  if (ASegment.AsIP^.Protocol = SOL_UDP) and FActive then
    ProcessUDPSegment(ASegment)

  else if ASegment.AsIP^.Protocol = SOL_TCP then
      { see if this is a new connection, SYN / ACK will come from server }
    if IsSyn(ASegment.AsTCP) and IsAck(ASegment.AsTCP) then begin
      IntializeFromSegment(ASegment);
      DoOnConnect;
    end

    else if IsFin(ASegment.AsTCP) and
      (FClientAddr = ASegment.AsIP^.SrcAddr) then begin
      FActive := false;
      DoOnDisconnect;
    end

    else if FActive then
      ProcessTCPSegment(ASegment);

  CheckScheduledTimeoutCallback;
end;

procedure TDAOCConnection.ProcessTCPSegment(ASegment: TEthernetSegment);
var
  pFrag:    TTCPFragment;
  pPacket:  TDAOCPacket;
  pAssembler: TDAOCTCPPacketAssembler;
begin
  if ASegment.AsIP^.Protocol <> SOL_TCP then
    exit;

  if FClientAddr = ASegment.AsIP^.SrcAddr then
    pAssembler := FTCPFromClient
  else if FServerAddr = ASegment.AsIP^.SrcAddr then
    pAssembler := FTCPFromServer
  else
    pAssembler := nil;

  if not Assigned(pAssembler) then
    exit;

  pFrag := TTCPFragment.CreateFrom(ASegment);

{$IFDEF GLOBAL_TICK_COUNTER}
  UpdateGlobalTickCount;
{$ENDIF GLOBAL_TICK_COUNTER}

  if pFrag.IsAck then begin
    while pAssembler.OtherSide.ParsePacket(pFrag.AckNo, pPacket) do begin
      if pPacket.Size > 0 then begin
        pPacket.IsFromClient := pAssembler.OtherSide.IsFromClient;
        pPacket.IPProtocol := daocpTCP;

        ProcessDAOCPacket(pPacket);
      end;  { if packet.size }

      pPacket.Free;
    end;  { while ParsePacket }
  end;  { if is Ack }

  pAssembler.AddFragment(pFrag);
end;

procedure TDAOCConnection.DoSetRegionID(ARegion: integer);
var
  pAcctChar:  TAccountCharInfo;
begin
  if FRegionID = ARegion then
    exit;
    
  FGroundTarget.Clear;

  FRegionID := ARegion;

  if FLocalPlayer.Name <> '' then begin
    pAcctChar := FAccountCharacters.FindOrAddChar(FLocalPlayer.Name);
    if Assigned(pAcctChar) then
      pAcctChar.FRegionID := FRegionID;
  end;

  FMasterVendorList.Clear;
  ClearDAOCObjectList;

  if Assigned(FOnRegionChanged) then
    FOnRegionChanged(Self);
  // Log('Player region changed to ' + IntToStr(FRegionID));
end;

procedure TDAOCConnection.ParsePlayerSpecsSpellsAbils(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParsePlayerSkills(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseLocalHeadUpdateFromClient(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'LocalHeadUpdateFromClient';
  pPacket.seek(2);
  FLocalPlayer.HeadWord := pPacket.getShort;
  DoOnPlayerPosUpdate;
end;

procedure TDAOCConnection.ParsePlayerPosUpdate(pPacket: TDAOCPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
begin
  pPacket.HandlerName := 'PlayerPosUpdate';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByPlayerID(wID);

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
      SpeedWord := pPacket.getShort;  //+2
      Z := pPacket.getShort;  //+4
      pPacket.seek(2);  //+6
      X := pPacket.getLong;  //+8
      Y := pPacket.getLong;  //+c
      HeadWord := pPacket.getShort;  //+10
      pPacket.seek(2);  //+12 word and 0xfff
      pDAOCObject.Stealthed := (pPacket.getByte and $02) <> 0;  // stealthed but visible
      HitPoints := pPacket.getByte; //+15

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

procedure TDAOCConnection.ParseMobUpdate(pPacket: TDAOCPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
  iIDOffset:  integer;
  iZoneBase:  integer;
  pZoneBase:  TDAOCZoneInfo;
  V162OrGreater:  boolean;
  bAddedObject:   boolean;
  procedure SetZoneBase;
  begin
    if Assigned(pZoneBase) and (pZoneBase.ZoneNum = iZoneBase) then
      exit
    else if Assigned(FZone) and (FZone.ZoneNum = iZoneBase) then
      pZoneBase := FZone
    else
      pZoneBase := FZoneList.FindZone(iZoneBase);
  end;
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

    { here we can add the object, even though we don't know what it is.
      The client will request the object via RequestObjectByInfoID
      and we'll just replace this object with the real mob.  Make sure
      we fire the event for the add though! }
  if not Assigned(pDAOCObject) then begin
    pDAOCObject := TDAOCUnknownMovingObject.Create;
    pDAOCObject.InfoID := wID;
    pDAOCObject.PlayerID := wID;
    FDAOCObjs.AddOrReplace(pDAOCObject);
    bAddedObject := true;
  end
  else
    bAddedObject := false;

  if Assigned(pDAOCObject) and (pDAOCObject is TDAOCMovingObject) then begin
    pPacket.seek(-(iIDOffset + 2));
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
        pPacket.seek(2);
        HitPoints := pPacket.getByte;
        pPacket.seek(1);

        pZoneBase := nil;

          { adjust X, Y to global coords }
        iZoneBase := pPacket.getByte;
        SetZoneBase;
        if Assigned(pZoneBase) then begin
          X := pZoneBase.ZoneToWorldX(X);
          Y := pZoneBase.ZoneToWorldY(Y);
        end;

          { adjust DestX, DestY to global coords }
        if (DestinationX <> 0) and (DestinationY <> 0) then begin
          iZoneBase := pPacket.getByte;
          SetZoneBase;
          if Assigned(pZoneBase) then begin
            DestinationX := pZoneBase.ZoneToWorldX(DestinationX);
            DestinationY := pZoneBase.ZoneToWorldY(DestinationY);
          end;
        end;  { if dest <> 0 }
      end  { protocol }
      else begin
        X := pPacket.getLong;  //+0
        Y := pPacket.getLong;  //+4
        DestinationX := pPacket.getLong;  //+8
        DestinationY := pPacket.getLong;  //+c
        z := pPacket.getShort;  //+10
        pPacket.seek(2);  // ID again //+12
        pPacket.seek(2);  //+14
        HitPoints := pPacket.getByte; //+15
      end;
    end;  { with TDAOCMovingObject(pDAOCObject) }

    if bAddedObject then
      DoOnNewDAOCObject(pDAOCObject)
    else
      DoOnDAOCObjectMoved(pDAOCObject);
  end  { if Assigned pDAOCObject }

  else begin
    pDAOCObject := FDAOCObjs.FindByPlayerID(wID);
    if Assigned(pDAOCObject) then
      Log('MobUpdate: MOB by InfoID 0x' + IntToHex(wID, 4) + ' is in playerid list, not infoid list.  Type: ' + DAOCObjectClassToStr(pDAOCObject.ObjectClass))
    else
      Log('MobUpdate: Can not find MOB by InfoID 0x' + IntToHex(wID, 4));
  end;
end;

procedure TDAOCConnection.ParsePlayerHeadUpdate(pPacket: TDAOCPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
begin
  pPacket.HandlerName := 'PlayerHeadUpdate';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByPlayerID(wID);

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

procedure TDAOCConnection.ParseLogUpdate(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseLocalHealthUpdate(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'LocalHealthUpdate NOTIMPL';
end;

procedure TDAOCConnection.ParseCharacterLoginInit(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseNewObject(pPacket: TDAOCPacket;
  AClass: TDAOCObjectClass);
var
  tmpObject:  TDAOCObject;
  pOldObject: TDAOCObject;
begin
  pPacket.HandlerName := 'NewObject (' + DAOCObjectClassToStr(AClass) + ')';
  pOldObject := nil;

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
          Level := pPacket.getByte; //+15
          pPacket.getByte;  //+16  bittest 0, 1, 2, 4, 8, c0
          pPacket.getByte;  //+17  shl 2
          Name := pPacket.getPascalString;//+18
          TypeTag := pPacket.getPascalString;
            { the destination is usually set in the update packet before the client
              requests the NewObject.  This may not be a good idea, but it is the
              easy fix }
          pOldObject := FDAOCObjs.FindByInfoID(tmpObject.InfoID);
          if Assigned(pOldObject) and not pOldObject.Stale and
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

          IsInGuild := AnsiSameText(Guild, FLocalPlayer.Guild);
        end;  { with TDAOCPlayer }
      end;  { ocPlayer }

    else
      tmpObject := nil;
  end;  { case AClass }

  if Assigned(tmpObject) then begin
    if not Assigned(pOldObject) then
      pOldObject := FDAOCObjs.FindByInfoID(tmpObject.InfoID);
    if Assigned(pOldObject) then
      DoOnDeleteDAOCObject(pOldObject);
    FDAOCObjs.AddOrReplace(tmpObject);
    DoOnNewDAOCObject(tmpObject);
  end;
end;

procedure TDAOCConnection.ParseObjectEquipment(pPacket: TDAOCPacket);
var
  ID:   integer;
  iCnt: integer;
  bSlot:    byte;
  obj_major:   byte;
  obj_minor:  byte;
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
    obj_major := 0;
    obj_minor := 0;
    obj_color := 0;
    bSlot := pPacket.getByte;

    case bSlot of
      $0a..$0d,  // L, R, 2h, Ranged, Thrown
      $15..$17,  // head, hands, feet
      $19..$1c:  // chest, cloak, legs, sleeves
        begin
          obj_major := pPacket.getByte;    // which object list the index is for
          obj_minor := pPacket.getByte;   // index in the object list
          if (obj_major and $40) <> 0 then
            obj_color := pPacket.getByte;
          if (obj_major and $20) <> 0 then
            pPacket.getShort;  // particle effect?
          if (obj_major and $80) <> 0 then
            pPacket.getShort;  // guild emblem?
        end;  { if in a real slot }
    end;  { case bSlot }

    tmpInvItem := TDAOCInventoryItem.Create;
    tmpInvItem.Slot := bSlot;
    tmpInvItem.ItemIDMajor := obj_major and $0f;
    tmpInvItem.ItemIDMinor := obj_minor;
    tmpInvItem.Color := obj_color;
    TDAOCMovingObject(pMob).Inventory.TakeItem(tmpInvItem);

    dec(iCnt);
  end;  { while cnt and !EOF }

  TDAOCMovingObject(pMob).InventoryChanged;
end;

procedure TDAOCConnection.ParseMoneyUpdate(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'MoneyUpdate';
  with FLocalPlayer.Currency do begin
    Copper := pPacket.getByte;
    Silver := pPacket.getByte;
    Gold := pPacket.getShort;
    Mithril := pPacket.getShort;
    Platinum := pPacket.getShort;
  end;  { with FLocalPlayer.Currency }

  DoOnMoneyChanged;
end;

procedure TDAOCConnection.DoOnMoneyChanged;
begin
  if Assigned(FOnMoneyChanged) then
    FOnMoneyChanged(Self);
end;

procedure TDAOCConnection.ParseRequestBuyItem(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'RequestBuyItem NOTIMPL';
end;

procedure TDAOCConnection.ParseSelectedIDUpdate(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'SelectedIDUpdate';
  FSelectedID := pPacket.getShort;

  DoOnSelectedObjectChanged(SelectedObject);
end;

procedure TDAOCConnection.ParseProgressMeter(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseSpellPulse(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'SpellPulse NOTIMPL';
end;

procedure TDAOCConnection.ParsePopupMessage(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ParseCommandFromClient(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ScheduleCallback(ATimeout: DWORD;
  ACallback: TSheduledCallback; AParm: LPARAM);
var
  pInfo:  PCallbackEventInfo;
begin
  New(pInfo);
  FScheduledCallbacks.Add(pInfo);
  pInfo^.dwTime := GetTickCount + ATimeout;
  pInfo^.fn := ACallback;
  pInfo^.parm := AParm;
end;

procedure TDAOCConnection.CheckScheduledTimeoutCallback;
var
  I:      integer;
  dwTime: DWORD;
  pItem:  PCallbackEventInfo;
begin
  I := 0;
  dwTime := GetTickCount;
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

function TDAOCConnection.GetCryptKey: string;
begin
  Result := DAOCCryptKeyToString(FCryptKey);
end;

procedure TDAOCConnection.SetCryptKey(const Value: string);
begin
  if Value = '' then begin
    FCryptKeySet := false;
    exit;
  end;

  StringToDAOCCryptKey(Value, FCryptKey);
  FCryptKeySet := true;
end;

procedure TDAOCConnection.ParseVendorWindow(pPacket: TDAOCPacket);
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
  else if FSelectedID = FLocalPlayer.InfoID then
    Result := FLocalPlayer
  else
    Result := FDAOCObjs.FindByInfoID(FSelectedID);
end;

procedure TDAOCConnection.ParseRegionServerInfomation(
  pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'RegionServerInformation NOTIMPL';
end;

procedure TDAOCConnection.ParseDeleteObject(pPacket: TDAOCPacket);
var
  iPos: integer;
begin
  pPacket.HandlerName := 'DeleteObject';
  iPos := FDAOCObjs.IndexOfInfoID(pPacket.getShort);
  if iPos <> -1 then begin
    DoOnDeleteDAOCObject(FDAOCObjs[iPos]);
    FDAOCObjs.Delete(iPos);
  end;
end;

procedure TDAOCConnection.DoOnDeleteDAOCObject(AObject: TDAOCObject);
var
  I:    integer;
begin
    { make sure nobody still has this poor fella as a target }
  for I := 0 to FDAOCObjs.Count - 1 do
    if (FDAOCObjs[I] is TDAOCMovingObject) and
      (TDAOCMob(FDAOCObjs[I]).Target = AObject) then begin
      TDAOCMob(FDAOCObjs[I]).Target := nil;
      DoOnMobTargetChanged(TDAOCMob(FDAOCObjs[I]));
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

procedure TDAOCConnection.DoOnTradeskillSuccess(AQuality: integer);
begin
  if Assigned(FOnTradeskillSuccess) then
    FOnTradeskillSuccess(Self, AQuality);
end;

procedure TDAOCConnection.HookChatParseCallbacks;
begin
  FChatParser.OnTradeSkillSuccess := CPARSETradeSkillSuccess;
  FChatParser.OnTradeSkillFailure := CPARSETradeSkillFailure;
  FChatParser.OnTradeskillCapped := CPARSETradeSkillCapped;
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

procedure TDAOCConnection.ProcessUDPSegment(ASegment: TEthernetSegment);
var
  iDataLen:     integer;
  iPacketLen:   integer;
  wUDPDatagramLen:  WORD;
  bIsFromClient:    boolean;
  pPayloadDataPtr:  Pointer;
  pDAOCPacket:      TDAOCPacket;
begin
  if ASegment.AsIP^.Protocol <> SOL_UDP then
    exit;

  if FClientAddr = ASegment.AsIP^.SrcAddr then begin
    bIsFromClient := true;
    if FUDPServerAddr <> ASegment.AsIP^.DestAddr then begin
      Log('UDP Server changed to: ' + my_inet_ntoa(ASegment.AsIP^.DestAddr));
      FUDPServerAddr := ASegment.AsIP^.DestAddr;
    end;
  end
  else if FUDPServerAddr = ASegment.AsIP^.SrcAddr then
    bIsFromClient := false
  else
    exit;

    { total len is the size of the UDP header and data.
      Does not include IP and Ethernet headers }
  wUDPDatagramLen := ntohs(ASegment.AsUDP^.TotalLen);
  if ASegment.Size < (wUDPDatagramLen + sizeof(TIPHeader)) then begin
    Log('Short UDP packet discarded');
    exit;
  end;

    { DataLen is the (declared UDP total len) - (UDP Header len) }
  iDataLen := wUDPDatagramLen - (sizeof(TUDPHeader) - sizeof(TIPHeader));
  pPayloadDataPtr := Pointer(DWORD(ASegment.Data) + sizeof(TUDPHeader));

  while iDataLen > 2 do begin
      { first bytes are the DAOC Packet Len }
    iPacketLen := (PBYTEARRAY(pPayloadDataPtr)^[0] shl 8) + PBYTEARRAY(pPayloadDataPtr)^[1];
    if iPacketLen = 0 then
      exit;
      { which is understated by 3 }
    inc(iPacketLen, 3);

    dec(iDataLen, 2);
    inc(DWORD(pPayloadDataPtr), 2);

    if iDataLen < iPacketLen then begin
      Log('UDP packet too short to contain stated DAOC packet, discarded');
      exit;
    end;

    pDAOCPacket := TDAOCPacket.Create;
    pDAOCPacket.IsFromClient := bIsFromClient;
    pDAOCPacket.IPProtocol := daocpUDP;
    pDAOCPacket.CopyDataToPacket(pPayloadDataPtr, iPacketLen);

    ProcessDAOCPacket(pDAOCPacket);
    pDAOCPacket.Free;

    dec(iDataLen, iPacketLen);
    inc(DWORD(pPayloadDataPtr), iPacketLen);
  end;
end;

procedure TDAOCConnection.ProcessDAOCPacket(pPacket: TDAOCPacket);
begin
  if pPacket.Size > FLargestDAOCPacketSeen then
    FLargestDAOCPacketSeen := pPacket.Size;
    
  if FCryptKeySet then
      { decrypt all TCP packets and all UDP from server }
    if (pPacket.IPProtocol = daocpTCP) or pPacket.IsFromServer then
      pPacket.Decrypt(FCryptKey);

  if Assigned(FOnPacket) then
    FOnPacket(Self, pPacket);

  pPacket.HandlerName := '';

  if pPacket.IsFromClient then
    ProcessDAOCPacketFromClient(pPacket)
  else
    ProcessDAOCPacketFromServer(pPacket);

  if Assigned(FOnAfterPacket) then
    FOnAfterPacket(Self, pPacket);
end;

procedure TDAOCConnection.ParseSetGroundTarget(pPacket: TDAOCPacket);
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

function TDAOCConnection.GetUDPServerIP: string;
begin
  Result := my_inet_ntoa(FUDPServerAddr);
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

procedure TDAOCConnection.ParseCharacterStealthed(pPacket: TDAOCPacket);
var
  wID:    WORD;
  pDAOCObject:  TDAOCObject;
begin
  pPacket.HandlerName := 'CharacterStealthed';
  wID := pPacket.getShort;
  pDAOCObject := FDAOCObjs.FindByInfoID(wID);
  if Assigned(pDAOCObject) then
    pDAOCObject.Stealthed := true;
end;

procedure TDAOCConnection.ClearDAOCObjectList;
begin
    { delete all the daocobjects, back to front, calling the delete event handler }
  while FDAOCObjs.Count > 0 do begin
    DoOnDeleteDAOCObject(FDAOCObjs[FDAOCObjs.Count - 1]);
    FDAOCObjs.Delete(FDAOCObjs.Count - 1);
  end;
end;

procedure TDAOCConnection.CheckObjectsOutOfRange;
var
  I:      integer;
  fDist:  double;
  pObj:   TDAOCObject;
begin
  I := FDAOCObjs.Count - 1;
  while I >= 0 do begin
    pObj := FDAOCObjs[I];
    fDist := pObj.DistanceSqr3D(FLocalPlayer);
    if fDist > FMaxObjectDistSqr then begin
      DoOnDeleteDAOCObject(pObj);
      FDAOCObjs.Delete(I);
    end
    else if fDist > FMaxObjectUpdtDistSqr then
      pObj.IsInUpdateRange := false
    else
      pObj.IsInUpdateRange := true;

    dec(I);
  end;  { while I > 0 }
end;

procedure TDAOCConnection.SetMaxObjectDistance(const Value: double);
begin
  FMaxObjectDistSqr := Value * Value;
end;

procedure TDAOCConnection.DoVersionNumsSet;
begin
  if Assigned(FOnVersionNumsSet) then
    FOnVersionNumsSet(Self, FVersionMajor, FVersionMinor, FVersionRelease);
end;

procedure TDAOCConnection.ParseCharacterActivationRequest(
  pPacket: TDAOCPacket);
var
  sCharName:  string;
begin
  pPacket.HandlerName := 'CharacterActivationRequest';
  pPacket.seek(4);
  sCharName := pPacket.getNullTermString(28);
  SetActiveCharacterByName(sCharName);
  { account name follows as an NTS, but who cares }
end;

function TDAOCConnection.SetActiveCharacterByName(const ACharacterName: string) : TAccountCharInfo;
begin
  Result := FAccountCharacters.FindOrAddChar(ACharacterName);

  FLocalPlayer.Level := Result.Level;
  FLocalPlayer.Realm := Result.Realm;
  FLocalPlayer.Name := Result.Name;
  DoSetRegionID(Result.RegionID);
end;

procedure TDAOCConnection.ParseServerProcotolInit(pPacket: TDAOCPacket);
begin
  FServerProtocol := pPacket.GetByte;
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
  pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'RequestPlayerByPlayerID';
  // pPacket.getShort;  // PlayerID
end;

procedure TDAOCConnection.ParseRequestObjectByInfoID(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'RequestObjectByInfoID';
end;

procedure TDAOCConnection.CheckForStaleObjects;
var
  I:    integer;
begin
  for I := 0 to FDAOCObjs.Count - 1 do
    FDAOCObjs[I].CheckStale;
end;

procedure TDAOCConnection.SetMaxObjectUpdtDistance(const Value: double);
begin
  FMaxObjectUpdtDistSqr := Value * Value;
end;

procedure TDAOCConnection.ParsePlayerCenteredSpellEffect(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'PlayerCenteredSpellEffect NOTIMPL';
  { I think this is a "Player-Centered" spell effect.  Like a spell effect
    that will follow a player as they move, like a buff or run chant effect
    B3 18 32 0E 1C 18 32 00 - 00 01 01 00 00           ..2...2......
  }
  // ID := pPacket.getShort;
end;

procedure TDAOCConnection.ParseServerPingResponse(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'ServerPingResponse';
  { the server sends us back a copy of the timer we sent }
  // FPingRequestNumber = pPacket.getLong;

  FLastPingTime := GlobalTickCount - FPingRequestSentTime;
  DoOnPingReply;
end;

procedure TDAOCConnection.ParseServerPingRequest(pPacket: TDAOCPacket);
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

procedure TDAOCConnection.ResumeConnection(const AFileName: string);
begin
  with TINIFile.Create(AFileName) do begin
    CryptKey := ReadString('ConnectionState', 'CryptKey', '');
    ServerIP := ReadString('ConnectionState', 'ServerIP', '');
    ClientIP := ReadString('ConnectionState', 'ClientIP', '');
    RegionID := ReadInteger('ConnectionState', 'RegionID', 0);
    ServerProtocol := ReadInteger('ConnectionState', 'ServerProtocol', 0);
    FLocalPlayer.Level := ReadInteger('ConnectionState', 'Level', 0);
    FLocalPlayer.Realm := TDAOCRealm(ReadInteger('ConnectionState', 'Realm', ord(drNeutral)));
    Free;
  end;

  if (FServerAddr = 0) or (FClientAddr = 0) then
    exit;

  FCryptKeySet := true; // CryptKey <> '000000000000000000000000';
  FActive := true;

  FTCPFromClient.Clear;
  FTCPFromServer.Clear;
end;

procedure TDAOCConnection.SaveConnectionState(const AFileName: string);
begin
  with TINIFile.Create(AFileName) do begin
    WriteString('ConnectionState', 'CryptKey', CryptKey);
    WriteString('ConnectionState', 'ServerIP', ServerIP);
    WriteString('ConnectionState', 'ClientIP', ClientIP);
    WriteInteger('ConnectionState', 'RegionID', RegionID);
    WriteInteger('ConnectionState', 'ServerProtocol', ServerProtocol);
    WriteInteger('ConnectionState', 'Level', FLocalPlayer.Level);
    WriteInteger('ConnectionState', 'Realm', ord(FLocalPlayer.Realm));
    Free;
  end;
end;

procedure TDAOCConnection.ParseGroupMembersUpdate(pPacket: TDAOCPacket);
var
  iCnt:   integer;
  wID:    WORD;
  pObj:   TDAOCObject;
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
    pPacket.getByte; // mana
    pPacket.seek(1);
    wID := pPacket.getShort;

    pObj := FDAOCObjs.FindByInfoID(wID);
    if Assigned(pObj) and (pObj.ObjectClass = ocPlayer) then
      TDAOCPlayer(pObj).IsInGroup := true
    else if wID <> FLocalPlayer.InfoID then
      Log('GroupMembersUpdate: Can not find player by InfoID 0x' + IntToHex(wID, 2));

    pPacket.getPascalString;  // name
    pPacket.getPascalString;  // class
  end;  { while !EOF }
end;

procedure TDAOCConnection.ParseGroupWindowUpdate(pPacket: TDAOCPacket);
begin
  pPacket.HandlerName := 'GroupWindowUpdate? NOTIMPL';
end;

procedure TDAOCConnection.UpdatePlayersInGuild;
var
  I:    integer;
begin
  for I := 0 to FDAOCObjs.Count - 1 do
    if FDAOCObjs[I].ObjectClass = ocPlayer then
      TDAOCPlayer(FDAOCObjs[I]).IsInGuild :=
        AnsiSameText(TDAOCPlayer(FDAOCObjs[I]).Guild, FLocalPlayer.Guild);
end;

procedure TDAOCConnection.ResetPlayersInGroup;
var
  I:    integer;
begin
  for I := 0 to FDAOCObjs.Count - 1 do
    if FDAOCObjs[I].ObjectClass = ocPlayer then
      TDAOCPlayer(FDAOCObjs[I]).IsInGroup := false;
end;

procedure TDAOCConnection.ParseAggroIndicator(pPacket: TDAOCPacket);
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

  if pAggressor is TDAOCMob then begin
    if FLocalPlayer.InfoID = wTarget then
      pTarget := FLocalPlayer
    else
      pTarget := FDAOCObjs.FindByInfoID(wTarget);
    if pTarget is TDAOCMovingObject then begin
      TDAOCMob(pAggressor).Target := TDAOCMovingObject(pTarget);
      DoOnMobTargetChanged(TDAOCMob(pAggressor));
    end;
  end;
*)
end;

procedure TDAOCConnection.DoOnMobTargetChanged(AMob: TDAOCMob);
begin
  if Assigned(FOnMobTargetChanged) then
    FOnMobTargetChanged(Self, AMob);
end;

end.


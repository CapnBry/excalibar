unit RemoteAdmin;

interface

uses
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPServer, Contnrs,
  DAOCControl, MapNavigator, StringParseHlprs
{$IFDEF VER130}
,Forms
{$ENDIF}
;


type
  TClientConn = TIdTCPServerConnection;
  TRequestActionHandler = procedure (AConn: TClientConn; const ACmd: string) of Object;

  TStringActionLink = class(TObject)
  public
    key:      string;
    handler:  TRequestActionHandler;
    help:     string;
  end;

  TdmdRemoteAdmin = class(TDataModule)
    tcpRemoteAdmin: TIdTCPServer;
    procedure tcpRemoteAdminExecute(AThread: TIdPeerThread);
    procedure tcpRemoteAdminConnect(AThread: TIdPeerThread);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FActions:   TObjectList;
    FDControl:  TDAOCControl;
    FCommandParams:   string;
    FCommandParamOffset: integer;  // 1-based

    function FindAction(const AKey: string) : TStringActionLink;
    procedure AddAction(const AKey: string; AHandler: TRequestActionHandler;
      const AHelp: string = '');
  protected
    function ParseParamWord : string;
    function ParseParamInt : integer;

    procedure HandleAutoMode(AConn: TClientConn; const ACmd: string);
    procedure HandleCommission(AConn: TClientConn; const ACmd: string);
    procedure HandleCurrency(AConn: TClientConn; const ACmd: string);
    procedure HandleCloseDialog(AConn: TClientConn; const ACmd: string);
    procedure HandleStyles(AConn: TClientConn; const ACmd: string);
    procedure HandleSpecializations(AConn: TClientConn; const ACmd: string);
    procedure HandleAbilities(AConn: TClientConn; const ACmd: string);
    procedure HandleSpells(AConn: TClientConn; const ACmd: string);
    procedure HandleSkills(AConn: TClientConn; const ACmd: string);
    procedure HandleInventory(AConn: TClientConn; const ACmd: string);
    procedure HandleDumpPath(AConn: TClientConn; const ACmd: string);
    procedure HandleLinkNearestTo(AConn: TClientConn; const ACmd: string);
    procedure HandleTurnToNode(AConn: TClientConn; const ACmd: string);
    procedure HandleMoveInv(AConn: TClientConn; const ACmd: string);
    procedure HandlePathTo(AConn: TClientConn; const ACmd: string);
    procedure HandleSet(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeClear(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeLoad(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeSave(AConn: TClientConn; const ACmd: string);
    procedure HandleSlashCommand(AConn: TClientConn; const ACmd: string);
    procedure HandleCharList(AConn: TClientConn; const ACmd: string);
    procedure HandleExit(AConn: TClientConn; const ACmd: string);
    procedure HandleLeft(AConn: TClientConn; const ACmd: string);
    procedure HandleRight(AConn: TClientConn; const ACmd: string);
    procedure HandleUp(AConn: TClientConn; const ACmd: string);
    procedure HandleDown(AConn: TClientConn; const ACmd: string);
    procedure HandleRawLoc(AConn: TClientConn; const ACmd: string);
    procedure HandleLoc(AConn: TClientConn; const ACmd: string);
    procedure HandleStop(AConn: TClientConn; const ACmd: string);
    procedure HandleHelp(AConn: TClientConn; const ACmd: string);
    procedure HandleGet(AConn: TClientConn; const ACmd: string);
    procedure HandleQuit(AConn: TClientConn; const ACmd: string);
    procedure HandleQuickbarPage(AConn: TClientConn; const ACmd: string);
    procedure HandleTurnTo(AConn: TClientConn; const ACmd: string);
    procedure HandleJump(AConn: TClientConn; const ACmd: string);
    procedure HandleTurnRateRecal(AConn: TClientConn; const ACmd: string);
    procedure HandleGotoXY(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeList(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeAdd(AConn: TClientConn; const ACmd: string);
    procedure HandleNodeNearest(AConn: TClientConn; const ACmd: string);
    procedure HandleGotoNode(AConn: TClientConn; const ACmd: string);
    procedure HandleLeftClick(AConn: TClientConn; const ACmd: string);
    procedure HandleRightClick(AConn: TClientConn; const ACmd: string);
    procedure HandleZone(AConn: TClientConn; const ACmd: string);
  public
    property DAOCControl: TDAOCControl read FDControl write FDControl;
  end;

var
  dmdRemoteAdmin: TdmdRemoteAdmin;

implementation

uses IdTCPConnection, DAOCObjs;

{$R *.dfm}

procedure TdmdRemoteAdmin.HandleLoc(AConn: TClientConn; const ACmd: string);
begin
  with FDControl do
    AConn.WriteLn(Format('200 Location (X,Y,Z) - (%u,%u,%u) head %d speed %d', [
      PlayerZoneX, PlayerZoneY, PlayerZoneZ, PlayerZoneHead, LocalPlayer.Speed]));
end;

procedure TdmdRemoteAdmin.tcpRemoteAdminExecute(AThread: TIdPeerThread);
var
  sCmd:   string;
  iPos:   integer;
  pAction:  TStringActionLink;
begin
  AThread.Connection.Write('> ');
  sCmd := AThread.Connection.ReadLn;
  if not Assigned(FDControl) then begin
    AThread.Connection.WriteLn('500 Internal error - No DAOC control assigned.');
    exit;
  end;

    { convert backspace to destructive backspace }
  iPos := Pos(#8, sCmd);
  while iPos > 0 do begin
      { if we're after the first char delete the BS and the preceding char } 
    if iPos > 1 then
      Delete(sCmd, iPos - 1, 2)
    else
      Delete(sCmd, iPos - 1, 1);
    iPos := Pos(#8, sCmd);
  end;

  if sCmd = '' then
    exit;

  pAction := FindAction(sCmd);
  if Assigned(pAction) then begin
    FCommandParams := copy(sCmd, Length(pAction.key) + 2, Length(sCmd));
    FCommandParamOffset := 1;
    pAction.handler(AThread.Connection, sCmd);
    exit;
  end;

  AThread.Connection.WriteLn('500 Unknown command: ' + sCmd);
end;

procedure TdmdRemoteAdmin.tcpRemoteAdminConnect(AThread: TIdPeerThread);
begin
  AThread.Connection.WriteLn('DAOC Skilla remote control');
end;

procedure TdmdRemoteAdmin.HandleStop(AConn: TClientConn; const ACmd: string);
begin
  FDControl.StopAllActions;
  AConn.WriteLn('200 All actions stopped.');
end;

procedure TdmdRemoteAdmin.HandleDown(AConn: TClientConn; const ACmd: string);
begin
  FDControl.ArrowDown := true;
  AConn.WriteLn('200 Down arrow pressed.');
end;

procedure TdmdRemoteAdmin.HandleLeft(AConn: TClientConn; const ACmd: string);
begin
  FDControl.ArrowLeft := true;
  AConn.WriteLn('200 Left arrow pressed.');
end;

procedure TdmdRemoteAdmin.HandleRight(AConn: TClientConn; const ACmd: string);
begin
  FDControl.ArrowRight := true;
  AConn.WriteLn('200 Right arrow pressed.');
end;

procedure TdmdRemoteAdmin.HandleUp(AConn: TClientConn; const ACmd: string);
begin
  FDControl.ArrowUp := true;
  AConn.WriteLn('200 Up arrow pressed.');
end;

procedure TdmdRemoteAdmin.DataModuleCreate(Sender: TObject);
begin
  FActions := TObjectList.Create;
  AddAction('Exit', HandleExit,
    'Close this connection.');
  AddAction('Loc', HandleLoc,
    'Return current zone-adjusted location information');
  AddAction('Get', HandleGet,
    'Get the value of in internal variable.');
  AddAction('Stop', HandleStop,
    'Stop all autonomous actions.');
  AddAction('Help', HandleHelp,
    'Display this help.');
  AddAction('Quit', HandleQuit,
    'Quit Dark Age of Camelot');
  AddAction('ArrowUp', HandleUp,
    'Press and hold the Up arrow.  Use ''stop'' to release.');
  AddAction('ArrowDown', HandleDown,
    'Press and hold the Down arrow.  Use ''stop'' to release.');
  AddAction('ArrowLeft', HandleLeft,
    'Press and hold the Left arrow.  Use ''stop'' to release.');
  AddAction('ArrowRight', HandleRight,
    'Press and hold the Right arrow.  Use ''stop'' to release.');
  AddAction('QuickBarPage', HandleQuickbarPage,
    '(page) Set the current quickbar page to <page>.  Valid values are 1-8.');
  AddAction('TurnTo', HandleTurnTo,
    '(heading) Set the player heading to <heading> degrees. (zone-relative)');
  AddAction('Jump', HandleJump,
    'Make the player jump.');
  AddAction('TurnRateRecal', HandleTurnRateRecal,
    'Recalibrate the turn rate metric.');
  AddAction('GotoXY', HandleGotoXY,
    '(x,y) Move the player to loc <x>,<y>.');
  AddAction('NodeList', HandleNodeList,
    '([nodename]) List all nodes in the map node list.  If a <nodename> is specified, only <nodename> is listed.');
  AddAction('NodeAdd', HandleNodeAdd,
    '(name) Add a node to the map node list at the current position and heading.');
  AddAction('NodeNearest', HandleNodeNearest,
    'Find the name of the node in the map node list closest to the current position.');
  AddAction('LeftClick', HandleLeftClick,
    '(x,y) Simulate a left mouse click at <x>,<y>.');
  AddAction('RightClick', HandleRightClick,
    '(x,y) Simulate a right mouse click at <x>,<y>.');
  AddAction('Zone', HandleZone,
    'Dump information about the current zone.');
  AddAction('CharList', HandleCharList,
    'Dump the list of characters on this account.');
  AddAction('RawLoc', HandleRawLoc,
    'Return current raw location information.');
  AddAction('/', HandleSlashCommand,
    'Send command directly to DAoC.');
  AddAction('GotoNode', HandleGotoNode,
    '(nodename) Move the player directly to map node <name>.');
  AddAction('NodeSave', HandleNodeSave,
    '(filename) Save the map node list to <filename>.');
  AddAction('NodeLoad', HandleNodeLoad,
    '(filename) Load the map node list from <filename>.');
  AddAction('NodeClear', HandleNodeClear,
    'Clear the map node list.');
  AddAction('Set', HandleSet,
    'Set the value of in internal variable.');
  AddAction('PathTo', HandlePathTo,
    '(nodename) Move the player to map node <name> via pathing if available.');
  AddAction('MoveInv', HandleMoveInv,
    '(frombag,frompos,tobag,topos) Move an inventory item. (1-based indexes).');
  AddAction('TurnToNode', HandleTurnToNode,
    '(nodename) Set the player heading to face map node <nodename>.');
  AddAction('LinkNearestTo', HandleLinkNearestTo,
    '(nodename) Link the node nearest the player to map node <nodename> as well as a reciprical link from <nodename> to the nearest node.');
  AddAction('DumpPath', HandleDumpPath,
    '(nodename) Display the path that will be taken from nearest node to <nodename>.');
  AddAction('Inventory', HandleInventory,
    'Dump the contents of the player''s inventory.');
  AddAction('Skills', HandleSkills,
    'Dump a list of the player''s tradeskills.');
  AddAction('Spells', HandleSpells,
    'Dump a list of the player''s spells.');
  AddAction('Abilities', HandleAbilities,
    'Dump a list of the player''s abilities.');
  AddAction('Specs', HandleSpecializations,
    'Dump a list of the player''s specializations.');
  AddAction('Styles', HandleStyles,
    'Dump a list of the player''s combat styles.');
  AddAction('Currency', HandleCurrency,
    'Display the amount of currency on the current local character.');
  AddAction('CloseDialog', HandleCloseDialog,
    'Clicks the OK button on a dialog.  If no dialog is visible then the click will still be issued!!');
  AddAction('Commission', HandleCommission,
    'Display information about the current tradeskill commission.');
  AddAction('AutoMode', HandleAutoMode,
    '([mode]) Sets automation mode to <mode>.  Options are none, and trade.');
end;

procedure TdmdRemoteAdmin.AddAction(const AKey: string;
  AHandler: TRequestActionHandler; const AHelp: string);
var
  pAction:  TStringActionLink;
begin
  pAction := TStringActionLink.Create;
  FActions.Add(pAction);
  pAction.key := AKey;
  pAction.handler := AHandler;
  pAction.help := AHelp;
end;

procedure TdmdRemoteAdmin.DataModuleDestroy(Sender: TObject);
begin
  FActions.Free;
end;

procedure TdmdRemoteAdmin.HandleExit(AConn: TClientConn; const ACmd: string);
begin
  AConn.Disconnect;
end;

function TdmdRemoteAdmin.FindAction(const AKey: string): TStringActionLink;
var
  I:  integer;
  sAction:  string;
begin
  for I := 0 to FActions.Count - 1 do begin
    sAction := TStringActionLink(FActions[I]).key;
    if (Length(sAction) > 1) and (Length(AKey) > Length(sAction)) then
      sAction := sAction + ' ';
    if StrLIComp(Pointer(AKey), Pointer(sAction), Length(sAction)) = 0 then begin
      Result := TStringActionLink(FActions[I]);
      exit;
    end;
  end;

  Result := nil;
end;

procedure TdmdRemoteAdmin.HandleHelp(AConn: TClientConn; const ACmd: string);
var
  I:    integer;
  s:    string;
begin
  s := '201 Help follows'#13#10;
  for I := 0 to FActions.Count - 1 do begin
    s := s + Trim(TStringActionLink(FActions[I]).key) + ' - ' +
      TStringActionLink(FActions[I]).help + #13#10;
  end;

  s := s + '.'#13#10;
  AConn.Write(s);
end;

procedure TdmdRemoteAdmin.HandleGet(AConn: TClientConn; const ACmd: string);
var
  sVar:   string;
begin
  sVar := ParseParamWord;
  if AnsiSameText(sVar, 'TurnRate') then
    AConn.WriteLn('200 TurnRate=' + IntToStr(FDControl.TurnRate))
//  else if AnsiSameText(sVar, 'TurnRateMax') then
//    AConn.WriteLn('200 TurnRateMax=' + IntToStr(FDControl.TurnRateMax))
  else
    AConn.WriteLn('500 Unknown variable: ' + sVar);
end;

procedure TdmdRemoteAdmin.HandleQuit(AConn: TClientConn;
  const ACmd: string);
begin
  FDControl.QuitDAOC;
  AConn.Writeln('200 Exiting Dark Age of Camelot.');
end;

procedure TdmdRemoteAdmin.HandleQuickbarPage(AConn: TClientConn;
  const ACmd: string);
var
  iPage:      integer;
begin
  iPage := ParseParamInt;
  FDControl.SetQuickbarPage(iPage);
  AConn.Writeln('200 Quickbar page set to ' + IntToStr(iPage) + '.')
end;

procedure TdmdRemoteAdmin.HandleTurnTo(AConn: TClientConn;
  const ACmd: string);
var
  sHead:    string;
  iHead:    integer;
begin                           
  sHead := ParseParamWord;
  if sHead = '' then begin
    AConn.WriteLn('500 Invalid heading specified (none).');
    exit;
  end;
  iHead := StrToIntDef(sHead, -1);
  if iHead = -1 then begin
    AConn.WriteLn('500 Invalid heading specified (' + sHead + ').');
    exit;
  end;

  AConn.WriteLn('200 Setting heading to ' + sHead + '.');

    { adjust for rotated zones }
  if Assigned(FDControl.Zone) then
    iHead := iHead + FDControl.Zone.Rotate mod 360;
  FDControl.SetPlayerHeading(iHead, 5000);
end;

procedure TdmdRemoteAdmin.HandleJump(AConn: TClientConn;
  const ACmd: string);
begin
  FDControl.Jump;
  AConn.WriteLn('200 Jumping.');
end;

procedure TdmdRemoteAdmin.HandleTurnRateRecal(AConn: TClientConn;
  const ACmd: string);
begin
  FDControl.TurnRateRecalibrate;
  AConn.WriteLn('200 Turn rate metric recalibrated (' +
    IntToStr(FDControl.TurnRate) + ').');
end;

procedure TdmdRemoteAdmin.HandleGotoXY(AConn: TClientConn;
  const ACmd: string);
var
  sAX:    string;
  sAY:    string;
  AX:     integer;
  AY:     integer;
begin
  sAX := ParseParamWord;
  sAY := ParseParamWord;

  AX := StrToIntDef(sAX, -1);
  if AX = -1 then begin
    AConn.WriteLn('500 Invalid GotoXY X value ' + sAX + '.');
    exit;
  end;
  AY := StrToIntDef(sAY, -1);
  if AY = -1 then begin
    AConn.WriteLn('500 Invalid GotoXY Y value ' + sAY + '.');
    exit;
  end;

  FDControl.GotoXY(AX, AY);
  AConn.WriteLn('200 GotoXY en route to (' + sAX + ',' + sAY + ').');
end;

procedure TdmdRemoteAdmin.HandleGotoNode(AConn: TClientConn; const ACmd: string);
var
  sNode:      string;
  pNode:      TMapNode;
begin
  sNode := copy(ACmd, 10, Length(ACmd));
  if sNode = '' then begin
    AConn.WriteLn('500 Invalid GotoNode name (NULL).');
    exit;
  end;

  pNode := FDControl.MapNodes.NodeByName(sNode);
  if not Assigned(pNode) then begin
    AConn.WriteLn('500 Invalid GotoNode name (' + sNode + ').  Node not found.');
    exit;
  end;

  FDControl.GotoNode(pNode);
  AConn.WriteLn('200 GotoNode en route to node (' + sNode + ').');
end;

procedure TdmdRemoteAdmin.HandleLeftClick(AConn: TClientConn;
  const ACmd: string);
var
  sAX:    string;
  sAY:    string;
  AX:     integer;
  AY:     integer;
begin
  sAX := ParseParamWord;;
  sAY := ParseParamWord;

  AX := StrToIntDef(sAX, -1);
  if AX = -1 then begin
    AConn.WriteLn('500 Invalid LeftClick X value ' + sAX + '.');
    exit;
  end;
  AY := StrToIntDef(sAY, -1);
  if AY = -1 then begin
    AConn.WriteLn('500 Invalid LeftClick Y value ' + sAY + '.');
    exit;
  end;

  FDControl.LeftClick(AX, AY);
  AConn.WriteLn('200 Left clicked at (' + sAX + ',' + sAY + ').');
end;

procedure TdmdRemoteAdmin.HandleNodeList(AConn: TClientConn;
  const ACmd: string);
var
  sParam: string;
  pNode:  TMapNode;
begin
  sParam := ParseParamWord;
  if sParam = '' then begin
    AConn.WriteLn('201 Map node list follows');
    AConn.Write(FDControl.MapNodes.NodesAsText);
    AConn.WriteLn('.');
  end
  
  else begin
    pNode := FDControl.MapNodes.NodeByName(sParam);
    if Assigned(pNode) then begin
      AConn.WriteLn('201 Map node follows');
      AConn.Write(pNode.AsText);
      AConn.WriteLn('.');
    end
    else
      AConn.WriteLn('300 NodeList could not find map node (' + sParam + ').');
  end;
end;

procedure TdmdRemoteAdmin.HandleNodeAdd(AConn: TClientConn;
  const ACmd: string);
var
  sName:      string;
begin
  sName := copy(ACmd, 9, Length(ACmd));
  try
    FDControl.NodeAddAtPlayerPos(sName);
    FDControl.MapNodes.Sort;
    AConn.WriteLn('200 Map node added (' + sName + ').');
  except
    on e: Exception do
      AConn.WriteLn('500 ' + e.Message);
  end;
end;

procedure TdmdRemoteAdmin.HandleNodeNearest(AConn: TClientConn;
  const ACmd: string);
var
  pNode:  TMapNode;
begin
  pNode := FDControl.NodeClosestToPlayerPos;
  if not Assigned(pNode) then
    AConn.WriteLn('300 Can not find a node close to the current location.')
  else
    AConn.WriteLn(Format('200 Closest node is [%s] dist %d dist3D %d bearing %d.',
      [pNode.Name, pNode.Distance2D(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y),
        pNode.Distance3D(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y, FDControl.LocalPlayer.Z),
       pNode.BearingFrom(FDControl.LocalPlayer.X, FDControl.LocalPlayer.Y)]));
end;

procedure TdmdRemoteAdmin.HandleZone(AConn: TClientConn;
  const ACmd: string);
begin
  if Assigned(FDControl.Zone) then begin
    AConn.WriteLn('200 Current zone info follows');
    AConn.Write(FDControl.Zone.AsString);
    AConn.WriteLn('.');
  end
  else
    AConn.WriteLn('300 No zone info found for zone.');
end;

procedure TdmdRemoteAdmin.HandleRightClick(AConn: TClientConn;
  const ACmd: string);
var
  sAX:    string;
  sAY:    string;
  AX:     integer;
  AY:     integer;
begin
  sAX := ParseParamWord;
  sAY := ParseParamWord;

  AX := StrToIntDef(sAX, -1);
  if AX = -1 then begin
    AConn.WriteLn('500 Invalid RightClick X value ' + sAX + '.');
    exit;
  end;
  AY := StrToIntDef(sAY, -1);
  if AY = -1 then begin
    AConn.WriteLn('500 Invalid RightClick Y value ' + sAY + '.');
    exit;
  end;

  FDControl.LeftClick(AX, AY);
  AConn.WriteLn('200 Right clicked at (' + sAX + ',' + sAY + ').');
end;

procedure TdmdRemoteAdmin.HandleRawLoc(AConn: TClientConn; const ACmd: string);
begin
  with FDControl.LocalPlayer do
    AConn.WriteLn(Format('200 Raw location (X,Y,Z) - (%u,%u,%u) head %d speed %d', [
      X, Y, Z, Head, Speed]));
end;

procedure TdmdRemoteAdmin.HandleCharList(AConn: TClientConn; const ACmd: string);
var
  I: Integer;
begin
  AConn.WriteLn('201 Character list follows for account ' +
    FDControl.AccountCharacterList.AccountName);
  for I := 0 to FDControl.AccountCharacterList.Count - 1 do
    AConn.WriteLn(FDControl.AccountCharacterList[I].AsString);
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleSlashCommand(AConn: TClientConn;
  const ACmd: string);
begin
  FDControl.DoSendKeys(ACmd + '[cr]');
  AConn.WriteLn('200 Sent command ' + ACmd);
end;

procedure TdmdRemoteAdmin.HandleNodeSave(AConn: TClientConn;
  const ACmd: string);
var
  iStartPos:  Integer;
  sFName:     string;
begin
  iStartPos := 10;
  sFName := ParseWordEx(ACmd, iStartPos, pcsFILENAME_CHARS);
  if sFName = '' then begin
    AConn.WriteLn('500 Invalid NodeSave filename (NULL).');
    exit;
  end;

  if ExtractFileExt(sFName) = '' then
    sFName := sFName + '.nod';
    
  try
    FDControl.MapNodes.SaveToFile(sFName);
    AConn.WriteLn('200 Map node list saved to (' + sFName + ').');
  except
    on e: Exception do
      AConn.WriteLn('300 Could not save map node list to (' + sFName + ').  ' + E.Message);
  end;  { try/except }
end;

procedure TdmdRemoteAdmin.HandleNodeClear(AConn: TClientConn;
  const ACmd: string);
begin
  FDControl.MapNodes.Clear;
  AConn.WriteLn('200 Map node list cleared.');
end;

procedure TdmdRemoteAdmin.HandleSet(AConn: TClientConn;
  const ACmd: string);
var
  sVar:   string;
begin
  sVar := ParseParamWord;
  if AnsiSameText(sVar, 'GotoDistTolerance') then begin
    FDControl.GotoDistTolerance := ParseParamInt;
    AConn.WriteLn('200 GotoDistTolerance=' + IntToStr(FDControl.GotoDistTolerance))
  end
  else if AnsiSameText(sVar, 'DAOCPath') then begin
    FDControl.DAOCPath := copy(ACmd, 13, Length(ACmd));
    AConn.WriteLn('200 DAOCPath=' + FDControl.DAOCPath)
  end
  else
    AConn.WriteLn('500 Unknown variable: ' + sVar);
end;

procedure TdmdRemoteAdmin.HandlePathTo(AConn: TClientConn;
  const ACmd: string);
var
  sNode:      string;
  pNode:      TMapNode;
begin
  sNode := copy(ACmd, 8, Length(ACmd));
  if sNode = '' then begin
    AConn.WriteLn('500 Invalid PathTo node name (NULL).');
    exit;
  end;

  pNode := FDControl.MapNodes.NodeByName(sNode);
  if not Assigned(pNode) then begin
    AConn.WriteLn('500 Invalid PathTo node name (' + sNode + ').  Node not found.');
    exit;
  end;

  if FDControl.PathToNode(pNode) then
    AConn.WriteLn('200 PathTo en route to node (' + sNode + ').')
  else
    AConn.WriteLn('300 PathTo cannot path to node (' + sNode + ').');
end;

procedure TdmdRemoteAdmin.HandleNodeLoad(AConn: TClientConn;
  const ACmd: string);
var
  iStartPos:  Integer;
  sFName:     string;
begin
  iStartPos := 10;
  sFName := ParseWordEx(ACmd, iStartPos, pcsFILENAME_CHARS);
  if sFName = '' then begin
    AConn.WriteLn('500 Invalid NodeLoad filename (NULL).');
    exit;
  end;

  if ExtractFileExt(sFName) = '' then
    sFName := sFName + '.nod';
     
  try
    FDControl.MapNodes.LoadFromFile(sFName);
    AConn.WriteLn('200 Map node list loaded from (' + sFName + ').');
  except
    on e: Exception do
      AConn.WriteLn('300 Could not load map node list from (' + sFName + ').  ' + E.Message);
  end;  { try/except }
end;

procedure TdmdRemoteAdmin.HandleMoveInv(AConn: TClientConn;
  const ACmd: string);
var
  sFromBag: string;
  sFromPos: string;
  sToBag: string;
  sToPos: string;
  iFromBag: integer;
  iFromPos: integer;
  iToBag: integer;
  iToPos: integer;
begin
  sFromBag := ParseParamWord;
  sFromPos := ParseParamWord;
  sToBag := ParseParamWord;
  sToPos := ParseParamWord;
  if (sFromBag = '') or (sFromPos = '') or (sToBag = '') or (sToPos = '') then begin
    AConn.WriteLn('500 MoveInv (NULL) parameter specified for FromBag, FromPos, ToBag, or ToPos.');
    exit;
  end;

  iFromBag := StrToIntDef(sFromBag, 0);
  iFromPos := StrToIntDef(sFromPos, 0);
  iToBag := StrToIntDef(sToBag, 0);
  iToPos := StrToIntDef(sToPos, 0);
  if (iFromBag = 0) or (iFromPos = 0) or (iToBag = 0) or (iToPos = 0) then begin
    AConn.WriteLn('500 MoveInv non-integer parameter specified for FromBag, FromPos, ToBag, or ToPos.');
    exit;
  end;

  FDControl.WindowManager.MoveInventoryItem(iFromBag-1, iFromPos-1, iToBag-1, iToPos-1);
  AConn.WriteLn(Format('200 MoveInv complete (%d, %d) -> (%d, %d).', [
    iFromBag, iFromPos, iToBag, iToPos]));
end;

function TdmdRemoteAdmin.ParseParamInt : integer;
begin
  Result := ParseInt(FCommandParams, FCommandParamOffset);
end;

function TdmdRemoteAdmin.ParseParamWord : string;
begin
  Result := ParseWord(FCommandParams, FCommandParamOffset);
end;

procedure TdmdRemoteAdmin.HandleTurnToNode(AConn: TClientConn;
  const ACmd: string);
var
  sNode:  string;
  pNode:  TMapNode;
begin
  sNode := ParseParamWord;
  if sNode = '' then begin
    AConn.WriteLn('500 TurntoNode invalid map node name specified (NULL).');
    exit;
  end;

  pNode := FDControl.MapNodes.NodeByName(sNode);
  if Assigned(pNode) then begin
    AConn.WriteLn('200 TurnToNode turning to face map node (' + sNode + ').');
    FDControl.FaceNode(pNode, 3000);
  end
  else
    AConn.WriteLn('300 TurnToNode invalid map node name specified (' + sNode + ').');
end;

procedure TdmdRemoteAdmin.HandleLinkNearestTo(AConn: TClientConn;
  const ACmd: string);
var
  sNode:      string;
  pSrcNode:   TMapNode;
  pDestNode:  TMapNode;
begin
  pSrcNode := FDControl.NodeClosestToPlayerPos;
  if not Assigned(pSrcNode) then begin
    AConn.WriteLn('300 LinkNearestTo can not find a nearest node to link from.');
    exit;
  end;

  sNode := copy(ACmd, 15, Length(ACmd));
  if sNode = '' then begin
    AConn.WriteLn('500 LinkNearestTo invalid map node name specified (NULL).');
    exit;
  end;

  pDestNode := FDControl.MapNodes.NodeByName(sNode);
  if not Assigned(pDestNode) then begin
    AConn.WriteLn('500 LinkNearestTo invalid map node name specified (' + sNode + ').');
    exit;
  end;

  try
    pSrcNode.AddConnection(pDestNode);
    pDestNode.AddConnection(pSrcNode);
    AConn.WriteLn('200 LinkNearestTo linked (' + pSrcNode.Name + ') <=> (' +
      pDestNode.Name + ').');
  except
    on E: Exception do
      AConn.WriteLn('300 LinkNearestTo could not link (' + pSrcNode.Name +
        ') <=> (' + pDestNode.Name + '): ' + e.Message);
  end;
end;

procedure TdmdRemoteAdmin.HandleDumpPath(AConn: TClientConn;
  const ACmd: string);
var
  sNode:      string;
  pNode:      TMapNode;
  pSrcNode:   TMapNode;
  pPath:      TMapNodeList;
begin
  pSrcNode := FDControl.NodeClosestToPlayerPos;
  if not Assigned(pSrcNode) then begin
    AConn.WriteLn('300 DumpPath can not find a nearest node to start from.');
    exit;
  end;

  sNode := copy(ACmd, 10, Length(ACmd));
  if sNode = '' then begin
    AConn.WriteLn('500 Invalid DumpPath destination node name (NULL).');
    exit;
  end;

  pNode := FDControl.MapNodes.NodeByName(sNode);
  if not Assigned(pNode) then begin
    AConn.WriteLn('500 Invalid DumpPath destination node name (' + sNode + ').  Node not found.');
    exit;
  end;

  pPath := pSrcNode.FindPathTo(pNode);
  if Assigned(pPath) then begin
    AConn.WriteLn('201 DumpPath pathing information follows (' + pSrcNode.Name +
      ') -> (' + pNode.Name + ').');
    AConn.WriteLn(pPath.NodesAsText);
    AConn.WriteLn('.');
  end
  else
    AConn.WriteLn('300 DumpPath cannot find path (' + pSrcNode.Name +
      ') -> (' + pNode.Name + ').');

  pPath.Free;
end;

procedure TdmdRemoteAdmin.HandleInventory(AConn: TClientConn; const ACmd: string);
begin
  AConn.WriteLn('201 Player inventory follows');
  AConn.WriteLn(FDControl.LocalPlayer.Inventory.AsText);
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleSkills(AConn: TClientConn; const ACmd: string);
var
  I:    integer;
begin
  AConn.WriteLn('201 Player skill list follows');
  with FDControl.LocalPlayer.Skills do
    for I := 0 to Count - 1 do
      AConn.WriteLn(Format('  %s = %d', [Items[I].Name, Items[I].Value]));
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleSpells(AConn: TClientConn;
  const ACmd: string);
var
  I:    integer;
begin
  AConn.WriteLn('201 Player spell list follows');
  with FDControl.LocalPlayer.Spells do
    for I := 0 to Count - 1 do
      AConn.WriteLn('  ' + Items[I].Name);
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleAbilities(AConn: TClientConn;
  const ACmd: string);
var
  I:  integer;
begin
  AConn.WriteLn('201 Player ability list follows');
  with FDControl.LocalPlayer.Abilities do
    for I := 0 to Count - 1 do
      AConn.WriteLn('  ' + Items[I].Name);
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleSpecializations(AConn: TClientConn;
  const ACmd: string);
var
  I:  integer;
begin
  AConn.WriteLn('201 Player specialization list follows');
  with FDControl.LocalPlayer.Specializations do
    for I := 0 to Count - 1 do
      AConn.WriteLn(Format('  %s = %d', [Items[I].Name, Items[I].Value]));
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleStyles(AConn: TClientConn; const ACmd: string);
var
  I:  integer;
begin
  AConn.WriteLn('201 Player combat style list follows');
  with FDControl.LocalPlayer.Styles do
    for I := 0 to Count - 1 do
      AConn.WriteLn('  ' + Items[I].Name);
  AConn.WriteLn('.');
end;

procedure TdmdRemoteAdmin.HandleCloseDialog(AConn: TClientConn;
  const ACmd: string);
begin
  AConn.WriteLn('200 Closing dialog.');
  FDControl.CloseDialog;
end;

procedure TdmdRemoteAdmin.HandleCurrency(AConn: TClientConn; const ACmd: string);
begin
  AConn.WriteLn('200 Current currency: ' + FDControl.LocalPlayer.Currency.AsText + '.');
end;

procedure TdmdRemoteAdmin.HandleCommission(AConn: TClientConn;
  const ACmd: string);
begin
  if FDControl.TradeCommissionNPC = '' then
    AConn.WriteLn('200 No tradeskill commission assigned.')
  else
    AConn.WriteLn('200 Tradeskill commission: [' + FDControl.TradeCommissionItem +
      '] for [' + FDControl.TradeCommissionNPC + '].');
end;

procedure TdmdRemoteAdmin.HandleAutoMode(AConn: TClientConn; const ACmd: string);
var
  sMode:  string;
begin
  sMode := ParseParamWord;
  if AnsiSameText(sMode, 'trade') then
    FDControl.AutomationMode := amCommission
  else
    FDControl.AutomationMode := amNone;
  if sMode <> '' then
    AConn.WriteLn('200 Automation mode set to ' + sMode + '.')
  else
    AConn.WriteLn('200 Automation mode set to none.');
end;

end.


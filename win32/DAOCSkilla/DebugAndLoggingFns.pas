unit DebugAndLoggingFns;

(****************************************************************************
**
** Copyright (C) 2004 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  SysUtils, Classes, DAOCConnection, DAOCObjs, DAOCInventory, GameNetPackets;

type
  TStringFunc = procedure (const s: string) of object;

  TDebugLoggingState = class
  private
    FMobseenFile:   TStream;
    FDelveseenFile: TStream;
    FChatLog:       TStream;

    FRecordMobseen: boolean;
    FDumpPacketsToLog: boolean;
    FLogSink: TStringFunc;
    FRecordDelveseen: boolean;
    FChatLogXIEnabled: boolean;
    FChatLogEnabled: boolean;
    FChatLogFileName: string;

    procedure CheckWriteAllMobseen(AConn: TDAOCConnection);
    procedure CheckWriteMobseen(AConn: TDAOCConnection; ADAOCObject: TDAOCObject);
    procedure SetRecordMobseen(const Value: boolean);
    function GetMobseenFileName : string;
    function GetDelveseenFileName : string;
    procedure Log(const s: string);
    procedure SetRecordDelveseen(const Value: boolean);
    procedure ChatLogXI(Sender: TObject; const s: string);
    procedure CreateChatLog;
    procedure CloseChatLog;
    procedure LogLocalPlayerXI(AConn: TDAOCConnection);
    procedure SetChatLogFileName(const Value: string);
    procedure SetChatLogEnabled(const Value: boolean);
  public
    destructor Destroy; override;

    procedure DAOCZoneChanged(Sender: TObject);
    procedure DAOCAfterPacket(Sender: TObject; APacket: TGameNetPacket);
    procedure DAOCNewObject(Sender: TObject; AObj: TDAOCObject);
    procedure DAOCDelveItem(Sender: TObject; AItem: TDAOCInventoryItem);
    procedure DAOCChatLog(Sender: TObject; const s: string);
    procedure DAOCSelectedObjectChanged(Sender: TObject; ADAOCObject: TDAOCObject);

    procedure DumpMobsToLog(AConn: TDAOCConnection);

    property ChatLogEnabled: boolean read FChatLogEnabled write SetChatLogEnabled;
    property ChatLogFileName: string read FChatLogFileName write SetChatLogFileName;
    property ChatLogXIEnabled: boolean read FChatLogXIEnabled write FChatLogXIEnabled;
    property RecordMobseen: boolean read FRecordMobseen write SetRecordMobseen;
    property RecordDelveseen: boolean read FRecordDelveseen write SetRecordDelveseen;
    property DumpPacketsToLog: boolean read FDumpPacketsToLog write FDumpPacketsToLog;
    property LogSink: TStringFunc read FLogSink write FLogSink;
  end;

implementation

const
  CHAT_XI_PREFIX = 'XI: ';

{ TDebugLoggingState }

procedure TDebugLoggingState.ChatLogXI(Sender: TObject; const s: string);
begin
  if FChatLogXIEnabled then
    DAOCChatLog(Sender, CHAT_XI_PREFIX + s);
end;

procedure TDebugLoggingState.CheckWriteAllMobseen(AConn: TDAOCConnection);
var
  pObj:   TDAOCObject;
begin
  pObj := AConn.DAOCObjects.Head;
  while Assigned(pObj) do begin
    CheckWriteMobseen(AConn, pObj);
    pObj := pObj.Next;
  end;
end;

procedure TDebugLoggingState.CheckWriteMobseen(AConn: TDAOCConnection; ADAOCObject: TDAOCObject);
var
  s:         string;
  X, Y, Z:   Cardinal;
begin
  if Assigned(FMobseenFile) and (ADAOCObject.ObjectClass in [ocMob, ocVehicle]) and
    Assigned(AConn.Zone) then begin
    X := AConn.Zone.WorldToZoneX(ADAOCObject.X);
    Y := AConn.Zone.WorldToZoneY(ADAOCObject.Y);
    Z := ADAOCObject.Z;
    if (X >= 65535) or (Y > 65535) then
      exit;
    s := Format('MOBseen,%d,%d,%d,%d,%d,%s,', [
      AConn.Zone.ZoneNum, X, Y, Z,
      ADAOCObject.Level, ADAOCObject.Name]);
    if ADAOCObject.ObjectClass = ocMob then
      s := s + TDAOCMob(ADAOCObject).TypeTag;
    s := s + #13#10;
    FMobseenFile.Write(s[1], Length(s));
  end;
end;

procedure TDebugLoggingState.CloseChatLog;
var
  sCloseLine: string;
begin
  if Assigned(FChatLog) then begin
    sCloseLine := #13#10'*** Chat Log Closed: ' +
      FormatDateTime('ddd mmm dd hh:nn:ss yyyy', Now) + // Tue Jan 08 08:09:33 2002
      #13#10#13#10#13#10;
    FChatLog.Write(sCloseLine[1], Length(sCloseLine));

    FChatLog.Free;
    FChatLog := nil;
  end;
end;

procedure TDebugLoggingState.CreateChatLog;
var
  sOpenLine:    string;
  sDirectory:   string;
begin
  if Assigned(FChatLog) then
    CloseChatLog;

    { make sure we have a file, Delphi 6 will not respect the share mode on an fmCreate }
  if not FileExists(FChatLogFileName) then begin
    sDirectory := ExtractFilePath(FChatLogFileName);
      { make sure the directory exists }
    if sDirectory <> '' then
      ForceDirectories(sDirectory);
    try
      FChatLog := TFileStream.Create(FChatLogFileName, fmCreate);
    except
      on E: Exception do begin
          { if we get an exception, log it and turn off the chat file }
        FChatLogEnabled := false;
        Log(e.Message);
        exit;
      end;
    end;
    FreeAndNil(FChatLog);
  end;  { if creating a new file }

  FChatLog := TFileStream.Create(FChatLogFileName, fmOpenWrite or fmShareDenyNone);
  FChatLog.Seek(0, soFromEnd);

  sOpenLine := #13#10'*** Chat Log Opened: ' +
    FormatDateTime('ddd mmm dd hh:nn:ss yyyy', Now) + // Tue Jan 08 08:09:33 2002
    #13#10#13#10;
  FChatLog.Write(sOpenLine[1], Length(sOpenLine));
end;

procedure TDebugLoggingState.DAOCAfterPacket(Sender: TObject; APacket: TGameNetPacket);
begin
  if FDumpPacketsToLog then begin
    Log('---- ' + APacket.IPProtocolStr + ' packet ' + APacket.IsFromClientStr + ' ---- ' + APacket.HandlerName);
    Log(APacket.AsString);
  end;
end;

procedure TDebugLoggingState.DAOCChatLog(Sender: TObject; const s: string);
var
  sChatLogLine:   string;
begin
  sChatLogLine := FormatDateTime('[hh:nn:ss] ', Now) + s + #13#10;
  if FChatLogEnabled then begin
    if not Assigned(FChatLog) then
      CreateChatLog;

    if Assigned(FChatLog) then
      FChatLog.Write(sChatLogLine[1], Length(sChatLogLine))
  end;
end;

procedure TDebugLoggingState.DAOCDelveItem(Sender: TObject; AItem: TDAOCInventoryItem);
var
  s:    string;
begin
  s := 'DELVE,' + IntToStr(ord(TDAOCConnection(Sender).LocalPlayer.Realm)) + ',' +
    AItem.SummaryLine;
  ChatLogXI(Sender, s);

  if not (Assigned(FDelveseenFile) and Assigned(AItem.DelveInfo)) then
    exit;

  s := s + #13#10;
  FDelveseenFile.Write(s[1], Length(s));
end;

procedure TDebugLoggingState.DAOCNewObject(Sender: TObject; AObj: TDAOCObject);
begin
  CheckWriteMobseen(TDAOCConnection(Sender), AObj);
end;

procedure TDebugLoggingState.DAOCSelectedObjectChanged(Sender: TObject;
  ADAOCObject: TDAOCObject);
begin
  if Assigned(ADAOCObject) and (ADAOCObject.ObjectClass = ocMob) then begin
    ChatLogXI(Sender, Format('New Target: "%s" Level: %d Health: %d%%',
      [ADAOCObject.Name, ADAOCObject.Level, ADAOCObject.HitPoints]));
    LogLocalPlayerXI(TDAOCConnection(Sender));
  end;
end;

procedure TDebugLoggingState.DAOCZoneChanged(Sender: TObject);
begin
  CheckWriteAllMobseen(TDAOCConnection(Sender));
end;

destructor TDebugLoggingState.Destroy;
begin
  CloseChatLog;
  FreeAndNil(FMobseenFile);
  FreeAndNil(FDelveseenFile);

  inherited;
end;

procedure TDebugLoggingState.DumpMobsToLog(AConn: TDAOCConnection);
var
  pObj:   TDAOCObject;
begin
  pObj := AConn.DAOCObjects.Head;
  while Assigned(pObj) do begin
    Log(pObj.AsString);
    pObj := pObj.Next;
  end;
end;

function TDebugLoggingState.GetDelveseenFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'delveseen.csv';
end;

function TDebugLoggingState.GetMobseenFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'mobseen.csv';
end;

procedure TDebugLoggingState.Log(const s: string);
begin
  if Assigned(FLogSink) then
    FLogSink(s);
end;

procedure TDebugLoggingState.LogLocalPlayerXI(AConn: TDAOCConnection);
begin
  with AConn.LocalPlayer do
    ChatLogXI(AConn, Format(
      'Local Player: "%s" Level: %d Health: %d%% Endurance: %d%% Mana: %d%%',
      [Name, Level, HitPoints, EndurancePct, ManaPct]));
end;

procedure TDebugLoggingState.SetChatLogEnabled(const Value: boolean);
begin
  FChatLogEnabled := Value;
  if not FChatLogEnabled then
    CloseChatLog;
  { open is handled when the first line comes across }
end;

procedure TDebugLoggingState.SetChatLogFileName(const Value: string);
begin
  if FChatLogFileName = Value then
    exit;

  FChatLogFileName := Value;
    { close the chat log.  The new file name will take effect on the next log line }
  CloseChatLog;
end;

procedure TDebugLoggingState.SetRecordDelveseen(const Value: boolean);
begin
  if FRecordDelveseen = Value then
    exit;

  FRecordDelveseen := Value;
  if FRecordDelveseen then begin
      { make sure the file exists and is blank before we start, we do this
        because Delphi doesn't respect the ShareMode on fmCreate }
    FDelveseenFile := TFileStream.Create(GetDelveseenFileName, fmCreate);
    FDelveseenFile.Free;
    FDelveseenFile := TFileStream.Create(GetDelveseenFileName, fmOpenWrite or fmShareDenyWrite);
    // BRY2: Does not know which connection to spool for
    // CheckWriteAllDelveseen;
  end
  else
    FreeAndNil(FDelveseenFile);
end;

procedure TDebugLoggingState.SetRecordMobseen(const Value: boolean);
begin
  if FRecordMobseen = Value then
    exit;

  FRecordMobseen := Value;
  if FRecordMobseen then begin
      { make sure the file exists and is blank before we start, we do this
        because Delphi doesn't respect the ShareMode on fmCreate }
    FMobseenFile := TFileStream.Create(GetMobseenFileName, fmCreate);
    FMobseenFile.Free;
    FMobseenFile := TFileStream.Create(GetMobseenFileName, fmOpenWrite or fmShareDenyWrite);
    // BRY2: Does not know which connection to spool for
    // CheckWriteAllMobseen;
  end
  else
    FreeAndNil(FMobseenFile);
end;

end.

unit QuickLaunchChars;

interface

uses
  Windows, SysUtils, Classes, Contnrs, FrameFns, INIFiles;

type
  TQuickLaunchCharList = class;

  TQuickLaunchChar = class(TObject)
  private
    FServerAddr: DWORD;
    FAccount: string;
    FName: string;
    FPassword: string;
    FRealm: integer;
    function GetServerIP: string;
    function GetServerName: string;
    function GetServer: string;
    function GetDisplayName: string;
  protected
    FParent:    TQuickLaunchCharList;
  public
    property Account: string read FAccount write FAccount;
    property Password: string read FPassword write FPassword;
    property DisplayName: string read GetDisplayName;
    property Name: string read FName write FName;
    property Realm: integer read FRealm write FRealm;
    property ServerAddr: DWORD read FServerAddr write FServerAddr;
    property ServerIP: string read GetServerIP;
    property ServerName: string read GetServerName;
    property Server: string read GetServer;
  end;

  TQuickLaunchCharList = class(TObjectList)
  private
    FServerNameFile: string;
    function GetItems(I: integer): TQuickLaunchChar;
    procedure SaveServerName(AServerAddr: DWORD; const AServerName: string);
  protected
    function ServerNameForAddr(AAddr: DWORD) : string;
  public
    procedure Add(AItem: TQuickLaunchChar);
    procedure AddOrUpdateChar(const AAccountName, AAcountPassword, AServerName,
      ACharacterName: string; ARealm: integer; AServerAddr: DWORD);
    function FindChar(const AAccountName, ACharacterName: string; AServerAddr: DWORD) : TQuickLaunchChar;
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);

    property Items[I: integer]: TQuickLaunchChar read GetItems; default;
    property ServerNameFile: string read FServerNameFile write FServerNameFile;
  end;

  TQuickLaunchProfile = class(TObject)
  private
    FCharINIFilename: string;       // local charini with overwrites
    FProfileName: string;
    FDAoCCharINIFilename: string;   // real charini in daoc dir
    FUserDATFilename: string;       // local user.dat with overwrites
  public
    procedure Activate(const ADAOCPath: string);

    property ProfileName: string read FProfileName write FProfileName;
    property CharINIFilename: string read FCharINIFilename write FCharINIFilename;
    property DAoCCharINIFilename: string read FDAoCCharINIFilename write FDAoCCharINIFilename;
    property UserDATFilename: string read FUserDATFilename write FUserDATFilename;
  end;

  TQuickLaunchProfileList = class(TObjectList)
  private
    function GetItems(I: integer): TQuickLaunchProfile;
  public
    procedure LoadFromFile(AFileName: string);

    property Items[I: integer]: TQuickLaunchProfile read GetItems; default;
  end;

implementation

{ TQuickLaunchCharList }

procedure TQuickLaunchCharList.Add(AItem: TQuickLaunchChar);
begin
  inherited Add(AItem);
  AItem.FParent := Self;
end;

procedure TQuickLaunchCharList.AddOrUpdateChar(const AAccountName,
  AAcountPassword, AServerName, ACharacterName: string; ARealm: integer;
  AServerAddr: DWORD);
var
  pTmpItem:   TQuickLaunchChar;
begin
  if AServerAddr = 0 then
    exit;
    
  pTmpItem := FindChar(AAccountName, ACharacterName, AServerAddr);
  if not Assigned(pTmpItem) then begin
    pTmpItem := TQuickLaunchChar.Create;
    Insert(0, pTmpItem);
    pTmpItem.FParent := Self;
    pTmpItem.Account := AAccountName;
    pTmpItem.Name := ACharacterName;
    pTmpItem.ServerAddr := AServerAddr;
    pTmpItem.Realm := ARealm;
  end;

  pTmpItem.FPassword := AAcountPassword;
  SaveServerName(AServerAddr, AServerName);
end;

function TQuickLaunchCharList.FindChar(const AAccountName,
  ACharacterName: string; AServerAddr: DWORD): TQuickLaunchChar;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if (Name = ACharacterName) and (Account = AAccountName) and
        (ServerAddr = AServerAddr) then begin
        Result := Items[I];
        exit;
      end;

  Result := nil;
end;

function TQuickLaunchCharList.GetItems(I: integer): TQuickLaunchChar;
begin
  Result := TQuickLaunchChar(inherited Items[I]);
end;

procedure TQuickLaunchCharList.LoadFromFile(AFileName: string);
var
  I:          integer;
  iCnt:       integer;
  pTmpItem:   TQuickLaunchChar;
  sName:      string;
begin
  with TINIFile.Create(AFileName) do begin
    iCnt := ReadInteger('QuickLaunch', 'Count', 0);
    for I := 0 to iCnt - 1 do begin
      sName := ReadString('QuickLaunch', 'Name' + IntToStr(I), '');
      if sName <> '' then begin
        pTmpItem := TQuickLaunchChar.Create;
        pTmpItem.Name := sName;
        Add(pTmpItem);
        pTmpItem.Account := ReadString('QuickLaunch', 'Account' + IntToStr(I), '');
        pTmpItem.Password := ReadString('QuickLaunch', 'Password' + IntToStr(I), '');
        pTmpItem.Realm := ReadInteger('QuickLaunch', 'Realm' + IntToStr(I), 1);
        pTmpItem.FServerAddr := Cardinal(ReadInteger('QuickLaunch', 'ServerAddr' + IntToStr(I), 0));
      end;  { if name }
    end;  { for I }

    Free;
  end;  { with INI }
end;

procedure TQuickLaunchCharList.SaveServerName(AServerAddr: DWORD;
  const AServerName: string);
begin
  if FServerNameFile = '' then
    exit;

  with TIniFile.Create(FServerNameFile) do begin
    WriteString('Servers', IntToHex(AServerAddr, 8), AServerName);
    Free;
  end;
end;

procedure TQuickLaunchCharList.SaveToFile(AFileName: string);
var
  I:          integer;
begin
  with TINIFile.Create(AFileName) do begin
    EraseSection('QuickLaunch');
    
    WriteInteger('QuickLaunch', 'Count', Count);
    for I := 0 to Count - 1 do begin
      WriteString('QuickLaunch', 'Name' + IntToStr(I), Items[I].Name);
      WriteString('QuickLaunch', 'Account' + IntToStr(I), Items[I].Account);
      WriteString('QuickLaunch', 'Password' + IntToStr(I), Items[I].Password);
      WriteInteger('QuickLaunch', 'Realm' + IntToStr(I), Items[I].Realm);
      WriteInteger('QuickLaunch', 'ServerAddr' + IntToStr(I), Items[I].ServerAddr);
    end;  { for I }

    Free;
  end;  { with INI }
end;

function TQuickLaunchCharList.ServerNameForAddr(AAddr: DWORD): string;
begin
  if FServerNameFile = '' then
    Result := ''
  else
    with TIniFile.Create(FServerNameFile) do begin
      Result := ReadString('Servers', IntToHex(AAddr, 8), '');
      Free;
    end;  { with }
end;

{ TQuickLaunchChar }

function TQuickLaunchChar.GetDisplayName: string;
begin
  Result := Format('%s (%s)', [FName, Server]);
end;

function TQuickLaunchChar.GetServer: string;
begin
  Result := ServerName;
  if Result = '' then
    Result := ServerIP;
end;

function TQuickLaunchChar.GetServerIP: string;
begin
  Result := my_inet_ntoa(FServerAddr);
end;

function TQuickLaunchChar.GetServerName: string;
begin
  if Assigned(FParent) then
    Result := FParent.ServerNameForAddr(FServerAddr)
  else
    Result := '';
end;

{ TQuickLaunchProfile }

procedure TQuickLaunchProfile.Activate(const ADAOCPath: string);
var
  UserDAT: TINIFile;
  CharINI: TINIFile;
  DAoCINI: TINIFile;
  DAoCDAT: TINIFile;
  Sections: TStringList;
  Values: TStringList;
  I: integer;
  J: integer;
begin
  Sections := TStringList.Create;
  Values := TStringList.Create;

    { copy the user.dat settings over }
  if FUserDATFilename <> '' then begin
    UserDAT := TINIFile.Create(ExtractFilePath(ParamStr(0)) + FUserDATFilename);
    DAoCDAT := TINIFile.Create(ADAOCPath + 'user.dat');
    UserDAT.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do begin
      UserDAT.ReadSectionValues(Sections[I], Values);
      for J := 0 to Values.Count - 1 do
        DAoCDAT.WriteString(Sections[I], Values.Names[J], Values.Values[Values.Names[J]]);
    end;
    DAoCDAT.Free;
    UserDAT.Free;
  end;  { if we have a userdatini }

    { copy the char INI settings over }
  if (FCharINIFilename <> '') and (FDAoCCharINIFilename <> '') then begin
    CharINI := TINIFile.Create(ExtractFilePath(ParamStr(0)) + FCharINIFilename);
    DAoCINI := TINIFile.Create(ADAOCPath + FDAoCCharINIFilename);
    CharINI.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do begin
      CharINI.ReadSectionValues(Sections[I], Values);
      for J := 0 to Values.Count - 1 do
        DAoCINI.WriteString(Sections[I], Values.Names[J], Values.Values[Values.Names[J]]);
    end;
    DAoCINI.Free;
    CharINI.Free;
  end;  { if we have a charini and daoccharini }

  Sections.Free;
  Values.Free;
end;

{ TQuickLaunchProfileList }

function TQuickLaunchProfileList.GetItems(I: integer): TQuickLaunchProfile;
begin
  Result := TQuickLaunchProfile(inherited Items[I]);
end;

procedure TQuickLaunchProfileList.LoadFromFile(AFileName: string);
var
  I:        integer;
  iCnt:     integer;
  sName:    string;
  pTmpItem: TQuickLaunchProfile;
begin
  with TINIFile.Create(AFileName) do begin
    iCnt := ReadInteger('QuickLaunch', 'ProfileCount', 0);
    for I := 0 to iCnt - 1 do begin
      sName := ReadString('QuickLaunch', 'ProfileName' + IntToStr(I), '');
      if sName <> '' then begin
        pTmpItem := TQuickLaunchProfile.Create;
        pTmpItem.ProfileName := sName;
        Add(pTmpItem);
        pTmpItem.CharINIFilename := ReadString('QuickLaunch', 'CharINI' + IntToStr(I), '');
        pTmpItem.DAoCCharINIFilename := ReadString('QuickLaunch', 'DAoCCharINI' + IntToStr(I), '');
        pTmpItem.UserDATFilename := ReadString('QuickLaunch', 'UserDAT' + IntToStr(I), '');
      end;  { if name }
    end;  { for I }

    Free;
  end;  { with INI }
end;

end.

unit MobFilterListFrame;

interface

uses
  Forms, Dialogs, SysUtils, Windows, Controls, StdCtrls,
  Classes, ExtCtrls, Contnrs, INIFiles, Graphics,
  RegExpr, StringParseHlprs,
{$IFDEF MSWINDOWS}
  MMSystem,
{$ENDIF}
  MobFilterEntry;

type
  TMobFilterMode = (mfmSubstring, mfmWildcard, mfmRegex);

  TMobFilterEntry = class(TObject)
  private
    FName: string;
    FAlert: string;
    FMinLevel: integer;
    FMaxLevel: integer;
    FNameLower: string;
    procedure SetName(const Value: string);
  public
    function Clone: TMobFilterEntry;

    property Alert: string read FAlert write FAlert;
    property Name: string read FName write SetName;
    property NameLower: string read FNameLower;
    property MaxLevel: integer read FMaxLevel write FMaxLevel;
    property MinLevel: integer read FMinLevel write FMinLevel;
  end;

  TMobFilterList = class(TObjectList)
  private
    FFilterMode: TMobFilterMode;
    function GetItems(I: integer): TMobFilterEntry;
  protected
    function IndexByRegex(const s: string): integer;
    function IndexByWildcard(const s: string): integer;
    function IndexBySubstring(const s: string): integer;
  public
    function Matches(const AName: string; ALevel: integer): boolean;
    function IndexByName(const s: string): integer;
    function IndexByPattern(const s: string): integer;
    function InLevelRange(idx: integer; level: integer): boolean;
    function AlertAvailable(const s: string): boolean;
    procedure AddOrReplace(AObject: TMobFilterEntry);

    procedure DoAlert(const name: string);
    procedure LoadFromINI(const AFileName: string);
    procedure SaveToINI(const AFileName: string);
    procedure CopyFrom(ASrc: TMobFilterList);

    property Items[I: integer]: TMobFilterEntry read GetItems; default;
    property FilterMode: TMobFilterMode read FFilterMode write FFilterMode;
  end;

  TfrmMobFilerList = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    lstMobFilterList: TListBox;
    pnlBottom: TPanel;
    lblAdd: TLabel;
    lblEdit: TLabel;
    lblRemove: TLabel;
    Label2: TLabel;
    procedure lblEditClick(Sender: TObject);
    procedure lblRemoveClick(Sender: TObject);
    procedure lstMobFilterListClick(Sender: TObject);
    procedure lstMobFilterListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lblAddClick(Sender: TObject);
  private
    FMobFilterList: TMobFilterList;
    FTopMost: boolean;
    FOnListModified: TNotifyEvent;

    procedure UpdateTopMost;
    function SelectedEntry: TMobFilterEntry;
    procedure SetMobFilterList(const Value: TMobFilterList);
    procedure ReloadFilterList;
    procedure DoOnListModified;
  public
    property MobFilterList: TMobFilterList read FMobFilterList write SetMobFilterList;
    property OnListModified: TNotifyEvent read FOnListModified write FOnListModified; 
  end;

implementation

resourcestring
  S_INISECTION = 'MobFilterList';

{$R *.dfm}

function TMobFilterList.GetItems(I: integer): TMobFilterEntry;
begin
  Result := TMobFilterEntry(inherited Items[I]);
end;

function TMobFilterList.Matches(const AName: string; ALevel: integer): boolean;
var
  idx:  integer;
begin
  idx := IndexByPattern(AName);
  if idx = -1 then begin
    Result := false;
    exit;
  end;

  Result := InLevelRange(idx, ALevel);
end;

function TMobFilterList.IndexByPattern(const s: string): integer;
begin
  case FFilterMode of
    mfmSubstring:
        Result := IndexBySubstring(s);
    mfmWildcard:
        Result := IndexByWildcard(s);
    mfmRegex:
        Result := IndexByRegex(s);
    else
      raise Exception.Create('Unknown filter mode!');
  end;  { case FFilterMode }
end;

function TMobFilterList.InLevelRange(idx: integer; level: integer): boolean;
begin
  Result := false;

  if (idx < 0) or (idx > Count - 1) then
    exit;

  if (level >= Items[idx].MinLevel) and (level <= Items[idx].MaxLevel) then
    Result := true;
end;

function TMobFilterList.AlertAvailable(const s: string): boolean;
var
  idx: integer;
begin
  Result := false;

  idx := IndexByPattern(s);

  if idx = -1 then
    exit;

  if (Items[idx].Alert <> '') and FileExists(Items[idx].Alert) then
    Result := true;
end;

function TMobFilterList.IndexByName(const s: string): integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = s then
      exit;
  Result := -1;
end;

procedure TMobFilterList.AddOrReplace(AObject: TMobFilterEntry);
var
  iPos: integer;
begin
  iPos := IndexByName(AObject.Name);
  if iPos <> -1 then
    Delete(iPos);

  Add(AObject);
end;

procedure TMobFilterList.DoAlert(const name: string);
var
  idx: integer;
begin
  idx := IndexByPattern(name);

  if idx = -1 then
    exit;

  PlaySound(PChar(Items[idx].Alert), 0, SND_FILENAME or SND_ASYNC or SND_NOWAIT);
end;

procedure TMobFilterList.LoadFromINI(const AFileName: string);
var
  iCnt: integer;
  I:    integer;
  AObject: TMobFilterEntry;
begin

  Clear;

  with TINIFile.Create(AFileName) do begin
    FFilterMode := TMobFilterMode(ReadInteger(S_INISECTION, 'FilterMode', 0));
    iCnt := ReadInteger(S_INISECTION, 'Count', 0);
    for I := 0 to iCnt - 1 do begin
      AObject := TMobFilterEntry.Create;
      AObject.Name := ReadString(S_INISECTION, 'MobName' + IntToStr(I), '');
      AObject.MinLevel := ReadInteger(S_INISECTION, 'MinLevel' + IntToStr(I), 0);
      AObject.MaxLevel := ReadInteger(S_INISECTION, 'MaxLevel' + IntToStr(I), 99);
      AObject.Alert := ReadString(S_INISECTION, 'Alert' + IntToStr(I), '');
      Add(AObject);
    end;

    Free;
  end;
end;

procedure TMobFilterList.SaveToINI(const AFileName: string);
var
  I: integer;
begin
  with TINIFile.Create(AFileName) do begin
    EraseSection(S_INISECTION);
    WriteInteger(S_INISECTION, 'FilterMode', ord(FFilterMode));
    WriteInteger(S_INISECTION, 'Count', Count);
    for I := 0 to Count - 1 do
      with Items[I] do begin
        WriteString(S_INISECTION, 'MobName' + IntToStr(I), Name);
        WriteInteger(S_INISECTION, 'MinLevel' + IntToStr(I), MinLevel);
        WriteInteger(S_INISECTION, 'MaxLevel' + IntToStr(I), MaxLevel);
        WriteString(S_INISECTION, 'Alert' + IntToStr(I), Alert);
      end;

    Free;
  end;  { with INIFile }
end;


{ TfrmMobFilerList }

procedure TfrmMobFilerList.ReloadFilterList;
begin
  if Assigned(FMobFilterList) then begin
    lstMobFilterList.Count := FMobFilterList.Count;
    lstMobFilterList.Height := lstMobFilterList.ItemHeight * lstMobFilterList.Count + 2;
    lstMobFilterListClick(nil);
  end
  else begin
    lstMobFilterList.Count := 0;
    lstMobFilterListClick(nil);
  end;
end;

procedure TfrmMobFilerList.UpdateTopMost;
var
  dwStyle: cardinal;
begin
  if not FTopMost then
  begin
    dwStyle := GetWindowLong((Owner as TForm).Handle, GWL_EXSTYLE);
    dwStyle := dwStyle and not WS_EX_TOPMOST;
    SetWindowLong((Owner as TForm).Handle, GWL_EXSTYLE, dwStyle);
    SetWindowPos((Owner as TForm).Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end
  else
  begin
    dwStyle := GetWindowLong((Owner as TForm).Handle, GWL_EXSTYLE);
    dwStyle := dwStyle or WS_EX_TOPMOST;
    SetWindowLong((Owner as TForm).Handle, GWL_EXSTYLE, dwStyle);
    SetWindowPos((Owner as TForm).Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
  FTopMost := Not FTopMost;
end;

function TfrmMobFilerList.SelectedEntry: TMobFilterEntry;
begin
  if Assigned(FMobFilterList) and (lstMobFilterList.ItemIndex <> -1) and
    (lstMobFilterList.ItemIndex < FMobFilterList.Count) then
    Result := FMobFilterList[lstMobFilterList.ItemIndex]
  else
    Result := nil;
end;

procedure TfrmMobFilerList.lblAddClick(Sender: TObject);
var AObject: TMobFilterEntry;
begin
  UpdateTopMost;
  with TfrmMobFilterEntry.Create(Owner) do
    try
      OKText := 'Add';

      if ShowModal = mrOk then begin
        AObject := TMobFilterEntry.Create;
        AObject.Name := MobName;
        AObject.MinLevel := MinLevel;
        AObject.MaxLevel := MaxLevel;
        if FileExists(AlertSound) then
          AObject.Alert := AlertSound
        else
          AObject.Alert := '';
        FMobFilterList.AddOrReplace(AObject);

        DoOnListModified;
      end;  { if ShowModal = mrOK }
    finally
      Free;
    end;  { with TFrmMobFilterEntry }

  UpdateTopMost;
  ReloadFilterList;
end;

procedure TfrmMobFilerList.lblEditClick(Sender: TObject);
var AObject: TMobFilterEntry;
begin
  AObject := SelectedEntry;
  if not Assigned(AObject) then
    exit;

  UpdateTopMost;
  with TfrmMobFilterEntry.Create(Owner) do
    try
      OKText := 'Edit';
      MobName := AObject.Name;
      MinLevel := AObject.MinLevel;
      MaxLevel := AObject.MaxLevel;
      AlertSound := AObject.Alert;
      if ShowModal = mrOk then begin
        AObject.Name := MobName;
        AObject.MinLevel := MinLevel;
        AObject.MaxLevel := MaxLevel;
        if FileExists(AlertSound) then
          AObject.Alert := AlertSound
        else
          AObject.Alert := '';

        DoOnListModified;
      end;  { if mrOK }
    finally
      Free;
    end;  { with TfrmMobFilterEntry }

  UpdateTopMost;
  ReloadFilterList;
end;

procedure TfrmMobFilerList.lblRemoveClick(Sender: TObject);
var
  AObject: TMobFilterEntry;
begin
  AObject := SelectedEntry;
  if not Assigned(AObject) then
    exit;

  UpdateTopMost;
  if MessageDlg('Are you sure you wish to delete the mob filter:'#13#9 +
    AObject.Name, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    FMobFilterList.Delete(lstMobFilterList.ItemIndex);
    ReloadFilterList;
    DoOnListModified;
  end;
  UpdateTopMost;
end;

procedure TfrmMobFilerList.lstMobFilterListClick(Sender: TObject);
begin
  lblEdit.Enabled := lstMobFilterList.ItemIndex <> -1;
  lblRemove.Enabled := lblEdit.Enabled;
end;

procedure TfrmMobFilerList.lstMobFilterListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  s: string;
begin
  if not Assigned(FMobFilterList) or (Index >= FMobFilterList.Count) then
    exit;

  with lstMobFilterList.Canvas do begin

    if odSelected in State then begin
      Brush.Color := clHighlight;
      Font := lstMobFilterList.Font;
      Font.Color := clHighlightText;
    end else begin
      Brush.Color := lstMobFilterList.Color;
      Font := lstMobFilterList.Font;
    end;

    FillRect(Rect);

    if FMobFilterList[Index].Alert = '' then
      s := '(none)'
    else begin
      s := Format('%.16s', [FMobFilterList[Index].Alert]);
      if length(FMobFilterList[Index].Alert) > 5 then
        s := s + '...';
    end;

    TextOut(Rect.Left + 14, Rect.Top + 2, FMobFilterList[Index].Name);
    TextOut(Rect.Left + 142, Rect.Top + 2, s);
    TextOut(Rect.Left + 262, Rect.Top + 2, IntToStr(FMobFilterList[Index].MinLevel));
    TextOut(Rect.Left + 324, Rect.Top + 2, IntToStr(FMobFilterList[Index].MaxLevel));
  end;  { with Canvas }
end;

procedure TfrmMobFilerList.SetMobFilterList(const Value: TMobFilterList);
begin
  FMobFilterList := Value;
  ReloadFilterList;
end;

procedure TfrmMobFilerList.DoOnListModified;
begin
  if Assigned(FOnListModified) then
    FOnListModified(Self);
end;

{ TMobFilterEntry }

function TMobFilterEntry.Clone: TMobFilterEntry;
begin
  Result := TMobFilterEntry.Create;
  Result.Name := Name;
  Result.Alert := Alert;
  Result.MinLevel := MinLevel;
  Result.MaxLevel := MaxLevel;
end;

procedure TMobFilterList.CopyFrom(ASrc: TMobFilterList);
var
  I:    integer;
begin
  Clear;
  for I := 0 to ASrc.Count - 1 do
    Add(ASrc[I].Clone);
end;

procedure TMobFilterEntry.SetName(const Value: string);
begin
  FName := Value;
  FNameLower := LowerCase(Value);
end;

function TMobFilterList.IndexByRegex(const s: string): integer;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  r.ModifierI := true;

  for Result := 0 to Count -1 do begin
    r.Expression := Items[Result].Name;
    if r.Exec(s) then begin
      r.Free;
      exit;
    end;
  end;

  r.Free;
  Result := -1;
end;

function TMobFilterList.IndexBySubstring(const s: string): integer;
var
  lows: string;
begin
  lows := LowerCase(s);
  for Result := 0 to Count - 1 do
    if Pos(Items[Result].NameLower, lows) > 0 then
      exit;

  Result := -1;
end;

function TMobFilterList.IndexByWildcard(const s: string): integer;
var
  lows: string;
begin
  lows := LowerCase(s);
  for Result := 0 to Count - 1 do
    if WildMatch(Items[Result].NameLower, lows) then
      exit;

  Result := -1;
end;

end.

unit StreamINI;

interface

uses
  SysUtils, Classes, LinedFileStream;

type
  TStreamINISection = class(TStringList)
  private
    FName: string;
  public
    function ReadString(const AValue, ADefault: string) : string;
    function ReadInteger(const AValue: string; ADefault: integer) : integer;
    function ReadBool(const AValue: string; ADefault: boolean) : boolean;

    property Name: string read FName;
  end;

  TStreamINIFile = class(TObject)
  private
    function GetSectionCount: integer;
    function GetSections(I: integer): TStreamINISection;
  protected
    FSections:    TList;
    FLastSection: TStreamINISection;
  public
    constructor Create(ASrc: TStream);
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(ASrc: TStream);

    function FindSection(const ASection: string) : TStreamINISection;
    function ReadString(const ASection, AValue, ADefault: string) : string;
    function ReadInteger(const ASection, AValue: string; ADefault: integer) : integer;
    function ReadBool(const ASection, AValue: string; ADefault: boolean) : boolean;
    property Sections[I: integer]: TStreamINISection read GetSections;
    property SectionCount: integer read GetSectionCount;
  end;

implementation

{ TStreamINIFile }

procedure TStreamINIFile.Clear;
var
  I:    integer;
begin
  FLastSection := nil;

  for I := 0 to FSections.Count - 1 do
    Sections[I].Free;
  FSections.Clear;
end;

constructor TStreamINIFile.Create(ASrc: TStream);
begin
  FSections := TList.Create;

  if Assigned(ASrc) then
    LoadFromStream(ASrc);
end;

destructor TStreamINIFile.Destroy;
begin
  Clear;
  FSections.Free;
  inherited;
end;

function TStreamINIFile.FindSection(const ASection: string): TStreamINISection;
var
  I:    integer;
begin
  if Assigned(FLastSection) and AnsiSameText(ASection, FLastSection.Name) then
    Result := FLastSection

  else begin
    for I := 0 to FSections.Count - 1 do
      if AnsiSameText(ASection, Sections[I].Name) then begin
        Result := Sections[I];
        FLastSection := Result;
        exit;
      end;
    Result := nil;
  end;
end;

function TStreamINIFile.GetSectionCount: integer;
begin
  Result := FSections.Count;
end;

function TStreamINIFile.GetSections(I: integer): TStreamINISection;
begin
  Result := TStreamINISection(FSections[I]);
end;

procedure TStreamINIFile.LoadFromStream(ASrc: TStream);
var
  strmWrapper: TLinedStreamWrapper;
  sLine:  string;
  iPos:   integer;
  pCurrentSection:  TStreamINISection;
  sName:  string;
begin
  Clear;
  strmWrapper := TLinedStreamWrapper.Create(ASrc);

  pCurrentSection := nil;
  while not strmWrapper.EOF do begin
    sLine := Trim(strmWrapper.ReadLn);
      { look for comments }
    iPos := Pos(';', sLine);
    if iPos = 1 then
      continue;
    if iPos > 0 then begin
      Delete(sLine, iPos, Length(sLine));
      sLine := TrimRight(sLine);
    end;

      { blank lines are ignored }
    if sLine = '' then
      continue;

    if sLine[1] = '[' then begin
      iPos := Pos(']', sLine);
      if iPos = 0 then
        continue;
      sName := Trim(copy(sLine, 2, iPos - 2));

      pCurrentSection := TStreamINISection.Create;
      pCurrentSection.FName := sName;
      FSections.Add(pCurrentSection);
    end  { if new section }

      { standard line, not null, comment removed, no spaces at front }
    else begin
        { we have to have a section to put a key/value in }
      if not Assigned(pCurrentSection) then
        continue;

      iPos := Pos('=', sLine);
      if iPos = 0 then
        continue;

      sName := TrimRight(copy(sLine, 1, iPos - 1));
      iPos := pCurrentSection.IndexOfName(sName);
        { we only add if we haven't seen the name yet }
      if iPos = -1 then
        pCurrentSection.Add(sLine);
    end;
  end;

  strmWrapper.Free;
end;

function TStreamINIFile.ReadBool(const ASection, AValue: string;
  ADefault: boolean): boolean;
var
  pSection:   TStreamINISection;
begin
  pSection := FindSection(ASection);
  if not Assigned(pSection) then
    Result := ADefault
  else
    Result := pSection.ReadBool(AValue, ADefault);
end;

function TStreamINIFile.ReadInteger(const ASection, AValue: string;
  ADefault: integer): integer;
var
  pSection:   TStreamINISection;
begin
  pSection := FindSection(ASection);
  if not Assigned(pSection) then
    Result := ADefault
  else
    Result := pSection.ReadInteger(AValue, ADefault);
end;

function TStreamINIFile.ReadString(const ASection, AValue, ADefault: string): string;
var
  pSection:   TStreamINISection;
begin
  pSection := FindSection(ASection);
  if not Assigned(pSection) then
    Result := ADefault
  else
    Result := pSection.ReadString(AValue, ADefault);
end;

{ TStreamINISection }

function TStreamINISection.ReadBool(const AValue: string; ADefault: boolean): boolean;
begin
  Result := StrToIntDef(ReadString(AValue, ''), ord(ADefault)) <> 0;
end;

function TStreamINISection.ReadInteger(const AValue: string; ADefault: integer): integer;
begin
  Result := StrToIntDef(ReadString(AValue, ''), ADefault);
end;

function TStreamINISection.ReadString(const AValue, ADefault: string): string;
begin
  Result := Values[AValue];
  if Result = '' then
    Result := ADefault;
end;

end.

unit CSVLineParser;

interface

uses
  SysUtils, Classes;

type
  TCSVLineParser = class(TObject)
  private
    FFields:    TStringList;
    FDelimiter: char;
    FDataString: string;

    procedure ReparseString;
    procedure SetDataString(const Value: string);
    procedure SetDelimiter(const Value: char);
    function GetFields(I: integer): string;
    function GetFieldCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function FieldAsInt(AFldNum, ADefault: integer) : integer;

    property Delimiter: char read FDelimiter write SetDelimiter;
    property DataString: string read FDataString write SetDataString;
    property FieldCount: integer read GetFieldCount;
    property Fields[I: integer]: string read GetFields; default;
    property FieldCount: integer read GetFieldCount;
  end;

implementation

{ TCSVLineParser }

constructor TCSVLineParser.Create;
begin
  FFields := TStringList.Create;
  FDelimiter := ',';
end;

destructor TCSVLineParser.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TCSVLineParser.FieldAsInt(AFldNum, ADefault: integer): integer;
begin
  Result := StrToIntDef(Trim(FFields[AFldNum]), ADefault);
end;

function TCSVLineParser.GetFieldCount: integer;
begin
  Result := FFields.Count;
end;

function TCSVLineParser.GetFields(I: integer): string;
begin
  Result := FFields[I];
end;

procedure TCSVLineParser.ReparseString;
var
  bInQuotes:  boolean;
  P:    PChar;
  pStart: PChar;
  s:    string;
begin
  FFields.Clear;

  bInQuotes := false;
  P := PChar(FDataString);
  pStart := P;

  while P^ <> #0 do begin
    if P^ = '"' then
      bInQuotes := not bInQuotes
    else if not bInQuotes and (P^ = FDelimiter) then begin
      SetString(s, pStart, P - pStart);
      FFields.Add(s);
      pStart := P + 1;
    end;

    inc(P);
  end;  { while P^ }

  if pStart <> P then begin
    SetString(s, pStart, P - pStart);
    FFields.Add(s);
  end;
end;

procedure TCSVLineParser.SetDataString(const Value: string);
begin
  FDataString := Value;
  ReparseString;
end;

procedure TCSVLineParser.SetDelimiter(const Value: char);
begin
  FDelimiter := Value;
  ReparseString;
end;

end.

unit DAOCPlayerAttributes;

interface

uses
  SysUtils, Contnrs;
  
type
  TDAOCNameValuePair = class(TObject)
  private
    FValue:     integer;
    FLastValue: integer;
    FName: string;
    function GetModified: boolean;
    procedure SetValue(const AValue: integer);
  public
    property Name: string read FName write FName;
    property Value: integer read FValue write SetValue;
    property Modified: boolean read GetModified;
  end;

  TNameValueModifiedNotify = procedure(Sender: TObject; AItem: TDAOCNameValuePair) of object;

  TDAOCNameValueList = class(TObjectList)
  private
    function GetItems(AIndex: integer): TDAOCNameValuePair;
  public
    function Find(const AName: string) : TDAOCNameValuePair;
    function FindOrAdd(const AName: string) : TDAOCNameValuePair;

    property Items[AIndex: integer]: TDAOCNameValuePair read GetItems; default;
  end;

implementation

{ TDAOCNameValuePair }

function TDAOCNameValuePair.GetModified: boolean;
begin
  Result := FLastValue <> FValue;
end;

procedure TDAOCNameValuePair.SetValue(const AValue: integer);
begin
  FLastValue := FValue;
  FValue := AValue;
end;

{ TDAOCNameValueList }

function TDAOCNameValueList.Find(const AName: string): TDAOCNameValuePair;
var
  I:  integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AName, Items[I].Name) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TDAOCNameValueList.FindOrAdd(const AName: string): TDAOCNameValuePair;
begin
  Result := Find(AName);

  if not Assigned(Result) then begin
    Result := TDAOCNameValuePair.Create;
    Result.Name := AName;
    Add(Result);
  end;
end;

function TDAOCNameValueList.GetItems(AIndex: integer): TDAOCNameValuePair;
begin
  Result := TDAOCNameValuePair(inherited Items[AIndex]);
end;

end.

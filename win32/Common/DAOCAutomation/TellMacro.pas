unit TellMacro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComObj, ActiveX, DAOCSkilla_TLB,
  AXScript, ScriptSiteImpl, DAOCControl;

type
  TfrmTellMacro = class(TForm)
    edtScriptFile: TEdit;
    btnLoad: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDControl:    TDAOCControl;

    FScriptSite:  TAXScriptSite;
    FIScriptSite: IActiveScriptSite;
    FScript:      IActiveScript;
    FExceptInfo:  TExcepInfo;
    FScriptResult:  OleVariant;

    procedure Log(const S: String);
    procedure ClearScript;

    procedure SSOnTerminate(Sender: TObject);
    procedure SSOnStateChange(Sender: TObject);
    procedure SSOnError(Sender: TObject; const AErr, ALine: string);
    procedure SSOnEnter(Sender: TObject);
    procedure SSOnLeave(Sender: TObject);
    procedure SSObjNeeded(Sender: TObject; const AName: string; out IObj: IUnknown);
    procedure SSTInfoNeeded(Sender: TObject; const AName: string; out ITyp: ITypeInfo);
    procedure SetDControl(const Value: TDAOCControl);
    function GetFilename: string;
    procedure SetFilename(const Value: string);
  public
    property DAOCControl: TDAOCControl read FDControl write SetDControl;
    property Filename: string read GetFilename write SetFilename;
  end;

var
  frmTellMacro: TfrmTellMacro;

implementation

uses
  Unit1;

{$R *.dfm}

{ TfrmTellMacro }

procedure TfrmTellMacro.Log(const S: String);
begin
  frmMain.Log(s);
end;

procedure TfrmTellMacro.FormCreate(Sender: TObject);
begin
  FScriptSite := TAXScriptSite.Create;
  FIScriptSite := FScriptSite as IActiveScriptSite;  // add a refcount for us
  FScriptSite.OnTerminate := SSOnTerminate;
  FScriptSite.OnStateChange := SSOnStateChange;
  FScriptSite.OnError := SSOnError;
  FScriptSite.OnEnter := SSOnEnter;
  FScriptSite.OnLeave := SSOnLeave;
  FScriptSite.OnObjectNeeded := SSObjNeeded;
  FScriptSite.OnTypeInfoNeeded := SSTInfoNeeded;
end;

procedure TfrmTellMacro.FormDestroy(Sender: TObject);
begin
  FIScriptSite := nil;
  FScriptSite := nil;  // the object is freed by the above release()
end;

procedure TfrmTellMacro.btnLoadClick(Sender: TObject);
var
  wsFileName: WideString;
  pParse: IActiveScriptParse;
  fs:   TFileStream;
  ss:   TStringStream;
  wsScript: WideString;
begin
  ClearScript;
  wsFileName := GetFilename;

  fs := TFileStream.Create(GetFilename, fmOpenRead or fmShareDenyNone);
  ss := TStringStream.Create('');
  ss.CopyFrom(fs, 0);
  fs.Free;
  wsScript := ss.DataString;
  ss.Free;

  if ExtractFileExt(edtScriptFile.Text) = '.js' then
    FScript := CreateComObject(ProgIDToClassID('JScript')) as IActiveScript
  else
    FScript := CreateComObject(ProgIDToClassID('VBScript')) as IActiveScript;

  OLECheck(FScript.SetScriptSite(FScriptSite as IActiveScriptSite));
  pParse := FScript as IActiveScriptParse;
  OLECheck(pParse.InitNew);

  OLECheck(FScript.AddNamedItem('DAOCControl', SCRIPTITEM_ISVISIBLE or SCRIPTITEM_ISSOURCE));
  OLECheck(pParse.ParseScriptText(PWideChar(wsScript), 'DAOCControl', nil, nil, 0, 0, 0,
    FScriptResult, FExceptInfo));

  OLECheck(FScript.SetScriptState(SCRIPTSTATE_CONNECTED));
end;

procedure TfrmTellMacro.SSOnEnter(Sender: TObject);
begin
  Log('SSEnter');
end;

procedure TfrmTellMacro.SSOnError(Sender: TObject; const AErr, ALine: string);
begin
  Log('SSError: ' + AErr + #13#10 + ALine);
end;

procedure TfrmTellMacro.SSOnLeave(Sender: TObject);
begin
  Log('SSLeave');
end;

procedure TfrmTellMacro.SSOnStateChange(Sender: TObject);
begin
  Log('SSStateChange');
end;

procedure TfrmTellMacro.SSOnTerminate(Sender: TObject);
begin
  Log('SSTerminate');
end;

procedure TfrmTellMacro.ClearScript;
begin
  if Assigned(FScript) then begin
    FScript.Close;
    FScript := nil;
  end;
end;

procedure TfrmTellMacro.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearScript;
end;

procedure TfrmTellMacro.SSObjNeeded(Sender: TObject; const AName: string;
  out IObj: IUnknown);
begin
  Log('SSObjNeeded: ' + AName);

  if AnsiSameText(AName, 'DAOCControl') then
    IObj := FDControl as IUnknown;
end;

procedure TfrmTellMacro.SSTInfoNeeded(Sender: TObject; const AName: string;
  out ITyp: ITypeInfo);
begin
  Log('SSTInfoNeeded: ' + AName);

  if AnsiSameText(AName, 'DAOCControl') then
    ITyp := FDControl.ClassTypeInfo;
end;

procedure TfrmTellMacro.SetDControl(const Value: TDAOCControl);
begin
  FDControl := Value;
end;

function TfrmTellMacro.GetFilename: string;
begin
  Result := edtScriptFile.Text;
end;

procedure TfrmTellMacro.SetFilename(const Value: string);
begin
  edtScriptFile.Text := Value;
end;

end.

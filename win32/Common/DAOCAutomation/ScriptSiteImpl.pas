unit ScriptSiteImpl;

interface

uses
  Windows, Classes, ActiveX, AXScript;

type
  TScriptErrorEvent = procedure (Sender: TObject; const AErr, ALine: string; APos: TPoint) of object;
  TScriptObjectNeededEvent = procedure (Sender: TObject; const AName: string;
    out IObj: IUnknown) of Object;
  TScriptTypeInfoNeededEvent = procedure (Sender: TObject; const AName: string;
    out ITyp: ITypeInfo) of Object;

  TAXScriptSite = class(TInterfacedObject, IActiveScriptSite)
  private
    FOnLeave: TNotifyEvent;
    FOnError: TScriptErrorEvent;
    FOnEnter: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnObjectNeeded: TScriptObjectNeededEvent;
    FOnTypeInfoNeeded: TScriptTypeInfoNeededEvent;
  protected
    function GetLCID(out Lcid: TLCID): HRESULT; stdcall;
    function GetItemInfo(const pstrName: POleStr; dwReturnMask: DWORD;
      out ppiunkItem: IUnknown; out Info: ITypeInfo): HRESULT; stdcall;
    function GetDocVersionString(out Version: WideString): HRESULT; stdcall;
    function OnScriptTerminate(const pvarResult: OleVariant; const pexcepinfo: TExcepInfo): HRESULT; stdcall;
    function IActiveScriptSite.OnStateChange = IASS_OnStateChange;
    function IASS_OnStateChange(ScriptState: TScriptState): HRESULT; stdcall;
    function OnScriptError(const pscripterror: IActiveScriptError): HRESULT; stdcall;
    function OnEnterScript: HRESULT; stdcall;
    function OnLeaveScript: HRESULT; stdcall;
  public
    destructor Destroy; override;

    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnError: TScriptErrorEvent read FOnError write FOnError;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave: TNotifyEvent read FOnLeave write FOnLeave;
    property OnObjectNeeded: TScriptObjectNeededEvent read FOnObjectNeeded write FOnObjectNeeded;
    property OnTypeInfoNeeded: TScriptTypeInfoNeededEvent read FOnTypeInfoNeeded write FOnTypeInfoNeeded;
  end;

implementation

{ TAXScriptSite }

function TAXScriptSite.GetDocVersionString(out Version: WideString): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAXScriptSite.GetItemInfo(const pstrName: POleStr;
  dwReturnMask: DWORD; out ppiunkItem: IUnknown; out Info: ITypeInfo): HRESULT;
var
  sObjName: string;
begin
  sObjName := string(pstrName);
  Result := S_OK;

  if dwReturnMask and SCRIPTINFO_ITYPEINFO <> 0 then begin
    Pointer(Info) := nil;
    if Assigned(FOnTypeInfoNeeded) then
      FOnTypeInfoNeeded(Self, sObjName, Info);
  end;

  if dwReturnMask and SCRIPTINFO_IUNKNOWN <> 0 then begin
    Pointer(ppiunkItem) := nil;
    if Assigned(FOnObjectNeeded) then
      FOnObjectNeeded(Self, sObjName, ppiunkItem);
  end;
end;

function TAXScriptSite.GetLCID(out Lcid: TLCID): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TAXScriptSite.OnEnterScript: HRESULT;
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
  Result := S_OK;
end;

function TAXScriptSite.OnLeaveScript: HRESULT;
begin
  if Assigned(FOnLeave) then
    FOnLeave(Self);
  Result := S_OK;
end;

function TAXScriptSite.OnScriptError(const pscripterror: IActiveScriptError): HRESULT;
var
  ExcepInfo:  TExcepInfo;
  pwsLine:    PWideChar;
  sErr:       string;
  sLine:      string;
  dwContext:  DWORD;
  dwLine:     DWORD;
  iCharPos:   integer;
  pt:         TPoint;
begin
  if SUCCEEDED(pscripterror.GetExceptionInfo(ExcepInfo)) then
    sErr := ExcepInfo.bstrDescription
  else
    sErr := '';
  if SUCCEEDED(pscripterror.GetSourceLineText(pwsLine)) then
    sLine := WideCharToString(pwsLine)
  else
    sLine := '';
  if SUCCEEDED(pscripterror.GetSourcePosition(dwContext, dwLine, iCharPos)) then begin
    pt.X := iCharPos;
    pt.Y := dwLine;
  end
  else begin
    pt.X := 0;
    pt.Y := 0;
  end;

  if Assigned(FOnError) then
    FOnError(Self, sErr, sLine, pt);
    
  Result := S_OK;
end;

function TAXScriptSite.OnScriptTerminate(const pvarResult: OleVariant;
  const pexcepinfo: TExcepInfo): HRESULT;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
  Result := S_OK;
end;

function TAXScriptSite.IASS_OnStateChange(ScriptState: TScriptState): HRESULT;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
  Result := S_OK;
end;

destructor TAXScriptSite.Destroy;
begin
  inherited Destroy;
end;

end.

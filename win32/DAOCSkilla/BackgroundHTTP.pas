unit BackgroundHTTP;

interface

uses
{$IFDEF LINUX}
  QControls,
{$ELSE}
  Windows, Messages,
{$ENDIF !LINUX}
  SysUtils, Classes, SyncObjs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP;

type
  TBackgroundHTTPRequest = class;

  THTTPErrorEvent = procedure (const AErr: string; AResponse: TBackgroundHTTPRequest) of object;
  THTTPCompletionEvent = procedure (AResponse: TBackgroundHTTPRequest) of object;

  TBackgroundHTTPRequest = class(TObject)
  private
    FTag: integer;
    FURL: string;
    FResponseStream:  TStream;
    FMethod: TIdHTTPMethod;
    FOnHTTPError: THTTPErrorEvent;
    FOnRequestComplete: THTTPCompletionEvent;
    FExtraHeaders:    TStrings;

    procedure DoOnRequestComplete;
    procedure DoOnHTTPError(const AErr: string);
  public
    constructor Create;
    destructor Destroy; override;

    property ExtraHeaders: TStrings read FExtraHeaders;
    property Method: TIdHTTPMethod read FMethod write FMethod;
    property URL: string read FURL write FURL;
    property ResponseStream: TStream read FResponseStream write FResponseStream;
    property Tag: integer read FTag write FTag;
    property OnHTTPError: THTTPErrorEvent read FOnHTTPError write FOnHTTPError;
    property OnRequestComplete: THTTPCompletionEvent read FOnRequestComplete write FOnRequestComplete;
  end;

  TBackgroundHTTPRequestList = class(TList)
  private
    FListLock:    TCriticalSection;
    function GetItems(I: integer): TBackgroundHTTPRequest;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
    procedure Clear; override;
    function GetFirst: TBackgroundHTTPRequest;

    property Items[I: integer]: TBackgroundHTTPRequest read GetItems; default;
  end;

  TBackgroundHTTPManager = class(TThread)
  private
    FRequestList:   TBackgroundHTTPRequestList;
    FIdHTTP:        TIdHTTP;
    FRequestEvent:  TEvent;
    FNotifyWnd:     THANDLE;
  protected
    procedure SetAgentVersion(const AValue: string);
    procedure ProcessRequests;
    procedure Execute; override;
{$IFDEF LINUX}
{$ELSE}
    procedure NotifyProc(var Msg: TMessage);
{$ENDIF !LINUX}
    procedure DoOnRequestCompleteMainThread(ARequest: TBackgroundHTTPRequest);
    procedure DoOnHTTPComplete(ARequest: TBackgroundHTTPRequest);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Shutdown;
    procedure Request(ARequest: TBackgroundHTTPRequest);
    procedure ClearRequests;
    
    property AgentVersion: string write SetAgentVersion;
  end;

implementation

const
{$IFDEF LINUX}
  WM_REQUEST_COMPLETE = CM_BASE - 1;
{$ELSE}
  WM_REQUEST_COMPLETE = WM_USER + 1;
{$ENDIF !LINUX}

{ TBackgroundHTTPRequestList }

procedure TBackgroundHTTPRequestList.Clear;
var
  I:    integer;
begin
  Lock;
  try
    for I := 0 to Count - 1 do
      Items[I].Free;

    inherited;
  finally
    Unlock;
  end;
end;

constructor TBackgroundHTTPRequestList.Create;
begin
  inherited;
  FListLock := TCriticalSection.Create;
end;

destructor TBackgroundHTTPRequestList.Destroy;
begin
  inherited;
  FListLock.Free;
end;

function TBackgroundHTTPRequestList.GetFirst: TBackgroundHTTPRequest;
begin
  Lock;
  try
    if Count > 0 then begin
      Result := Items[0];
      Delete(0);
    end
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

function TBackgroundHTTPRequestList.GetItems(I: integer): TBackgroundHTTPRequest;
begin
  Result := TBackgroundHTTPRequest(inherited Items[I]);
end;

procedure TBackgroundHTTPRequestList.Lock;
begin
  FListLock.Enter;
end;

procedure TBackgroundHTTPRequestList.Unlock;
begin
  FListLock.Leave;
end;

{ TBackgroundHTTPManager }

procedure TBackgroundHTTPManager.ClearRequests;
begin
  FRequestList.Clear;
end;

constructor TBackgroundHTTPManager.Create;
begin
  FIdHTTP := TIdHTTP.Create(nil);
  FRequestList := TBackgroundHTTPRequestList.Create;
  FRequestEvent := TEvent.Create(nil, false, false, '');
  FNotifyWnd := AllocateHWnd(NotifyProc);

  SetAgentVersion('');
  
  inherited Create(false);
end;

destructor TBackgroundHTTPManager.Destroy;
begin
  ClearRequests;
  FRequestList.Free;
  FRequestEvent.Free;
  FIdHTTP.Free;
  DeallocateHWnd(FNotifyWnd);

  inherited;
end;

procedure TBackgroundHTTPManager.DoOnHTTPComplete(ARequest: TBackgroundHTTPRequest);
begin
  if Terminated then
    ARequest.Free
  else
    PostMessage(FNotifyWnd, WM_REQUEST_COMPLETE, 0, LPARAM(ARequest));
end;

procedure TBackgroundHTTPManager.DoOnRequestCompleteMainThread(
  ARequest: TBackgroundHTTPRequest);
begin
  ARequest.DoOnRequestComplete;
  ARequest.Free;
end;

procedure TBackgroundHTTPManager.Execute;
begin
  while not Terminated do begin
    FRequestEvent.WaitFor(INFINITE);
    if not Terminated then
      ProcessRequests;
  end;
end;

procedure TBackgroundHTTPManager.NotifyProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_REQUEST_COMPLETE:
      DoOnRequestCompleteMainThread(TBackgroundHTTPRequest(Msg.LParam));
  end;
end;

procedure TBackgroundHTTPManager.ProcessRequests;
var
  pRequest:  TBackgroundHTTPRequest;
begin
  while not Terminated do begin
    pRequest := FRequestList.GetFirst;
    if not Assigned(pRequest) then
      exit;

    try
      FIdHTTP.Request.ExtraHeaders.Assign(pRequest.ExtraHeaders);
      FIdHTTP.DoRequest(pRequest.Method, pRequest.URL, nil, pRequest.ResponseStream);
      DoOnHTTPComplete(pRequest);
    except
      on E: Exception do begin
        pRequest.DoOnHTTPError(E.Message);
        pRequest.Free;
      end;
    end;
  end;  { while !Terminated }
end;

procedure TBackgroundHTTPManager.Request(ARequest: TBackgroundHTTPRequest);
begin
  FRequestList.Add(ARequest);
  FRequestEvent.SetEvent;
end;

procedure TBackgroundHTTPManager.SetAgentVersion(const AValue: string);
begin
  if AValue <> '' then
    FIdHTTP.Request.UserAgent := 'Mozilla/3.0 (compatible; DaocSkilla ' + AValue + ')'
  else
    FIdHTTP.Request.UserAgent := 'Mozilla/3.0 (compatible; DaocSkilla)';
end;

procedure TBackgroundHTTPManager.Shutdown;
begin
  Terminate;
  FRequestEvent.SetEvent;
  WaitFor;
end;

{ TBackgroundHTTPRequest }

constructor TBackgroundHTTPRequest.Create;
begin
  inherited;
  FExtraHeaders := TStringList.Create;
  FMethod := hmGet;
end;

destructor TBackgroundHTTPRequest.Destroy;
begin
  FExtraHeaders.Free;
  FResponseStream.Free;
  inherited;
end;

procedure TBackgroundHTTPRequest.DoOnHTTPError(const AErr: string);
begin
  if Assigned(FOnHTTPError) then
    FOnHTTPError(AErr, Self);
end;

procedure TBackgroundHTTPRequest.DoOnRequestComplete;
begin
  if Assigned(FOnRequestComplete) then
    FOnRequestComplete(Self);
end;

end.

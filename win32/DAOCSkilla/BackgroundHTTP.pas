unit BackgroundHTTP;

interface

uses
  Windows, Messages, SysUtils, Classes, SyncObjs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP;

type
  TBackgroundHTTPRequest = class(TObject)
  private
    FTag: integer;
    FURL: string;
    FResponseStream: TStream;
    FMethod: TIdHTTPMethod;
  public
    constructor CreateGET;
    destructor Destroy; override;
    
    property Method: TIdHTTPMethod read FMethod write FMethod;
    property URL: string read FURL write FURL;
    property ResponseStream: TStream read FResponseStream write FResponseStream;
    property Tag: integer read FTag write FTag;
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

  THTTPErrorEvent = procedure (const AErr: string; AResponse: TBackgroundHTTPRequest) of object;
  THTTPCompletionEvent = procedure (AResponse: TBackgroundHTTPRequest) of object;

  TBackgroundHTTPManager = class(TThread)
  private
    FRequestList:   TBackgroundHTTPRequestList;
    FIdHTTP:        TIdHTTP;
    FRequestEvent:  THANDLE;
    FOnHTTPError: THTTPErrorEvent;
    FNotifyWnd:     THANDLE;
    FOnRequestComplete: THTTPCompletionEvent;
  protected
    procedure ProcessRequests;
    procedure Execute; override;
    procedure DoOnHTTPError(const AErr: string; ARequest: TBackgroundHTTPRequest);
    procedure DoOnHTTPComplete(ARequest: TBackgroundHTTPRequest);
    procedure NotifyProc(var Msg: TMessage);
    procedure DoOnRequestCompleteMainThread(ARequest: TBackgroundHTTPRequest);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Shutdown;
    procedure Request(ARequest: TBackgroundHTTPRequest);
    procedure ClearRequests;

    property OnHTTPError: THTTPErrorEvent read FOnHTTPError write FOnHTTPError;
    property OnRequestComplete: THTTPCompletionEvent read FOnRequestComplete write FOnRequestComplete; 
  end;

implementation

const
  WM_REQUEST_COMPLETE = WM_USER + 1;

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
  FIdHTTP.Request.UserAgent := 'Mozilla/3.0 (compatible; DaocSkilla)';
  FRequestList := TBackgroundHTTPRequestList.Create;
  FRequestEvent := CreateEvent(nil, false, false, nil);
  FNotifyWnd := AllocateHWnd(NotifyProc);

  inherited Create(false);
end;

destructor TBackgroundHTTPManager.Destroy;
begin
  ClearRequests;
  FRequestList.Free;
  CloseHandle(FRequestEvent);
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

procedure TBackgroundHTTPManager.DoOnHTTPError(const AErr: string;
  ARequest: TBackgroundHTTPRequest);
begin
  if Assigned(FOnHTTPError) then
    FOnHTTPError(AErr, ARequest);
end;

procedure TBackgroundHTTPManager.DoOnRequestCompleteMainThread(
  ARequest: TBackgroundHTTPRequest);
begin
  if Assigned(FOnRequestComplete) then
    FOnRequestComplete(ARequest);
  ARequest.Free;
end;

procedure TBackgroundHTTPManager.Execute;
begin
  while not Terminated do begin
    WaitForSingleObject(FRequestEvent, INFINITE);
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
      FIdHTTP.DoRequest(pRequest.Method, pRequest.URL, nil, pRequest.ResponseStream);
      DoOnHTTPComplete(pRequest);
    except
      on E: Exception do begin
        DoOnHTTPError(E.Message, pRequest);
        pRequest.Free;
      end;
    end;
  end;  { while !Terminated }
end;

procedure TBackgroundHTTPManager.Request(ARequest: TBackgroundHTTPRequest);
begin
  FRequestList.Add(ARequest);
  SetEvent(FRequestEvent);
end;

procedure TBackgroundHTTPManager.Shutdown;
begin
  Terminate;
  SetEvent(FRequestEvent);
  WaitFor;
end;

{ TBackgroundHTTPRequest }

constructor TBackgroundHTTPRequest.CreateGET;
begin
  inherited;
  FMethod := hmGet;
end;

destructor TBackgroundHTTPRequest.Destroy;
begin
  FResponseStream.Free;
  inherited;
end;

end.
program login;

uses
  SysUtils,
  Windows;

procedure LaunchGameDLL;
var
  sLastDir:   string;
  sCmdLine:   string;
  sDAOCPath:  string;
  I:    integer;
  pi:   TProcessInformation;
  si:   TStartupInfo;
begin
  if ParamCount < 7 then
    exit;

  sDAOCPath := ExtractFilePath(ParamStr(1));
  sCmdLine := ExtractFileName(ParamStr(1));
  for I := 2 to 7 do
    sCmdLine := sCmdLine + ' ' + ParamStr(I);

  //MessageBox(0, PChar(sCmdLine), Pchar(sDaocPath), MB_OK);
  //exit;

  sLastDir := GetCurrentDir;
  if not SetCurrentDir(sDAOCPath) then
    exit;

  try
    FillChar(si, sizeof(si), 0);
    si.cb := sizeof(si);
    FillChar(pi, sizeof(pi), 0);
    if not CreateProcess(nil, PChar(sCmdLine),
      nil, nil, false, CREATE_SUSPENDED, nil, nil, si, pi) then begin
      MessageBox(0, PChar('Error ' + IntToStr(GetLastError) + ' launching'), 'QuickLaunch', MB_OK);
      exit;
    end;

    ResumeThread(pi.hThread);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

      { wait for the other process to start to find us }
    sleep(10000);
  finally
     SetCurrentDir(sLastDir);
  end;
end;

begin
  LaunchGameDLL;
end.
 
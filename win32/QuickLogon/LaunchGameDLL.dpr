program LaunchGameDLL;

uses
  Windows,
  SysUtils,
  PEFuncs in 'PEFuncs.pas',
  BoyerMoore in 'BoyerMoore.pas';

var
  pi:   TProcessInformation;

function SearchSegment(dwImageBase, dwSegmentOffset, dwSegmentLen: DWORD;
  const AKey: string) : Pointer;
var
  boymoo:   TBaseBoyMooSearch;
  pScratch: PChar;
  dwBytesRead:  DWORD;
  iOffset:  integer;
begin
  Result := nil;
  boymoo := TBaseBoyMooSearch.Create;
  GetMem(pScratch, dwSegmentLen);
  try
    ReadProcessMemory(pi.hProcess, Pointer(dwImageBase + dwSegmentOffset), pScratch,
      dwSegmentLen, dwBytesRead);
    if dwBytesRead <> dwSegmentLen then
      exit;

    boymoo.SetData(pScratch, dwSegmentLen);
    boymoo.Pattern := AKey;
    iOffset := boymoo.FindFirst;

    if iOffset = BOYERMOORE_POSITION_EOF then
      exit;

    Result := Pointer(dwImageBase + dwSegmentOffset + DWORD(iOffset));
  finally
    FreeMem(pScratch);
    boymoo.Free;
  end;
end;

function DoWriteMemory(ADest: Pointer; const ANewData: string) : boolean;
var
  OldMemProtect:  DWORD;
  dwBytesWritten: DWORD;
begin
  Result := false;
  if not VirtualProtectEx(pi.hProcess, ADest, Length(ANewData), PAGE_EXECUTE_READWRITE, OldMemProtect) then
    exit;

  dwBytesWritten := 0;
  WriteProcessMemory(pi.hProcess, ADest, PChar(ANewData), Length(ANewData), dwBytesWritten);
  Result := DWORD(Length(ANewData)) = dwBytesWritten;

  VirtualProtectEx(pi.hProcess, ADest, Length(ANewData), OldMemProtect, OldMemProtect);
end;

function PatchWINDOWEDLoc(pe:  TPortableExecutable) : boolean;
var
  pWindowedStr:   Pointer;
  sWindowedAddr:  array[0..4] of char absolute pWindowedStr;
  pCode:    Pointer;
  sCode:  string;
begin
  Result := false;
  pWindowedStr := SearchSegment(pe.ImageBase,
    pe.ObjectTable.FindSegment('.data').RVA,
    pe.ObjectTable.FindSegment('.data').PhysicalSize,
    'windowed'#0);

  if not Assigned(pWindowedStr) then begin
    MessageBox(0, 'Could not find "windowed" in dseg!', 'Error', 0);
    exit;
  end;

    { push aWindowed }
  sCode := #$68 + sWindowedAddr[0] + sWindowedAddr[1] + sWindowedAddr[2] + sWindowedAddr[3];

  pCode := SearchSegment(pe.ImageBase,
    pe.ObjectTable.FindSegment('.text').RVA,
    pe.ObjectTable.FindSegment('.text').PhysicalSize,
    sCode);

  if not Assigned(pCode) then begin
    MessageBox(0, 'Could not find "windowed" reference in cseg!', 'Error', 0);
    exit;
  end;

    { pretend we read windowed=1 from the user.dat file }
  sCode := #$58 +  // pop eax
    #$C7#$00#$01#$00#$00#$00 + // mov [eax], (dword)1
    #$90#$90#$90 + // nop (was push offset "main")
    #$90#$90#$90#$90#$90 +  // nop (was push offset "user.dat")
    #$90#$90#$90#$90#$90 +  // nop (was call readsub)
    #$83#$c4#$0c;           // add esp, 0x0c  (was add esp, 0x18)
  if not DoWriteMemory(pCode, sCode) then begin
    MessageBox(0, 'Could not force windowed=1!', 'Error', 0);
    exit;
  end;

    { change check of Legth(myIP) >= 11 to Legth(myIP) >= 1 }
  sCode := #$83#$f8#$01;  // cmp eax, 0x01 (was cmp eax ,0x0b)
  if not DoWriteMemory(PChar(pCode) + $6b, sCode) then begin
    MessageBox(0, 'Could not force IP check!', 'Error', 0);
    exit;
  end;

    { short circut strncmp(myIP, mythicIP, 11) to always true }
  sCode := #$90#$90;  // nop (was jnz vvvv)
  if not DoWriteMemory(PChar(pCode) + $88, sCode) then begin
    MessageBox(0, 'Could not short circut IP compare!', 'Error', 0);
    exit;
  end;

  Result := true;
end;

procedure PatchMULTIINSTANCELoc(pe: TPortableExecutable);
var
  pWndClassStr:   Pointer;
  sWndClassAddr:  array[0..4] of char absolute pWndClassStr;
  pCode:    Pointer;
  sCode:  string;
begin
  pWndClassStr := SearchSegment(pe.ImageBase,
    pe.ObjectTable.FindSegment('.data').RVA,
    pe.ObjectTable.FindSegment('.data').PhysicalSize,
    'DAoCMWC'#0'Dark Age of Camelot');

  if not Assigned(pWndClassStr) then
    exit;

    { push aWndClass; call ds:xxxxx }
  sCode := #$68 + sWndClassAddr[0] + sWndClassAddr[1] + sWndClassAddr[2] + sWndClassAddr[3] +
    #$ff#$15;

  pCode := SearchSegment(pe.ImageBase,
    pe.ObjectTable.FindSegment('.text').RVA,
    pe.ObjectTable.FindSegment('.text').PhysicalSize,
    sCode);

  if not Assigned(pCode) then
    exit;

    { change to think inst count and findwindow failed }
    { test esi, esi; jz short; // test eax, eax; jnz short }
    { xor eax, eax; nop }
  sCode := #$33#$c0 +   // xor eax, eax
    #$90#$90;           // nop
  if not DoWriteMemory(PChar(pCode) + $0b, sCode) then
    exit;
end;

procedure PatchDebugAccess;
var
  pe:   TPortableExecutable;
begin
  pe := TPortableExecutable.Create;
  try
    pe.LoadFromFile(ParamStr(1));

    DoWriteMemory(Pointer($6C8C70),
        //Pointer(pe.ImageBase +
        //pe.ObjectTable.FindSegment('.text').RVA +
        //$B3C70),
      #$ff#$ff#$ff#$ff);
  finally
    pe.Free;
  end;
end;

function PatchMemoryLocs : boolean;
var
  pe:   TPortableExecutable;
begin
  pe := TPortableExecutable.Create;
  try
    pe.LoadFromFile(ParamStr(1));

    Result := PatchWINDOWEDLoc(pe);
    if not Result then
      exit;

    PatchMULTIINSTANCELoc(pe);
  finally
    pe.Free;
  end;
end;

var
  I:    integer;
  sCmdLine: string;
  si:   TStartupInfo;
begin
  sCmdLine := ParamStr(1) + ' ';
  for I := 2 to ParamCount do
    sCmdLine := sCmdLine + ParamStr(I) + ' ';

  FillChar(si, sizeof(si), 0);
  si.cb := sizeof(si);
  FillChar(pi, sizeof(pi), 0);
  if not CreateProcess(PChar(ParamStr(1)), PChar(sCmdLine), nil, nil, false, CREATE_SUSPENDED, nil,
    nil, si, pi) then begin
    MessageBox(0, PChar('Error ' + IntToStr(GetLastError) + ' launching'), 'GameLaunch', MB_OK);
    exit;
  end;

  // PatchDebugAccess;

//  if not PatchMemoryLocs then
//    ExitProcess(pi.hProcess);

  ResumeThread(pi.hThread);

  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
end.

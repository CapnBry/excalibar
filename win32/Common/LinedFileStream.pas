unit LinedFileStream;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  SysUtils, Classes;

type
    { what do do when we reach EOF and we have data in the line buffer
      ie when the file does not end with CRLF }
  TEOFAction = (eofReturnBlank, eofReturnLine);
   
  TLinedStreamWrapper = class(TObject)
  private
    FStream:  TStream;
    FBuffer:  array[0..(4*1024)-1] of char;
    FBuffPos:   integer;
    FBuffCount: integer;
    FPrevLineBuffer:  string;
    FLastCharWasCR:    boolean;
    FEOFAction: TEOFAction;

    procedure ResetState;
    procedure SetStream(AStream: TStream);
  public
    constructor Create(AStream: TStream = nil);

    function ReadLn: string;
    function EOF: boolean;

    property CurrentLineBuffer: string read FPrevLineBuffer;
    property Stream: TStream read FStream write SetStream;
    property EOFAction: TEOFAction read FEOFAction write FEOFAction;
  end;

  TLinedFileStream = class(TFileStream)
  private
    FLineWrapper: TLinedStreamWrapper;
    procedure CheckCreateWrapper;
  public
    destructor Destroy; override;

    function ReadLn: string;
    function EOF: boolean;
  end;

implementation

{ TLinedStreamWrapper }

constructor TLinedStreamWrapper.Create(AStream: TStream);
begin
  FStream := AStream;
  FEOFAction := eofReturnLine;
end;

function TLinedStreamWrapper.EOF: boolean;
var
  iReportedSize: integer;
begin
    { only check stream position if we have a stream assigned and we're not
      out of buffer }
  if Assigned(FStream) and (FBuffPos >= FBuffCount) then begin
    iReportedSize := FStream.Size;
    if iReportedSize = -1 then
      raise Exception.Create('Reported size of file is now -1');
      
    Result := FStream.Position >= iReportedSize;
  end
  else
    Result := false;
end;

function TLinedStreamWrapper.ReadLn: string;
var
  iOldLen:    Integer;
  iStartPos:  Integer;
  cIn:    char;
begin
  if not Assigned(FStream) then
    raise Exception.Create('No inner stream set');
    
  cIn := #0;

    { if we're out of buffer, try a refill from file }
  if (FBuffPos >= FBuffCount) and (FStream.Position < FStream.Size) then begin
    FBuffCount := FStream.Read(FBuffer, sizeof(FBuffer));
    FBuffPos := 0;
  end;

    { the idea here is we're looking for LF to end the line.  However, since
        Windows = CRLF
        Linux = LF
        Max = CR
      What I do is look for either CR or LF to end a line.  If the line ends
      with CR, then check the next char for LF.  If it is, discard it }
  iStartPos := FBuffPos;
  while (FBuffPos < FBuffCount) and (cIn <> #13) do begin
    cIn := FBuffer[FBuffPos];
    inc(FBuffPos);

    if cIn = #10 then
      if FLastCharWasCR then
        iStartPos := FBuffPos
      else
        break;

    FLastCharWasCR := false;
  end;  { while pos < size }

  iOldLen := Length(FPrevLineBuffer);
  if (cIn in [#10, #13]) or ((FEOFAction = eofReturnLine) and EOF) then begin
    FLastCharWasCR := true;
     
    SetLength(Result, iOldLen + FBuffPos - iStartPos - 1);
    Move(Pointer(FPrevLineBuffer)^, Pointer(Result)^, iOldLen);
    Move(FBuffer[iStartPos], Pointer(Integer(Result) + iOldLen)^,
      FBuffPos - iStartPos - 1);
    FPrevLineBuffer := '';
  end
  else begin
    if iStartPos < FBuffPos then begin
      SetLength(FPrevLineBuffer, iOldLen + FBuffPos - iStartPos);
      Move(FBuffer[iStartPos], Pointer(Integer(FPrevLineBuffer) + iOldLen)^,
        FBuffPos - iStartPos);
    end;
    Result := '';
  end;  { if incomplete line }
end;

procedure TLinedStreamWrapper.ResetState;
begin
  FBuffPos := 0;
  FBuffCount := 0;
  FPrevLineBuffer := '';
end;

procedure TLinedStreamWrapper.SetStream(AStream: TStream);
begin
  FStream := AStream;
  ResetState;
end;

{ TLinedFileStream }

procedure TLinedFileStream.CheckCreateWrapper;
begin
  if not Assigned(FLineWrapper) then begin
    FLineWrapper := TLinedStreamWrapper.Create;
    FLineWrapper.SetStream(Self);
  end;
end;

destructor TLinedFileStream.Destroy;
begin
  FreeAndNil(FLineWrapper);
  inherited;
end;

function TLinedFileStream.EOF: boolean;
begin
  CheckCreateWrapper;
  Result := FLineWrapper.EOF;
end;

function TLinedFileStream.ReadLn: string;
begin
  CheckCreateWrapper;
  Result := FLineWrapper.ReadLn;
end;

end.

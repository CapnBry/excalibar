unit LinedFileStream;

interface

uses
  SysUtils, Classes;

type
  TLinedStreamWrapper = class(TObject)
  private
    FStream:  TStream;
    FBuffer:  array[0..(4*1024)-1] of char;
    FBuffPos:   integer;
    FBuffCount: integer;
    FPrevLineBuffer:  string;

    procedure ResetState;
    procedure SetStream(AStream: TStream);
  public
    constructor Create(AStream: TStream = nil);
    
    function ReadLn: string;
    function EOF: boolean;

    property Stream: TStream read FStream write SetStream;
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
end;

function TLinedStreamWrapper.EOF: boolean;
var
  iReportedSize: integer;
begin
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

  iStartPos := FBuffPos;
  while (FBuffPos < FBuffCount) and (cIn <> #13) do begin
    cIn := FBuffer[FBuffPos];
    inc(FBuffPos);

    if cIn = #10 then
      iStartPos := FBuffPos;
  end;  { while pos < size }

  iOldLen := Length(FPrevLineBuffer);
  if cIn = #13 then begin
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
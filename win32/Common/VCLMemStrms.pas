unit VCLMemStrms;

interface

uses
  Classes;

type
    { Memory stream which does a VCL GemMem (heap alloc) rather than a
       GlobalAlloc for its memory allocation.  Great optimized way to
       store small streams in memory. }
  TVCLMemoryStream = class(TMemoryStream)
  private
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
  end;

    { Class which always MinCapacity bytes of memory allocated.  Useful
      if you do alot of write / read / clear / repeat.  Set MinCapacity
      high enough to hold the largest item you'd want to put in the stream }
  TMinSizeVCLMemStream = class(TMemoryStream)
  private
    FMinCapacity: integer;
    procedure SetMinCapacity(const Value: integer);
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    destructor Destroy; override;
    property MinCapacity: integer read FMinCapacity write SetMinCapacity;
  end;


implementation

const
  MALLOC_INCREMENT = $400;  { must be multiple of 2? }

{ TVCLMemoryStream }

function TVCLMemoryStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  if NewCapacity > 0 then
    NewCapacity := (NewCapacity + (MALLOC_INCREMENT - 1)) and not (MALLOC_INCREMENT - 1);

  if NewCapacity = Capacity then
    Result := Memory
  else begin
    if NewCapacity = 0 then begin
      FreeMem(Memory);
      Result := nil;
      SetPointer(nil, 0);
    end

    else begin
      Result := Memory;
      ReallocMem(Result, NewCapacity);
      if Result = nil then raise EStreamError.Create('Out of memory');
    end;
  end;  { if changing capacity }
end;

{ TMinSizeVCLMemStream }

destructor TMinSizeVCLMemStream.Destroy;
begin
    { the capacity property will be wrong after this call, but who cares }
  if Assigned(Memory) then
    FreeMem(Memory);
  SetPointer(nil, 0);

  inherited Destroy;
end;

function TMinSizeVCLMemStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  if NewCapacity < FMinCapacity then
    NewCapacity := FMinCapacity
  else if NewCapacity > 0 then
    NewCapacity := (NewCapacity + (MALLOC_INCREMENT - 1)) and not (MALLOC_INCREMENT - 1);

  if NewCapacity = Capacity then
    Result := Memory
  else begin
    if NewCapacity = 0 then begin
      FreeMem(Memory);
      Result := nil;
      SetPointer(nil, 0);
    end

    else begin
      Result := Memory;
      ReallocMem(Result, NewCapacity);
      if Result = nil then raise EStreamError.Create('Out of memory');
    end;
  end;  { if changing capacity }
end;

procedure TMinSizeVCLMemStream.SetMinCapacity(const Value: integer);
begin
  FMinCapacity := (Value + (MALLOC_INCREMENT - 1)) and not (MALLOC_INCREMENT - 1);
end;

end.

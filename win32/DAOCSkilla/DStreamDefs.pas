unit DStreamDefs;

interface

type
  PDStream_Header = ^TDStream_Header;
  TDStream_Header = packed record
    total_length:   WORD;
    command_id:     BYTE;
  end;

  TDStreamPacketNotify = procedure (Sender: TObject; AHeader: PDStream_Header) of object;

const
  DEFAULT_DSTREAM_PORT = 9867;

  DPACKET_DAOC_CONNECTION_OPENED = $05;
  DPACKET_DAOC_CONNECTION_CLOSED = $06;
  DPACKET_DAOC_DATA = $07;

function DStreamPointToData(AHeader: PDStream_Header) : Pointer;

implementation

function DStreamPointToData(AHeader: PDStream_Header) : Pointer;
begin
  Result := Pointer(Cardinal(AHeader) + sizeof(TDStream_Header));
end;

end.

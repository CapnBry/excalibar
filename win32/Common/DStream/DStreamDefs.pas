unit DStreamDefs;

interface

type
  PDStream_Header = ^TDStream_Header;
  TDStream_Header = packed record
    total_length:   WORD;
    command_id:     BYTE;
  end;

  PDStream_DAOCConnectionDetails = ^TDStream_DAOCConnectionDetails;
  TDStream_DAOCConnectionDetails = packed record
    ConnectionID: Cardinal;
    ServerIP:     Cardinal;
    ServerPort:   WORD;
    ClientIP:     Cardinal;
    ClientPort:   WORD;
  end;

  PDStream_DAOCDataHeader = ^TDStream_DAOCDataHeader;
  TDStream_DAOCDataHeader = packed record
    ConnectionID: Cardinal;
    Origin:       BYTE;   // 0 from server, 1 from client
    Protocol:     BYTE;   // 0 for TCP, 1 for UDP
    DataSize:     WORD;
  end;

  TDStreamPacketNotify = procedure (Sender: TObject; AHeader: PDStream_Header) of object;

const
  DEFAULT_DSTREAM_PORT = 9867;

  DSTREAM_PROTOCOL_VER = 1;

    { Packet types }
  DPACKET_HELO = $01;
  DPACKET_LOG = $02;
  DPACKET_CONNECTION_QUERY = $03;  // NOTIMPL
  DPACKET_CONNECTION_DETAILS = $04; // NOTIMPL
  DPACKET_DAOC_CONNECTION_OPENED = $05;
  DPACKET_DAOC_CONNECTION_CLOSED = $06;
  DPACKET_DAOC_DATA = $07;
  DPACKET_SET_PACKETFILTER = $08;  // NOTIMPL
  DPACKET_AUTH_CREDENTIALS = $09;  // NOTIMPL
  DPACKET_AUTH_RESPONSE = $0a;  // NOTIMPL
  DPACKET_SQUELCH = $0b;
  DPACKET_RESUME = $0c;

function DStreamPointToData(AHeader: PDStream_Header) : Pointer;

implementation

function DStreamPointToData(AHeader: PDStream_Header) : Pointer;
begin
  Result := Pointer(Cardinal(AHeader) + sizeof(TDStream_Header));
end;

end.

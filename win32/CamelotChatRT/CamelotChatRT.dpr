program CamelotChatRT;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  DAOCConnection in '..\Common\PacketSniff\DAOCConnection.pas',
  PReader2 in '..\Common\PacketSniff\PReader2.pas',
  NtDdNDIS in '..\Common\PacketSniff\NtDDNdis.pas',
  Packet32 in '..\Common\PacketSniff\Packet32.pas',
  bpf in '..\Common\PacketSniff\bpf.pas',
  Pcap in '..\Common\PacketSniff\pcap.pas',
  Ndis_def in '..\Common\PacketSniff\Ndis_def.pas',
  FrameFns in '..\Common\PacketSniff\FrameFns.pas',
  DAOCInventory in '..\Common\DAOCInfo\daocinventory.pas',
  DAOCObjs in '..\Common\DAOCInfo\DAOCObjs.pas',
  DAOCPlayerAttributes in '..\Common\DAOCInfo\DAOCPlayerAttributes.pas',
  DAOCRegion in '..\Common\DAOCInfo\DAOCRegion.pas',
  StringParseHlprs in '..\Common\StringParseHlprs.pas',
  VendorItems in '..\Common\DAOCInfo\VendorItems.pas',
  ChatParse in '..\Common\ChatParse\ChatParse.pas',
  MapNavigator in '..\Common\MapNavigator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Camelot Real Time Chat Log';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

program DAOCSkilla;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  bpf in '..\Common\PacketSniff\bpf.pas',
  DAOCConnection in '..\Common\PacketSniff\DAOCConnection.pas',
  FrameFns in '..\Common\PacketSniff\FrameFns.pas',
  Ndis_def in '..\Common\PacketSniff\Ndis_def.pas',
  NtDdNDIS in '..\Common\PacketSniff\NtDDNdis.pas',
  Packet32 in '..\Common\PacketSniff\Packet32.pas',
  Pcap in '..\Common\PacketSniff\pcap.pas',
  PReader2 in '..\Common\PacketSniff\PReader2.pas',
  DAOCInventory in '..\Common\DAOCInfo\daocinventory.pas',
  DAOCObjs in '..\Common\DAOCInfo\DAOCObjs.pas',
  DAOCPlayerAttributes in '..\Common\DAOCInfo\DAOCPlayerAttributes.pas',
  DAOCRegion in '..\Common\DAOCInfo\DAOCRegion.pas',
  VendorItems in '..\Common\DAOCInfo\VendorItems.pas',
  MapNavigator in '..\Common\MapNavigator.pas',
  StringParseHlprs in '..\Common\StringParseHlprs.pas',
  ChatParse in '..\Common\ChatParse\ChatParse.pas',
  LinedFileStream in '..\Common\LinedFileStream.pas',
  DAOCSkilla_TLB in 'DAOCSkilla_TLB.pas',
  MPKFile in '..\Common\MPKFile.pas',
  CSVLineParser in '..\Common\CSVLineParser.pas',
  Recipes in '..\Common\DAOCInfo\Recipes.pas',
  DAOCControl in '..\Common\DAOCAutomation\DAOCControl.pas',
  sndkey32 in '..\Common\DAOCAutomation\SNDKEY32.pas',
  DAOCWindows in '..\Common\DAOCInfo\DAOCWindows.pas',
  PowerSkill in '..\Common\DAOCAutomation\PowerSkill.pas',
  PowerSkillSetup in '..\Common\DAOCAutomation\PowerSkillSetup.pas' {frmPowerskill},
  ShowMapNodes in '..\Common\ShowMapNodes.pas' {frmShowMapNodes},
  MacroTradeSkill in '..\Common\DAOCAutomation\MacroTradeSkill.pas' {frmMacroTradeSkills},
  AFKMessage in '..\Common\DAOCAutomation\AFKMessage.pas' {frmAFK},
  TellMacro in '..\Common\DAOCAutomation\TellMacro.pas' {frmTellMacro},
  AXScript in '..\Common\DAOCAutomation\AXScript.pas',
  ScriptSiteImpl in '..\Common\DAOCAutomation\ScriptSiteImpl.pas',
  SpellcraftHelp in '..\Common\DAOCAutomation\SpellcraftHelp.pas' {frmSpellcraftHelp},
  GLRenderObjects in 'GLRenderObjects.pas',
  MapElementList in 'MapElementList.pas',
  DDSImage in 'DDSImage.pas',
  VCLMemStrms in '..\Common\VCLMemStrms.pas',
  DAOCPackets in '..\Common\PacketSniff\DAOCPackets.pas',
  RenderPrefs in 'RenderPrefs.pas' {frmRenderPrefs},
  DAOCClasses in '..\Common\DAOCInfo\DAOCClasses.pas',
  DAOCConSystem in '..\Common\DAOCInfo\DAOCConSystem.pas',
  DebugAndTracing in 'DebugAndTracing.pas' {frmDebugging};

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DAOC Auto-tradeskiller';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPowerskill, frmPowerskill);
  Application.CreateForm(TfrmShowMapNodes, frmShowMapNodes);
  Application.CreateForm(TfrmMacroTradeSkills, frmMacroTradeSkills);
  Application.CreateForm(TfrmAFK, frmAFK);
  Application.CreateForm(TfrmTellMacro, frmTellMacro);
  Application.CreateForm(TfrmSpellcraftHelp, frmSpellcraftHelp);
  Application.CreateForm(TfrmDebugging, frmDebugging);
  CreateOptionalForms;
  Application.Run;
end.

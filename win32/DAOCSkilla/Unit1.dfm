object frmMain: TfrmMain
  Left = 556
  Top = 197
  Width = 510
  Height = 497
  Caption = 'DAOC Skilla'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    502
    470)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlayerPos: TLabel
    Left = 380
    Top = 60
    Width = 121
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerPos'
  end
  object lblPlayerHead: TLabel
    Left = 408
    Top = 76
    Width = 93
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerHead'
  end
  object lblPlayerSpeed: TLabel
    Left = 407
    Top = 92
    Width = 94
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerSpeed'
  end
  object lblFilePos: TLabel
    Left = 348
    Top = 204
    Width = 44
    Height = 13
    Caption = 'lblFilePos'
  end
  object btnPowerskillBuy: TSpeedButton
    Left = 8
    Top = 136
    Width = 117
    Height = 22
    Caption = 'Powerskill Buy'
    OnClick = btnPowerskillBuyClick
  end
  object imgAdapter: TImage
    Left = 268
    Top = 108
    Width = 16
    Height = 12
    AutoSize = True
    Picture.Data = {
      07544269746D6170BA000000424DBA000000000000005A000000280000001000
      00000C000000010004000000000060000000120B0000120B0000090000000900
      0000FF00FF00C6C3C6008482840000FF00000082000000FFFF00008284000000
      FF00000000000000000888888000000000025656800088888882565688802444
      4444144444802488888414848480241241841414148824888884111114882712
      4184144411882488888414881488271241841418118824444444134314802222
      222222222280}
    Transparent = True
    Visible = False
  end
  object Bevel1: TBevel
    Left = 4
    Top = 128
    Width = 497
    Height = 10
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 4
    Top = 192
    Width = 493
    Height = 10
    Shape = bsTopLine
  end
  object lblZone: TLabel
    Left = 380
    Top = 108
    Width = 121
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblZone'
  end
  object lstAdapters: TListBox
    Left = 4
    Top = 4
    Width = 493
    Height = 49
    Style = lbOwnerDrawFixed
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnClick = lstAdaptersClick
    OnDrawItem = lstAdaptersDrawItem
  end
  object Memo1: TMemo
    Left = 4
    Top = 256
    Width = 493
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object chkCapture: TCheckBox
    Left = 4
    Top = 84
    Width = 185
    Height = 17
    Caption = 'Capture packets to c:\mycap.cap'
    TabOrder = 2
    OnClick = chkCaptureClick
  end
  object btnOpenPlayback: TButton
    Left = 248
    Top = 200
    Width = 97
    Height = 25
    Caption = 'Open playback file'
    TabOrder = 3
    OnClick = btnOpenPlaybackClick
  end
  object edtPlayback: TEdit
    Left = 4
    Top = 200
    Width = 237
    Height = 21
    TabOrder = 4
    Text = 'C:\savedcap.cap'
  end
  object btnPlayPacket: TButton
    Left = 348
    Top = 228
    Width = 89
    Height = 25
    Caption = 'Play one packet'
    TabOrder = 5
    OnClick = btnPlayPacketClick
  end
  object btnPlayTimer: TButton
    Left = 248
    Top = 228
    Width = 97
    Height = 25
    Caption = 'Play packets'
    TabOrder = 6
    OnClick = btnPlayTimerClick
  end
  object chkDumpPackets: TCheckBox
    Left = 4
    Top = 104
    Width = 205
    Height = 17
    Caption = 'Dump packet data to log window'
    TabOrder = 7
  end
  object chkProcessPackets: TCheckBox
    Left = 4
    Top = 56
    Width = 213
    Height = 17
    Caption = 'Process packet data (don'#39't just capture)'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object chkAutosell: TCheckBox
    Left = 132
    Top = 140
    Width = 209
    Height = 17
    Caption = 'Autosell hinges and all powerskill items'
    TabOrder = 9
    OnClick = chkAutosellClick
  end
  object Button3: TButton
    Left = 8
    Top = 160
    Width = 117
    Height = 25
    Caption = 'Show nav map nodes'
    TabOrder = 10
    OnClick = Button3Click
  end
  object chkPCAPFile: TCheckBox
    Left = 4
    Top = 224
    Width = 237
    Height = 17
    Caption = 'Process as libpcap file'
    TabOrder = 11
  end
  object btnMacroTradeskill: TButton
    Left = 132
    Top = 160
    Width = 101
    Height = 25
    Caption = 'Macro Tradeskill'
    TabOrder = 12
    OnClick = btnMacroTradeskillClick
  end
  object btnAFK: TButton
    Left = 240
    Top = 160
    Width = 89
    Height = 25
    Caption = 'AFK Message'
    TabOrder = 13
    OnClick = btnAFKClick
  end
  object btnGLRender: TButton
    Left = 416
    Top = 160
    Width = 75
    Height = 25
    Caption = 'GL Render'
    TabOrder = 14
    OnClick = btnGLRenderClick
  end
  object btnTellMacro: TButton
    Left = 336
    Top = 160
    Width = 75
    Height = 25
    Caption = 'MacroScript'
    TabOrder = 15
    OnClick = btnTellMacroClick
  end
  object btnSpellcraftHlp: TButton
    Left = 416
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Spellcrft Help'
    TabOrder = 16
    OnClick = btnSpellcraftHlpClick
  end
  object Button1: TButton
    Left = 300
    Top = 104
    Width = 75
    Height = 25
    Caption = 'dmpmobs'
    TabOrder = 17
    OnClick = Button1Click
  end
  object chkRecordMobseen: TCheckBox
    Left = 244
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Record mobseen'
    TabOrder = 18
    OnClick = chkRecordMobseenClick
  end
  object tmrPlayback: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrPlaybackTimer
    Left = 188
    Top = 212
  end
  object tmrTimeoutDelay: TTimer
    Enabled = False
    OnTimer = tmrTimeoutDelayTimer
    Left = 376
    Top = 76
  end
end

object frmMain: TfrmMain
  Left = 716
  Top = 201
  Width = 510
  Height = 507
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
    480)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlayerPos: TLabel
    Left = 340
    Top = 60
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerPos'
  end
  object lblPlayerHeadSpeed: TLabel
    Left = 340
    Top = 76
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerHeadSpeed'
  end
  object btnPowerskillBuy: TSpeedButton
    Left = 8
    Top = 188
    Width = 117
    Height = 25
    Caption = 'Powerskill Buy'
    OnClick = btnPowerskillBuyClick
  end
  object imgAdapter: TImage
    Left = 284
    Top = 64
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
    Top = 136
    Width = 497
    Height = 10
    Shape = bsTopLine
  end
  object lblZone: TLabel
    Left = 340
    Top = 92
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblZone'
  end
  object Label1: TLabel
    Left = 8
    Top = 144
    Width = 477
    Height = 13
    Caption = 
      'Macroing is active while the appropriate window is open.  Do not' +
      ' use if you have not read the source.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
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
    Top = 216
    Width = 493
    Height = 261
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
  object chkAutosell: TCheckBox
    Left = 132
    Top = 164
    Width = 205
    Height = 17
    Caption = 'Autosell hinges and all powerskill items'
    TabOrder = 2
    OnClick = chkAutosellClick
  end
  object btnMacroTradeskill: TButton
    Left = 132
    Top = 188
    Width = 101
    Height = 25
    Caption = 'Macro Tradeskill'
    TabOrder = 3
    OnClick = btnMacroTradeskillClick
  end
  object btnAFK: TButton
    Left = 240
    Top = 188
    Width = 89
    Height = 25
    Caption = 'AFK Message'
    TabOrder = 4
    OnClick = btnAFKClick
  end
  object btnGLRender: TButton
    Left = 8
    Top = 76
    Width = 75
    Height = 25
    Caption = 'E&xcalibur'
    TabOrder = 5
    OnClick = btnGLRenderClick
  end
  object btnTellMacro: TButton
    Left = 336
    Top = 188
    Width = 75
    Height = 25
    Caption = 'MacroScript'
    TabOrder = 6
    OnClick = btnTellMacroClick
  end
  object btnSpellcraftHlp: TButton
    Left = 416
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Spellcrft Help'
    TabOrder = 7
    OnClick = btnSpellcraftHlpClick
  end
  object btnDebugging: TButton
    Left = 84
    Top = 76
    Width = 75
    Height = 25
    Caption = '&Debugging'
    TabOrder = 8
    OnClick = btnDebuggingClick
  end
  object chkAutolaunchExcal: TCheckBox
    Left = 8
    Top = 56
    Width = 169
    Height = 17
    Caption = 'Autolaunch Excalibur windows'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
  object btnShowMapModes: TButton
    Left = 8
    Top = 162
    Width = 117
    Height = 22
    Caption = 'Show nav map nodes'
    TabOrder = 10
    OnClick = btnShowMapModesClick
  end
  object chkChatLog: TCheckBox
    Left = 8
    Top = 110
    Width = 105
    Height = 17
    Caption = 'Realtime chat.log'
    TabOrder = 11
    OnClick = chkChatLogClick
  end
  object edtChatLogFile: TEdit
    Left = 116
    Top = 108
    Width = 381
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    Text = 'c:\mythic\isles\realchat.log'
  end
  object tmrTimeoutDelay: TTimer
    Enabled = False
    OnTimer = tmrTimeoutDelayTimer
    Left = 300
    Top = 60
  end
end

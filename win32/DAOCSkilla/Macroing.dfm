object frmMacroing: TfrmMacroing
  Left = 253
  Top = 238
  BorderStyle = bsDialog
  Caption = 'Macroing'
  ClientHeight = 119
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnPowerskillBuy: TSpeedButton
    Left = 4
    Top = 92
    Width = 117
    Height = 25
    Caption = 'Powerskill Buy'
    Enabled = False
    OnClick = btnPowerskillBuyClick
  end
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 266
    Height = 13
    Caption = 'Macroing is active while the appropriate window is open.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 4
    Top = 40
    Width = 89
    Height = 13
    Caption = 'Macro connection:'
  end
  object Label3: TLabel
    Left = 4
    Top = 20
    Width = 477
    Height = 13
    Caption = 
      'Currently only works on the same machine as the daoc client, and' +
      ' only on the foreground connection.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 264
    Top = 40
    Width = 212
    Height = 13
    Caption = '<-- Whatever you set here is ignored!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object chkAutosell: TCheckBox
    Left = 128
    Top = 68
    Width = 213
    Height = 17
    Caption = 'Autosell all powerskill items and trinkets'
    Enabled = False
    TabOrder = 0
    OnClick = chkAutosellClick
  end
  object btnMacroTradeskill: TButton
    Left = 128
    Top = 92
    Width = 101
    Height = 25
    Caption = 'Macro Tradeskill'
    Enabled = False
    TabOrder = 1
    OnClick = btnMacroTradeskillClick
  end
  object btnAFK: TButton
    Left = 236
    Top = 92
    Width = 89
    Height = 25
    Caption = 'AFK Message'
    Enabled = False
    TabOrder = 2
    OnClick = btnAFKClick
  end
  object btnTellMacro: TButton
    Left = 332
    Top = 92
    Width = 75
    Height = 25
    Caption = 'MacroScript'
    Enabled = False
    TabOrder = 3
    OnClick = btnTellMacroClick
  end
  object btnSpellcraftHlp: TButton
    Left = 412
    Top = 92
    Width = 75
    Height = 25
    Caption = 'Spellcrft Help'
    Enabled = False
    TabOrder = 4
    OnClick = btnSpellcraftHlpClick
  end
  object btnShowMapModes: TButton
    Left = 4
    Top = 62
    Width = 117
    Height = 27
    Caption = 'Show nav map nodes'
    Enabled = False
    TabOrder = 5
    OnClick = btnShowMapModesClick
  end
  object btnLowOnStat: TButton
    Left = 412
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Low On Stat'
    Enabled = False
    TabOrder = 6
    OnClick = btnLowOnStatClick
  end
  object cbxConnectionList: TComboBox
    Left = 100
    Top = 36
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    OnChange = cbxConnectionListChange
  end
  object tmrTimeoutDelay: TTimer
    Enabled = False
    OnTimer = tmrTimeoutDelayTimer
    Left = 344
    Top = 60
  end
end

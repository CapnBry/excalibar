object frmMacroing: TfrmMacroing
  Left = 253
  Top = 238
  BorderStyle = bsDialog
  Caption = 'Macroing'
  ClientHeight = 117
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
    Top = 76
    Width = 117
    Height = 25
    Caption = 'Powerskill Buy'
    Enabled = False
    OnClick = btnPowerskillBuyClick
  end
  object Label1: TLabel
    Left = 0
    Top = 104
    Width = 492
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = 'Macroing is active while the appropriate window is open.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 4
    Top = 24
    Width = 89
    Height = 13
    Caption = 'Macro connection:'
  end
  object Label3: TLabel
    Left = 4
    Top = 4
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
  object lblMacroState: TLabel
    Left = 260
    Top = 24
    Width = 65
    Height = 13
    Caption = 'lblMacroState'
    Visible = False
  end
  object chkAutosell: TCheckBox
    Left = 128
    Top = 52
    Width = 213
    Height = 17
    Caption = 'Autosell all powerskill items and trinkets'
    Enabled = False
    TabOrder = 0
    OnClick = chkAutosellClick
  end
  object btnMacroTradeskill: TButton
    Left = 128
    Top = 76
    Width = 101
    Height = 25
    Caption = 'Macro Tradeskill'
    Enabled = False
    TabOrder = 1
    OnClick = btnMacroTradeskillClick
  end
  object btnAFK: TButton
    Left = 236
    Top = 76
    Width = 89
    Height = 25
    Caption = 'AFK Message'
    Enabled = False
    TabOrder = 2
    OnClick = btnAFKClick
  end
  object btnTellMacro: TButton
    Left = 332
    Top = 76
    Width = 75
    Height = 25
    Caption = 'MacroScript'
    Enabled = False
    TabOrder = 3
    OnClick = btnTellMacroClick
  end
  object btnSpellcraftHlp: TButton
    Left = 412
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Spellcrft Help'
    Enabled = False
    TabOrder = 4
    OnClick = btnSpellcraftHlpClick
  end
  object btnShowMapModes: TButton
    Left = 4
    Top = 46
    Width = 117
    Height = 27
    Caption = 'Show nav map nodes'
    Enabled = False
    TabOrder = 5
    OnClick = btnShowMapModesClick
  end
  object btnLowOnStat: TButton
    Left = 412
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Low On Stat'
    Enabled = False
    TabOrder = 6
    OnClick = btnLowOnStatClick
  end
  object cbxConnectionList: TComboBox
    Left = 100
    Top = 20
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
    Top = 44
  end
end

object frmMacroing: TfrmMacroing
  Left = 253
  Top = 238
  BorderStyle = bsDialog
  Caption = 'Macroing'
  ClientHeight = 83
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
    Top = 52
    Width = 117
    Height = 25
    Caption = 'Powerskill Buy'
    OnClick = btnPowerskillBuyClick
  end
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 481
    Height = 13
    Caption = 
      'Macroing is active while the appropriate window is open.  This c' +
      'ode is unsupported and unmaintained.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object chkAutosell: TCheckBox
    Left = 128
    Top = 28
    Width = 213
    Height = 17
    Caption = 'Autosell all powerskill items and trinkets'
    TabOrder = 0
    OnClick = chkAutosellClick
  end
  object btnMacroTradeskill: TButton
    Left = 128
    Top = 52
    Width = 101
    Height = 25
    Caption = 'Macro Tradeskill'
    TabOrder = 1
    OnClick = btnMacroTradeskillClick
  end
  object btnAFK: TButton
    Left = 236
    Top = 52
    Width = 89
    Height = 25
    Caption = 'AFK Message'
    TabOrder = 2
    OnClick = btnAFKClick
  end
  object btnTellMacro: TButton
    Left = 332
    Top = 52
    Width = 75
    Height = 25
    Caption = 'MacroScript'
    TabOrder = 3
    OnClick = btnTellMacroClick
  end
  object btnSpellcraftHlp: TButton
    Left = 412
    Top = 52
    Width = 75
    Height = 25
    Caption = 'Spellcrft Help'
    TabOrder = 4
    OnClick = btnSpellcraftHlpClick
  end
  object btnShowMapModes: TButton
    Left = 4
    Top = 22
    Width = 117
    Height = 27
    Caption = 'Show nav map nodes'
    TabOrder = 5
    OnClick = btnShowMapModesClick
  end
  object btnLowOnStat: TButton
    Left = 412
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Low On Stat'
    TabOrder = 6
    OnClick = btnLowOnStatClick
  end
  object tmrTimeoutDelay: TTimer
    Enabled = False
    OnTimer = tmrTimeoutDelayTimer
    Left = 344
    Top = 20
  end
end

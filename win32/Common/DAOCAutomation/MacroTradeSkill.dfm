object frmMacroTradeSkills: TfrmMacroTradeSkills
  Left = 511
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Macro Tradeskills'
  ClientHeight = 115
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgression: TLabel
    Left = 4
    Top = 28
    Width = 55
    Height = 13
    Caption = 'Progression'
  end
  object lblTargetQual: TLabel
    Left = 4
    Top = 52
    Width = 66
    Height = 13
    Caption = 'Target Quality'
  end
  object lblTargetSound: TLabel
    Left = 4
    Top = 76
    Width = 65
    Height = 13
    Caption = 'Target Sound'
  end
  object Label1: TLabel
    Left = 152
    Top = 4
    Width = 109
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Stack Odds'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblStackOddsCount: TLabel
    Left = 152
    Top = 76
    Width = 28
    Height = 13
    Caption = 'Count'
  end
  object Bevel1: TBevel
    Left = 144
    Top = 4
    Width = 9
    Height = 89
    Shape = bsLeftLine
  end
  object lblStackOddsProgression: TLabel
    Left = 152
    Top = 28
    Width = 66
    Height = 13
    Caption = 'Quickbar Item'
  end
  object Label4: TLabel
    Left = 4
    Top = 4
    Width = 133
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Build Item'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblStackOddsPct: TLabel
    Left = 152
    Top = 52
    Width = 59
    Height = 13
    Caption = 'Max percent'
  end
  object edtProgression: TEdit
    Left = 80
    Top = 24
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '0'
    OnKeyPress = edtProgressionKeyPress
  end
  object edtTargetQual: TEdit
    Left = 80
    Top = 48
    Width = 57
    Height = 21
    TabOrder = 1
    Text = '100'
    OnKeyPress = edtTargetQualKeyPress
  end
  object edtTargetSound: TEdit
    Left = 80
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 2
    Text = 'beep'
    OnKeyPress = edtTargetSoundKeyPress
  end
  object chkStopIfFull: TCheckBox
    Left = 0
    Top = 96
    Width = 145
    Height = 17
    Caption = 'Stop if inventory is full'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chkStopIfFullClick
  end
  object edtStackOddsProgression: TEdit
    Left = 232
    Top = 24
    Width = 33
    Height = 21
    TabOrder = 4
    Text = '1'
    OnKeyPress = edtStackOddsProgressionKeyPress
  end
  object edtStackOddsCount: TEdit
    Left = 232
    Top = 72
    Width = 33
    Height = 21
    TabOrder = 5
    Text = '50'
    OnKeyPress = edtStackOddsCountKeyPress
  end
  object edtStackOddsPct: TEdit
    Left = 232
    Top = 48
    Width = 33
    Height = 21
    TabOrder = 6
    Text = '20'
    OnKeyPress = edtStackOddsPctKeyPress
  end
end

object frmLowOnStat: TfrmLowOnStat
  Left = 264
  Top = 191
  BorderStyle = bsDialog
  Caption = 'Low Stat Warning'
  ClientHeight = 153
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object chkLowHealth: TCheckBox
    Left = 4
    Top = 4
    Width = 157
    Height = 17
    Caption = 'Low health warning'
    TabOrder = 0
    OnClick = chkLowHealthClick
  end
  object edtLowHealthPct: TSpinEdit
    Left = 168
    Top = 0
    Width = 61
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 1
    Value = 50
    OnChange = edtLowHealthPctChange
  end
  object edtLowHealthText: TEdit
    Left = 4
    Top = 24
    Width = 229
    Height = 21
    TabOrder = 2
    Text = '/g These guys are killing me, I could use a heal.'
  end
  object chkLowEnd: TCheckBox
    Left = 4
    Top = 56
    Width = 157
    Height = 17
    Caption = 'Low endurance warning'
    TabOrder = 3
    OnClick = chkLowEndClick
  end
  object edtLowEndPct: TSpinEdit
    Left = 168
    Top = 52
    Width = 61
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 4
    Value = 5
    OnChange = edtLowEndPctChange
  end
  object edtLowEndText: TEdit
    Left = 3
    Top = 76
    Width = 229
    Height = 21
    TabOrder = 5
    Text = '/g Out of endurance, need more Stimutax.'
  end
  object chkLowMana: TCheckBox
    Left = 4
    Top = 108
    Width = 157
    Height = 17
    Caption = 'Low mana warning'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = chkLowManaClick
  end
  object edtLowManaPct: TSpinEdit
    Left = 168
    Top = 104
    Width = 61
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 7
    Value = 15
    OnChange = edtLowManaPctChange
  end
  object edtLowManaText: TEdit
    Left = 3
    Top = 128
    Width = 229
    Height = 21
    TabOrder = 8
    Text = '/g Low on mana, stop getting hurt!'
  end
end

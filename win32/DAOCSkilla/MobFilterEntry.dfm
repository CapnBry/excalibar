object frmMobFilterEntry: TfrmMobFilterEntry
  Left = 553
  Top = 485
  BorderStyle = bsDialog
  Caption = 'Mob filter entry'
  ClientHeight = 125
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 103
    Height = 13
    Caption = 'Mob name (or regexp)'
  end
  object Label2: TLabel
    Left = 232
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Min level'
  end
  object Label3: TLabel
    Left = 280
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Max level'
  end
  object Label4: TLabel
    Left = 8
    Top = 48
    Width = 107
    Height = 13
    Caption = 'Play alert upon spawn:'
  end
  object edtMobname: TEdit
    Left = 8
    Top = 24
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object edtMinLevel: TEdit
    Left = 232
    Top = 24
    Width = 33
    Height = 21
    MaxLength = 4
    TabOrder = 1
    Text = '0'
    OnKeyPress = edtMinLevelKeyPress
  end
  object edtMaxLevel: TEdit
    Left = 280
    Top = 24
    Width = 33
    Height = 21
    MaxLength = 4
    TabOrder = 2
    Text = '99'
    OnKeyPress = edtMinLevelKeyPress
  end
  object btnOk: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Default = True
    TabOrder = 5
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 88
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'C&ancel'
    ModalResult = 2
    TabOrder = 6
  end
  object edtAlert: TEdit
    Left = 8
    Top = 64
    Width = 289
    Height = 21
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 304
    Top = 64
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.wav'
    Filter = 'Wave File|*.wav'
    Left = 268
    Top = 80
  end
end

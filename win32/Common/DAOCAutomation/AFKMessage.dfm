object frmAFK: TfrmAFK
  Left = 187
  Top = 107
  Width = 346
  Height = 56
  Caption = 'AFK Message'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblAFKMessage: TLabel
    Left = 4
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Message'
  end
  object edtAFKMessage: TEdit
    Left = 56
    Top = 4
    Width = 277
    Height = 21
    TabOrder = 0
    Text = 'edtAFKMessage'
    OnKeyPress = edtAFKMessageKeyPress
  end
end

object frmAddPushpin: TfrmAddPushpin
  Left = 287
  Top = 294
  BorderStyle = bsDialog
  Caption = 'Add pushpin'
  ClientHeight = 65
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtLabel: TEdit
    Left = 4
    Top = 4
    Width = 217
    Height = 21
    TabOrder = 0
    Text = 'edtLabel'
  end
  object cbxColor: TColorBox
    Left = 4
    Top = 36
    Width = 153
    Height = 22
    DefaultColorColor = clWhite
    Selected = clSilver
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    ItemHeight = 16
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 248
    Top = 4
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 248
    Top = 36
    Width = 75
    Height = 25
    Caption = 'C&ancel'
    TabOrder = 3
    Kind = bkCancel
  end
end

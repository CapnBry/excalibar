object frmTellMacro: TfrmTellMacro
  Left = 871
  Top = 219
  Width = 294
  Height = 60
  Caption = 'Macro Script File'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object edtScriptFile: TEdit
    Left = 4
    Top = 4
    Width = 197
    Height = 21
    TabOrder = 0
    Text = 'tellscript.vbs'
  end
  object btnLoad: TButton
    Left = 208
    Top = 4
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 1
    OnClick = btnLoadClick
  end
end

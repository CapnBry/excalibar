object frmDisplayLicense: TfrmDisplayLicense
  Left = 290
  Top = 165
  Width = 564
  Height = 294
  Caption = 'DaocSkilla 2 - License Information'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 556
    Height = 49
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'If you paid for this program, you have been screwed.  DaocSkilla' +
      ' is FREE and released under the GPL.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object memLicense: TMemo
    Left = 0
    Top = 49
    Width = 556
    Height = 187
    Align = alClient
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'memLicense')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 236
    Width = 556
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      556
      31)
    object BitBtn1: TBitBtn
      Left = 476
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkOK
    end
  end
end

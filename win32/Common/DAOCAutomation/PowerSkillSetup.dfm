object frmPowerskill: TfrmPowerskill
  Left = 684
  Top = 267
  Width = 264
  Height = 428
  Caption = 'PowerSkill Buy'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpRecipe: TGroupBox
    Left = 0
    Top = 296
    Width = 256
    Height = 105
    Align = alBottom
    Caption = '  Recipe  '
    TabOrder = 0
    object lblName: TLabel
      Left = 8
      Top = 16
      Width = 46
      Height = 13
      Caption = 'lblName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblIngr1: TLabel
      Left = 8
      Top = 44
      Width = 34
      Height = 13
      Caption = 'lblIngr1'
    end
    object lblCount: TLabel
      Left = 208
      Top = 8
      Width = 44
      Height = 13
      Hint = 'Click to change quantity for this item'
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'lblCount'
      ParentShowHint = False
      ShowHint = True
      OnClick = lblCountClick
    end
    object lblCost: TLabel
      Left = 20
      Top = 28
      Width = 31
      Height = 13
      Caption = 'lblCost'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btnToQuickbar: TSpeedButton
      Left = 230
      Top = 78
      Width = 23
      Height = 25
      Hint = 'Place on quickbar'
      Caption = 'Q'
      ParentShowHint = False
      ShowHint = True
      OnClick = btnToQuickbarClick
    end
  end
  object lstItems: TListBox
    Left = 0
    Top = 49
    Width = 256
    Height = 247
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 1
    OnClick = lstItemsClick
    OnDrawItem = lstItemsDrawItem
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 256
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 4
      Top = 8
      Width = 29
      Height = 13
      Caption = 'Profile'
    end
    object edtProfile: TEdit
      Left = 40
      Top = 4
      Width = 165
      Height = 21
      TabOrder = 0
      Text = 'spellcrafting'
      OnKeyPress = edtProfileKeyPress
    end
    object btnLoad: TButton
      Left = 212
      Top = 4
      Width = 45
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object chkAutoAdvance: TCheckBox
      Left = 4
      Top = 28
      Width = 209
      Height = 17
      Caption = 'Auto-advance item with skill'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkAutoAdvanceClick
    end
  end
  object pnlLoading: TPanel
    Left = 0
    Top = 49
    Width = 256
    Height = 247
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'Loading...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
end

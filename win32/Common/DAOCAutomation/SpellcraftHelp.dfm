object frmSpellcraftHelp: TfrmSpellcraftHelp
  Left = 589
  Top = 273
  Width = 348
  Height = 342
  Caption = 'Spellcrafting Help (with AutoBuy)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pumSCHelpMain
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    340
    315)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Realm:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGemName1: TLabel
    Left = 68
    Top = 32
    Width = 66
    Height = 13
    Caption = 'lblGemName1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblGemName2: TLabel
    Left = 68
    Top = 72
    Width = 66
    Height = 13
    Caption = 'lblGemName2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblGemName3: TLabel
    Left = 68
    Top = 112
    Width = 66
    Height = 13
    Caption = 'lblGemName3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblGemName4: TLabel
    Left = 68
    Top = 152
    Width = 66
    Height = 13
    Caption = 'lblGemName4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 0
    Top = 32
    Width = 67
    Height = 13
    Caption = 'Quickbar 2:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 0
    Top = 72
    Width = 67
    Height = 13
    Caption = 'Quickbar 3:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 0
    Top = 112
    Width = 67
    Height = 13
    Caption = 'Quickbar 4:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 0
    Top = 152
    Width = 67
    Height = 13
    Caption = 'Quickbar 5:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTotalSCCost: TLabel
    Left = 136
    Top = 4
    Width = 69
    Height = 13
    Caption = 'lblTotalSCCost'
  end
  object btnQuickbars: TSpeedButton
    Left = 316
    Top = 4
    Width = 23
    Height = 22
    Hint = 'Set quickbars with this configuration'
    Anchors = [akTop, akRight]
    Caption = 'Q'
    ParentShowHint = False
    ShowHint = True
    OnClick = btnQuickbarsClick
  end
  object lblTotalCost: TLabel
    Left = 136
    Top = 16
    Width = 55
    Height = 13
    Caption = 'lblTotalCost'
  end
  object cbxType1: TComboBox
    Tag = 1
    Left = 20
    Top = 48
    Width = 85
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbxTypeChange
    Items.Strings = (
      'Unused'
      'Stat'
      'Resist'
      'Hits'
      'Power'
      'Focus'
      'Skill')
  end
  object cbxPlus1: TComboBox
    Tag = 1
    Left = 108
    Top = 48
    Width = 57
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbxPlusChange
  end
  object cbxEffect1: TComboBox
    Tag = 1
    Left = 168
    Top = 48
    Width = 117
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbxEffectChange
  end
  object cbxRealm: TComboBox
    Left = 40
    Top = 4
    Width = 93
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbxRealmChange
    Items.Strings = (
      'Albion'
      'Midgard'
      'Hibernia')
  end
  object cbxType2: TComboBox
    Tag = 2
    Left = 20
    Top = 84
    Width = 85
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 4
    OnChange = cbxTypeChange
    Items.Strings = (
      'Unused'
      'Stat'
      'Resist'
      'Hits'
      'Power'
      'Focus'
      'Skill')
  end
  object cbxPlus2: TComboBox
    Tag = 2
    Left = 108
    Top = 84
    Width = 57
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 5
    OnChange = cbxPlusChange
  end
  object cbxEffect2: TComboBox
    Tag = 2
    Left = 168
    Top = 84
    Width = 117
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 6
    OnChange = cbxEffectChange
  end
  object cbxType3: TComboBox
    Tag = 3
    Left = 20
    Top = 124
    Width = 85
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 7
    OnChange = cbxTypeChange
    Items.Strings = (
      'Unused'
      'Stat'
      'Resist'
      'Hits'
      'Power'
      'Focus'
      'Skill')
  end
  object cbxPlus3: TComboBox
    Tag = 3
    Left = 108
    Top = 124
    Width = 57
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 8
    OnChange = cbxPlusChange
  end
  object cbxEffect3: TComboBox
    Tag = 3
    Left = 168
    Top = 124
    Width = 117
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 9
    OnChange = cbxEffectChange
  end
  object cbxType4: TComboBox
    Tag = 4
    Left = 20
    Top = 164
    Width = 85
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 10
    OnChange = cbxTypeChange
    Items.Strings = (
      'Unused'
      'Stat'
      'Resist'
      'Hits'
      'Power'
      'Focus'
      'Skill')
  end
  object cbxPlus4: TComboBox
    Tag = 4
    Left = 108
    Top = 164
    Width = 57
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 11
    OnChange = cbxPlusChange
  end
  object cbxEffect4: TComboBox
    Tag = 4
    Left = 168
    Top = 164
    Width = 117
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 12
    OnChange = cbxEffectChange
  end
  object memMaterials: TMemo
    Left = 0
    Top = 188
    Width = 337
    Height = 125
    Anchors = [akLeft, akTop, akBottom]
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'memMaterials')
    ParentFont = False
    PopupMenu = pumMaterialsList
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 13
  end
  object chkQuantity: TCheckBox
    Left = 268
    Top = 8
    Width = 45
    Height = 17
    Caption = 'Q10'
    TabOrder = 14
    OnClick = chkQuantityClick
  end
  object pumMaterialsList: TPopupMenu
    Left = 48
    Top = 204
    object mniRefreshMaterialsList: TMenuItem
      Caption = 'Refresh materials list'
      OnClick = mniRefreshMaterialsListClick
    end
  end
  object pumSCHelpMain: TPopupMenu
    Left = 296
    Top = 76
    object mniSaveToFile: TMenuItem
      Caption = 'Save to file...'
      OnClick = mniSaveToFileClick
    end
    object mniLoadFromFile: TMenuItem
      Caption = 'Load from file...'
      OnClick = mniLoadFromFileClick
    end
  end
  object dlgSCFilename: TOpenDialog
    DefaultExt = 'sch'
    Filter = 'Spellcraft Help files (*.sch)|*.sch|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 296
    Top = 48
  end
end

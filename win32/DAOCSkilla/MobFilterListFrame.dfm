object frmMobFilerList: TfrmMobFilerList
  Left = 0
  Top = 0
  Width = 384
  Height = 119
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clCream
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 0
      Width = 59
      Height = 13
      Caption = 'Mob name'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 264
      Top = 0
      Width = 50
      Height = 13
      Caption = 'Min level'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 322
      Top = 0
      Width = 54
      Height = 13
      Caption = 'Max level'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 144
      Top = 0
      Width = 21
      Height = 13
      Caption = 'Alert'
    end
  end
  object lstMobFilterList: TListBox
    Left = 0
    Top = 17
    Width = 384
    Height = 72
    Style = lbVirtualOwnerDraw
    Align = alTop
    BevelInner = bvNone
    Color = clInfoBk
    Ctl3D = False
    ItemHeight = 16
    ParentCtl3D = False
    TabOrder = 1
    OnClick = lsTMobFilterListClick
    OnDblClick = lblEditClick
    OnDrawItem = lsTMobFilterListDrawItem
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 89
    Width = 384
    Height = 30
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      384
      30)
    object lblAdd: TLabel
      Left = 274
      Top = 2
      Width = 22
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Add'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblAddClick
    end
    object lblEdit: TLabel
      Left = 306
      Top = 2
      Width = 21
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Edit'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblEditClick
    end
    object lblRemove: TLabel
      Left = 334
      Top = 2
      Width = 47
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Remove'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblRemoveClick
    end
  end
end

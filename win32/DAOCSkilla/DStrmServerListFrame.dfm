object frmDStrmServerList: TfrmDStrmServerList
  Left = 0
  Top = 0
  Width = 318
  Height = 236
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 137
    Width = 318
    Height = 99
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      318
      99)
    object lblAdd: TLabel
      Left = 208
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
    end
    object lblEdit: TLabel
      Left = 240
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
    end
    object lblRemove: TLabel
      Left = 268
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
  object lstServers: TListBox
    Left = 0
    Top = 37
    Width = 318
    Height = 100
    Style = lbVirtualOwnerDraw
    Align = alTop
    BorderStyle = bsNone
    Ctl3D = False
    ItemHeight = 16
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 1
    OnClick = lstServersClick
    OnDrawItem = lstServersDrawItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 318
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clCream
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    object Label4: TLabel
      Left = 4
      Top = 2
      Width = 123
      Height = 16
      Caption = 'DStream Servers'
      Color = clCream
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label1: TLabel
      Left = 12
      Top = 20
      Width = 25
      Height = 13
      Caption = 'Host'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 108
      Top = 20
      Width = 36
      Height = 13
      Caption = 'Status'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 240
      Top = 20
      Width = 12
      Height = 13
      Caption = 'In'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 280
      Top = 20
      Width = 20
      Height = 13
      Caption = 'Out'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
  end
end

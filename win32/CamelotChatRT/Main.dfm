object frmMain: TfrmMain
  Left = 333
  Top = 185
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Real-time DAOC Chat logger'
  ClientHeight = 233
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    438
    233)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 4
    Top = 3
    Width = 83
    Height = 13
    Caption = 'Network Adapter:'
  end
  object lblAdapterStatus: TLabel
    Left = 92
    Top = 3
    Width = 77
    Height = 13
    Caption = 'lblAdapterStatus'
  end
  object lblConnectionStatus: TLabel
    Left = 328
    Top = 3
    Width = 94
    Height = 13
    Caption = 'lblConnectionStatus'
  end
  object imgAdapter: TImage
    Left = 196
    Top = 3
    Width = 16
    Height = 12
    AutoSize = True
    Picture.Data = {
      07544269746D6170BA000000424DBA000000000000005A000000280000001000
      00000C000000010004000000000060000000120B0000120B0000090000000900
      0000FF00FF00C6C3C6008482840000FF00000082000000FFFF00008284000000
      FF00000000000000000888888000000000025656800088888882565688802444
      4444144444802488888414848480241241841414148824888884111114882712
      4184144411882488888414881488271241841418118824444444134314802222
      222222222280}
    Transparent = True
    Visible = False
  end
  object Label3: TLabel
    Left = 232
    Top = 3
    Width = 90
    Height = 13
    Caption = 'Connection Status:'
  end
  object lstAdapters: TListBox
    Left = 0
    Top = 20
    Width = 437
    Height = 65
    Style = lbOwnerDrawFixed
    Ctl3D = False
    ItemHeight = 16
    ParentCtl3D = False
    TabOrder = 0
    OnClick = lstAdaptersClick
    OnDrawItem = lstAdaptersDrawItem
  end
  object edtFileName: TEdit
    Left = 92
    Top = 88
    Width = 344
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'c:\mythic\camelot\realchat.log'
  end
  object rbnToFile: TRadioButton
    Tag = 1
    Left = 2
    Top = 90
    Width = 89
    Height = 17
    Caption = 'New chat log'
    TabOrder = 2
    OnClick = rbnToFileClick
  end
  object rbnToScreen: TRadioButton
    Tag = 2
    Left = 2
    Top = 112
    Width = 87
    Height = 17
    Caption = 'To screen'
    TabOrder = 3
    OnClick = rbnToFileClick
  end
  object rbnOff: TRadioButton
    Left = 96
    Top = 112
    Width = 61
    Height = 17
    Caption = 'Off'
    TabOrder = 4
    OnClick = rbnToFileClick
  end
  object memLog: TMemo
    Left = 0
    Top = 132
    Width = 437
    Height = 101
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
end

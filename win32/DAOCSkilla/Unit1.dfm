object frmMain: TfrmMain
  Left = 550
  Top = 223
  Width = 510
  Height = 307
  Caption = 'DAOC Skilla'
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    502
    280)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlayerPos: TLabel
    Left = 340
    Top = 60
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerPos'
  end
  object lblPlayerHeadSpeed: TLabel
    Left = 340
    Top = 76
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblPlayerHeadSpeed'
  end
  object imgAdapter: TImage
    Left = 284
    Top = 64
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
  object lblZone: TLabel
    Left = 340
    Top = 92
    Width = 159
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblZone'
  end
  object lstAdapters: TListBox
    Left = 4
    Top = 4
    Width = 493
    Height = 49
    Style = lbOwnerDrawFixed
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnClick = lstAdaptersClick
    OnDrawItem = lstAdaptersDrawItem
  end
  object Memo1: TMemo
    Left = 4
    Top = 136
    Width = 493
    Height = 141
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnGLRender: TButton
    Left = 8
    Top = 76
    Width = 75
    Height = 25
    Caption = 'E&xcalibur'
    TabOrder = 2
    OnClick = btnGLRenderClick
  end
  object btnDebugging: TButton
    Left = 84
    Top = 76
    Width = 75
    Height = 25
    Caption = '&Debugging'
    TabOrder = 3
    OnClick = btnDebuggingClick
  end
  object chkAutolaunchExcal: TCheckBox
    Left = 8
    Top = 56
    Width = 169
    Height = 17
    Caption = 'Autolaunch Excalibur windows'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkChatLog: TCheckBox
    Left = 8
    Top = 110
    Width = 105
    Height = 17
    Caption = 'Realtime chat.log'
    TabOrder = 5
    OnClick = chkChatLogClick
  end
  object edtChatLogFile: TEdit
    Left = 116
    Top = 108
    Width = 381
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = 'c:\mythic\isles\realchat.log'
  end
  object btnMacroing: TButton
    Left = 160
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Macroing'
    TabOrder = 7
    OnClick = btnMacroingClick
  end
end

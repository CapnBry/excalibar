object frmConnectionConfig: TfrmConnectionConfig
  Left = 325
  Top = 292
  BorderStyle = bsDialog
  Caption = 'Connection Configuration'
  ClientHeight = 375
  ClientWidth = 518
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
  object grpSniff: TGroupBox
    Left = 4
    Top = 24
    Width = 509
    Height = 193
    TabOrder = 0
    object lblPromisc: TLabel
      Left = 28
      Top = 110
      Width = 461
      Height = 13
      Caption = 
        'Use if you are sniffing from machine other than the DAOC client,' +
        ' and you are using a network hub.'
    end
    object imgAdapter: TImage
      Left = 336
      Top = 156
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
    object lstAdapters: TListBox
      Left = 8
      Top = 20
      Width = 493
      Height = 69
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
      TabOrder = 1
      OnDrawItem = lstAdaptersDrawItem
    end
    object rbnProcessPackets: TRadioButton
      Left = 8
      Top = 128
      Width = 189
      Height = 17
      Caption = 'Process packets locally'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object rbnProcessRemotely: TRadioButton
      Left = 8
      Top = 148
      Width = 189
      Height = 17
      Caption = 'Send packets to another machine'
      TabOrder = 4
    end
    object chkPromiscuous: TCheckBox
      Left = 8
      Top = 96
      Width = 193
      Height = 17
      Caption = 'Promiscuous network capture'
      TabOrder = 2
    end
    object edtRemoteMachine: TEdit
      Left = 28
      Top = 164
      Width = 169
      Height = 21
      TabOrder = 5
      Text = 'localhost:9867'
    end
    object chkSniffPackets: TCheckBox
      Left = 12
      Top = 0
      Width = 185
      Height = 17
      Caption = 'Sniff packets directly from adapter'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkSniffPacketsClick
    end
  end
  object grpRecv: TGroupBox
    Left = 4
    Top = 220
    Width = 509
    Height = 65
    Caption = '  Receive packets from another machine  '
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 64
      Height = 13
      Caption = 'Listen on port'
    end
    object Label1: TLabel
      Left = 160
      Top = 18
      Width = 341
      Height = 41
      AutoSize = False
      Caption = 
        'Note:  Data received from other machines will only be processed ' +
        'if either "Sniff packets" is disabled or  "Process packets local' +
        'ly" mode is selected.'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsItalic]
      ParentFont = False
      WordWrap = True
    end
    object edtLocalPort: TEdit
      Left = 80
      Top = 20
      Width = 69
      Height = 21
      TabOrder = 0
      Text = '9867'
      OnKeyPress = edtLocalPortKeyPress
    end
  end
  object btnOK: TBitBtn
    Left = 436
    Top = 344
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    Kind = bkOK
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 288
    Width = 509
    Height = 40
    Caption = '  Server subnet mask  '
    TabOrder = 3
    object rbnUSServers: TRadioButton
      Left = 8
      Top = 16
      Width = 129
      Height = 17
      Caption = 'United States servers'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbnCustomServersClick
    end
    object rbnEUServers: TRadioButton
      Tag = 1
      Left = 164
      Top = 16
      Width = 113
      Height = 17
      Caption = 'European servers'
      TabOrder = 1
      OnClick = rbnCustomServersClick
    end
    object rbnCustomServers: TRadioButton
      Tag = 2
      Left = 304
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Custom subnet'
      TabOrder = 2
      OnClick = rbnCustomServersClick
    end
    object edtServerSubnet: TEdit
      Left = 404
      Top = 12
      Width = 93
      Height = 21
      TabOrder = 3
      Text = '192.168.100.0'
      OnKeyPress = edtServerSubnetKeyPress
    end
  end
  object chkRemoteAdmin: TCheckBox
    Left = 12
    Top = 336
    Width = 241
    Height = 17
    Caption = 'Enable remote control telnet server'
    TabOrder = 4
  end
  object chkEnableInject: TCheckBox
    Left = 16
    Top = 2
    Width = 313
    Height = 17
    Caption = 'Read packets directly from DAoC process'
    TabOrder = 5
    OnClick = chkEnableInjectClick
  end
end

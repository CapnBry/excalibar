object frmDebugging: TfrmDebugging
  Left = 259
  Top = 186
  Width = 432
  Height = 162
  Caption = 'Tracing and Debugging'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblFilePos: TLabel
    Left = 248
    Top = 88
    Width = 44
    Height = 13
    Caption = 'lblFilePos'
  end
  object Bevel1: TBevel
    Left = 4
    Top = 48
    Width = 413
    Height = 9
    Shape = bsTopLine
  end
  object edtPlayback: TEdit
    Left = 4
    Top = 60
    Width = 237
    Height = 21
    TabOrder = 0
    Text = 'C:\savedcap.cap'
  end
  object btnOpenPlayback: TButton
    Left = 248
    Top = 60
    Width = 77
    Height = 25
    Caption = 'Reset file'
    TabOrder = 1
    OnClick = btnOpenPlaybackClick
  end
  object chkPCAPFile: TCheckBox
    Left = 4
    Top = 84
    Width = 237
    Height = 17
    Caption = 'Process as libpcap file'
    TabOrder = 2
  end
  object btnPlayTimer: TButton
    Left = 4
    Top = 104
    Width = 97
    Height = 25
    Caption = 'Play packet&s'
    TabOrder = 3
    OnClick = btnPlayTimerClick
  end
  object btnPlayPacket: TButton
    Left = 104
    Top = 104
    Width = 89
    Height = 25
    Caption = 'Play one packe&t'
    TabOrder = 4
    OnClick = btnPlayPacketClick
  end
  object chkRecordMobseen: TCheckBox
    Left = 228
    Top = 24
    Width = 189
    Height = 17
    Caption = 'Capture mob updates to mobseen'
    TabOrder = 5
    OnClick = chkRecordMobseenClick
  end
  object chkDumpPackets: TCheckBox
    Left = 4
    Top = 24
    Width = 189
    Height = 17
    Caption = 'Dump packet data to log window'
    TabOrder = 6
  end
  object chkCapture: TCheckBox
    Left = 228
    Top = 4
    Width = 189
    Height = 17
    Caption = 'Capture packets to mycap.cap'
    TabOrder = 7
    OnClick = chkCaptureClick
  end
  object chkProcessPackets: TCheckBox
    Left = 4
    Top = 4
    Width = 213
    Height = 17
    Caption = 'Process packet data (don'#39't just capture)'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chkProcessPacketsClick
  end
  object btnDumpMobs: TButton
    Left = 344
    Top = 106
    Width = 75
    Height = 25
    Caption = 'Dump mobs'
    TabOrder = 9
    OnClick = btnDumpMobsClick
  end
  object tmrPlayback: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrPlaybackTimer
    Left = 196
    Top = 16
  end
end
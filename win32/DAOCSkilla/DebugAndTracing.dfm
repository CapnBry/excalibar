object frmDebugging: TfrmDebugging
  Left = 259
  Top = 186
  BorderStyle = bsSingle
  Caption = 'Tracing and Debugging'
  ClientHeight = 127
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblFilePos: TLabel
    Left = 244
    Top = 84
    Width = 44
    Height = 13
    Caption = 'lblFilePos'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 44
    Width = 413
    Height = 9
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 324
    Top = 68
    Width = 89
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Playback speed'
  end
  object edtPlayback: TEdit
    Left = 0
    Top = 56
    Width = 237
    Height = 21
    TabOrder = 0
    Text = 'C:\savedcap.cap'
  end
  object btnOpenPlayback: TButton
    Left = 244
    Top = 56
    Width = 77
    Height = 25
    Caption = 'Reset file'
    TabOrder = 1
    OnClick = btnOpenPlaybackClick
  end
  object chkPCAPFile: TCheckBox
    Left = 0
    Top = 80
    Width = 237
    Height = 17
    Caption = 'Process as libpcap file'
    TabOrder = 2
    OnClick = chkPCAPFileClick
  end
  object btnPlayTimer: TButton
    Left = 88
    Top = 100
    Width = 97
    Height = 25
    Caption = 'Play packet&s'
    TabOrder = 3
    OnClick = btnPlayTimerClick
  end
  object btnPlayPacket: TButton
    Left = 0
    Top = 100
    Width = 89
    Height = 25
    Caption = 'Play one packe&t'
    TabOrder = 4
    OnClick = btnPlayPacketClick
  end
  object chkRecordMobseen: TCheckBox
    Left = 224
    Top = 20
    Width = 189
    Height = 17
    Caption = 'Capture mob updates to mobseen'
    TabOrder = 5
    OnClick = chkRecordMobseenClick
  end
  object chkDumpPackets: TCheckBox
    Left = 0
    Top = 20
    Width = 189
    Height = 17
    Caption = 'Dump packet data to log window'
    TabOrder = 6
  end
  object chkCapture: TCheckBox
    Left = 224
    Top = 0
    Width = 189
    Height = 17
    Caption = 'Capture packets to mycap.cap'
    TabOrder = 7
    OnClick = chkCaptureClick
  end
  object chkProcessPackets: TCheckBox
    Left = 0
    Top = 0
    Width = 213
    Height = 17
    Caption = 'Process packet data (don'#39't just capture)'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chkProcessPacketsClick
  end
  object btnDumpMobs: TButton
    Left = 340
    Top = 100
    Width = 75
    Height = 25
    Caption = 'Dump mobs'
    TabOrder = 9
    OnClick = btnDumpMobsClick
  end
  object trackPlaySpeed: TTrackBar
    Left = 324
    Top = 52
    Width = 89
    Height = 17
    Max = 500
    Min = 1
    Orientation = trHorizontal
    PageSize = 100
    Frequency = 100
    Position = 50
    SelEnd = 0
    SelStart = 0
    TabOrder = 10
    ThumbLength = 10
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = trackPlaySpeedChange
  end
  object tmrPlayback: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrPlaybackTimer
    Left = 196
    Top = 16
  end
end

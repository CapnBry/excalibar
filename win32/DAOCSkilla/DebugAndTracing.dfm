object frmDebugging: TfrmDebugging
  Left = 262
  Top = 212
  Width = 425
  Height = 169
  HorzScrollBar.Range = 415
  VertScrollBar.Range = 141
  ActiveControl = edtPlayback
  AutoScroll = False
  Caption = 'Tracing and Debugging'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 60
    Width = 413
    Height = 9
    Shape = bsTopLine
  end
  object lblFilePos: TLabel
    Left = 244
    Top = 100
    Width = 61
    Height = 13
    Caption = 'File not open'
  end
  object Label1: TLabel
    Left = 324
    Top = 88
    Width = 89
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Playback speed'
  end
  object edtPlayback: TEdit
    Left = 0
    Top = 72
    Width = 237
    Height = 21
    TabOrder = 0
    Text = 'savedcap.cap'
  end
  object btnOpenPlayback: TButton
    Left = 244
    Top = 72
    Width = 77
    Height = 25
    Caption = 'Reset file'
    TabOrder = 1
    OnClick = btnOpenPlaybackClick
  end
  object chkPCAPFile: TCheckBox
    Left = 0
    Top = 96
    Width = 237
    Height = 17
    Caption = 'Process as libpcap file'
    TabOrder = 2
    OnClick = chkPCAPFileClick
  end
  object btnPlayTimer: TButton
    Left = 88
    Top = 116
    Width = 97
    Height = 25
    Caption = 'Play packet&s'
    TabOrder = 3
    OnClick = btnPlayTimerClick
  end
  object btnPlayPacket: TButton
    Left = 0
    Top = 116
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
    Top = 116
    Width = 75
    Height = 25
    Caption = 'Dump mobs'
    TabOrder = 9
    OnClick = btnDumpMobsClick
  end
  object trackPlaySpeed: TTrackBar
    Left = 324
    Top = 68
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
    ThumbLength = 12
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = trackPlaySpeedChange
  end
  object chkRecordDelveseen: TCheckBox
    Left = 224
    Top = 40
    Width = 189
    Height = 17
    Caption = 'Capture item delves to delveseen'
    TabOrder = 11
    OnClick = chkRecordDelveseenClick
  end
  object tmrPlayback: TTimer
    Enabled = False
    Interval = 50
    OnTimer = tmrPlaybackTimer
    Left = 60
    Top = 24
  end
end

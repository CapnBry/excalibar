object frmGLRender: TfrmGLRender
  Left = 358
  Top = 125
  Width = 843
  Height = 643
  Caption = 'Excalibur'
  Color = clBtnFace
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMap: TPanel
    Left = 217
    Top = 0
    Width = 618
    Height = 616
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object glMap: TglWindow
      Left = 0
      Top = 0
      Width = 618
      Height = 592
      Align = alClient
      OnClick = glMapClick
      OnMouseDown = glMapMouseDown
      OnMouseMove = glMapMouseMove
      ColorDepth = c16bits
      DepthBits = c16bits
      DepthBufferEnabled = False
      StencBits = c16bits
      StencBufferEnabled = False
      AccumBits = c16bits
      AccumBufferEnabled = False
      WindowFlags = [wfDrawToWindow, wfSupportOpenGL, wfGenericAccelerated, wfDoubleBuffer]
      OnResize = glMapResize
      OnDraw = glMapDraw
      OnInit = glMapInit
    end
    object slideZoom: TTrackBar
      Left = 0
      Top = 592
      Width = 618
      Height = 24
      Align = alBottom
      Ctl3D = True
      Max = 65000
      Min = 200
      Orientation = trHorizontal
      ParentCtl3D = False
      PageSize = 500
      Frequency = 750
      Position = 8000
      SelEnd = 0
      SelStart = 0
      TabOrder = 1
      TickMarks = tmBottomRight
      TickStyle = tsNone
      OnChange = slideZoomChange
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 616
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Mob List'
    DragKind = dkDock
    DragMode = dmAutomatic
    TabOrder = 1
    OnEndDock = pnlLeftEndDock
    object lblObjCounts: TLabel
      Left = 0
      Top = 0
      Width = 217
      Height = 25
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Albs: 999    Mids: 999'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnMouseDown = MobListMouseDown
    end
    object pnlGridHeader: TPanel
      Left = 0
      Top = 25
      Width = 217
      Height = 16
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Name                                    Level   Health'
      TabOrder = 0
      OnMouseDown = MobListMouseDown
    end
  end
  object tmrMinFPS: TTimer
    Interval = 500
    OnTimer = tmrMinFPSTimer
    Left = 32
    Top = 136
  end
end

object frmGLRender: TfrmGLRender
  Left = 358
  Top = 125
  Width = 843
  Height = 643
  Caption = 'Excalibur -- Win32'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object glMap: TglWindow
    Left = 149
    Top = 0
    Width = 686
    Height = 592
    Align = alClient
    OnClick = glMapClick
    ColorDepth = c16bits
    DepthBits = c16bits
    DepthBufferEnabled = True
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
    Width = 835
    Height = 24
    Align = alBottom
    Max = 65000
    Min = 200
    Orientation = trHorizontal
    Frequency = 1000
    Position = 6000
    SelEnd = 0
    SelStart = 0
    TabOrder = 1
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = slideZoomChange
  end
  object lstMobs: TListBox
    Left = 0
    Top = 0
    Width = 149
    Height = 592
    Style = lbOwnerDrawFixed
    Align = alLeft
    Color = clBtnFace
    Ctl3D = False
    ItemHeight = 14
    ParentCtl3D = False
    TabOrder = 2
    OnDrawItem = lstMobsDrawItem
  end
end

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
    Left = 221
    Top = 0
    Width = 614
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
  object grdObjects: TDrawGrid
    Left = 0
    Top = 0
    Width = 221
    Height = 592
    Align = alLeft
    ColCount = 3
    DefaultRowHeight = 17
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goRowSelect, goThumbTracking]
    TabOrder = 2
    OnClick = grdObjectsClick
    OnDrawCell = grdObjectsDrawCell
    ColWidths = (
      145
      33
      37)
  end
end

object frmGLRender: TfrmGLRender
  Left = 358
  Top = 125
  Width = 843
  Height = 643
  Caption = 'Ojects in the vicinity'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object glMap: TglWindow
    Left = 137
    Top = 0
    Width = 698
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
    WindowFlags = [wfDrawToWindow, wfSupportOpenGL, wfDoubleBuffer]
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
    Position = 3000
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
    Width = 137
    Height = 592
    Align = alLeft
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 2
  end
end

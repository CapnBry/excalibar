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
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
      TabOrder = 0
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
    object pnlInventory: TPanel
      Left = 0
      Top = 472
      Width = 217
      Height = 144
      Align = alBottom
      BevelOuter = bvNone
      Color = clBtnShadow
      TabOrder = 1
      object lblInventory: TLabel
        Left = 0
        Top = 13
        Width = 217
        Height = 131
        Align = alClient
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblInventoryHeader: TLabel
        Left = 0
        Top = 0
        Width = 217
        Height = 13
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Selected Player Inventory'
        Color = clBtnFace
        ParentColor = False
      end
    end
  end
  object tmrMinFPS: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrMinFPSTimer
    Left = 32
    Top = 136
  end
  object pumRadar: TPopupMenu
    OnPopup = pumRadarPopup
    Left = 300
    Top = 144
    object mniForceContextCurrent: TMenuItem
      Caption = 'Force context current'
      OnClick = mniForceContextCurrentClick
    end
    object mniShowZoneInfo: TMenuItem
      Caption = 'Show zone information'
      OnClick = mniShowZoneInfoClick
    end
  end
  object atlRadarKeys: TActionList
    Left = 32
    Top = 68
    object atnShowInventory: TAction
      Category = 'Extra Info'
      Caption = 'Show inventory'
      ShortCut = 9
      OnExecute = atnShowInventoryExecute
    end
    object atnInvaderWarningToggle: TAction
      Category = 'UI toggles'
      Caption = 'Invader warning'
      ShortCut = 65
      OnExecute = atnInvaderWarningToggleExecute
    end
    object atnDrawTextureToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw texture maps'
      ShortCut = 66
      OnExecute = atnDrawTextureToggleExecute
    end
    object atnDrawRangeCirclesToggle: TAction
      Category = 'UI toggles'
      Caption = 'Draw rance circles'
      ShortCut = 67
      OnExecute = atnDrawRangeCirclesToggleExecute
    end
    object atnDrawAIDestinationToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw AI destination'
      ShortCut = 68
      OnExecute = atnDrawAIDestinationToggleExecute
    end
    object atnDrawFriendlyPlayersToggle: TAction
      Category = 'Filter'
      Caption = 'Draw friendly players'
      ShortCut = 70
      OnExecute = atnDrawFriendlyPlayersToggleExecute
    end
    object atnDrawGridToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw grid'
      ShortCut = 71
      OnExecute = atnDrawGridToggleExecute
    end
    object atnDrawHUDToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw HUD'
      ShortCut = 72
      OnExecute = atnDrawHUDToggleExecute
    end
    object atnAddPushPin: TAction
      Category = 'Extra Info'
      Caption = 'Add pushpin'
      ShortCut = 73
      OnExecute = atnAddPushPinExecute
    end
    object atnDrawMobsToggle: TAction
      Category = 'Filter'
      Caption = 'Draw mobs'
      ShortCut = 77
      OnExecute = atnDrawMobsToggleExecute
    end
    object atnDrawObjectsToggle: TAction
      Category = 'Filter'
      Caption = 'Draw objects'
      ShortCut = 79
      OnExecute = atnDrawObjectsToggleExecute
    end
    object atnDrawPlayersToggle: TAction
      Category = 'Filter'
      Caption = 'Draw players'
      ShortCut = 80
      OnExecute = atnDrawPlayersToggleExecute
    end
    object atnDrawRulersToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw rulers'
      ShortCut = 82
      OnExecute = atnDrawRulersToggleExecute
    end
    object atnStayOnTopToggle: TAction
      Category = 'UI toggles'
      Caption = 'Stay on top'
      ShortCut = 84
      OnExecute = atnStayOnTopToggleExecute
    end
    object atnDrawUnknownToggle: TAction
      Category = 'Filter'
      Caption = 'Draw unknown'
      ShortCut = 85
      OnExecute = atnDrawUnknownToggleExecute
    end
    object atnDrawTypeTagToggle: TAction
      Category = 'UI toggles'
      Caption = 'Draw typetag'
      ShortCut = 89
      OnExecute = atnDrawTypeTagToggleExecute
    end
    object atnDrawVectorToggle: TAction
      Category = 'Extra Info'
      Caption = 'Draw vector maps'
      ShortCut = 86
      OnExecute = atnDrawVectorToggleExecute
    end
    object atnNextConnection: TAction
      Caption = 'Next connection'
      ShortCut = 122
      OnExecute = atnNextConnectionExecute
    end
    object atnPreviousConnection: TAction
      Caption = 'Previous connection'
      ShortCut = 8314
      OnExecute = atnPreviousConnectionExecute
    end
    object atnZoomIn: TAction
      Category = 'Movement'
      Caption = 'Zoom in'
      ShortCut = 34
      OnExecute = atnZoomInExecute
    end
    object atnZoomOut: TAction
      Category = 'Movement'
      Caption = 'Zoom out'
      ShortCut = 33
      OnExecute = atnZoomOutExecute
    end
    object atnRecenterMap: TAction
      Category = 'Movement'
      Caption = 'Recenter map'
      ShortCut = 36
      OnExecute = atnRecenterMapExecute
    end
    object atnScrollLeft: TAction
      Category = 'Movement'
      Caption = 'Scroll left'
      ShortCut = 37
      OnExecute = atnScrollLeftExecute
    end
    object atnScrollRight: TAction
      Category = 'Movement'
      Caption = 'Scroll right'
      ShortCut = 39
      OnExecute = atnScrollRightExecute
    end
    object atnScrollUp: TAction
      Category = 'Movement'
      Caption = 'Scroll up'
      ShortCut = 38
      OnExecute = atnScrollUpExecute
    end
    object atnScrollDown: TAction
      Category = 'Movement'
      Caption = 'Scroll down'
      ShortCut = 40
      OnExecute = atnScrollDownExecute
    end
    object atnShowPrefsDialog: TAction
      Caption = 'Show Render Preferences dialog'
      ShortCut = 112
      OnExecute = atnShowPrefsDialogExecute
    end
    object atnShowHideMobList: TAction
      Category = 'Mob List'
      Caption = 'Show / Hide mob list'
      ShortCut = 113
      OnExecute = atnShowHideMobListExecute
    end
    object atnScreenShot: TAction
      Caption = 'Screenshot'
      ShortCut = 123
      OnExecute = atnScreenShotExecute
    end
  end
end

object frmRenderPrefs: TfrmRenderPrefs
  Left = 326
  Top = 257
  BorderStyle = bsToolWindow
  Caption = 'Render Preferences'
  ClientHeight = 393
  ClientWidth = 467
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
  object grpExtras: TGroupBox
    Left = 4
    Top = 144
    Width = 457
    Height = 245
    Caption = ' Include this extra information '
    TabOrder = 1
    object Label1: TLabel
      Left = 28
      Top = 32
      Width = 290
      Height = 13
      Caption = 'Vector maps are the line art maps.  Place them in maps\*.map'
    end
    object Label2: TLabel
      Left = 28
      Top = 64
      Width = 391
      Height = 13
      Caption = 
        'The image map.  They should be in DXTC1 format, no MIPmaps in ma' +
        'ps\dds\*.dds'
    end
    object Label3: TLabel
      Left = 28
      Top = 96
      Width = 385
      Height = 13
      Caption = 
        'The cocentric range circles drawn around your character at 500, ' +
        '1500, and 6000.'
    end
    object Label4: TLabel
      Left = 28
      Top = 128
      Width = 178
      Height = 13
      Caption = 'The vertical and horizontal crosshairs.'
    end
    object Label5: TLabel
      Left = 28
      Top = 160
      Width = 358
      Height = 13
      Caption = 
        'The panel containing information about the selected object (requ' +
        'ires GLUT).'
    end
    object Label6: TLabel
      Left = 28
      Top = 192
      Width = 258
      Height = 13
      Caption = 'Draw a line to an AI object'#39's destination while en-route.'
    end
    object Label7: TLabel
      Left = 28
      Top = 224
      Width = 400
      Height = 13
      Caption = 
        'Draw a green area which shows the visible area of the DAOC rende' +
        'rer (far clip plane)'
    end
    object chkVectorMaps: TCheckBox
      Left = 8
      Top = 16
      Width = 241
      Height = 17
      Caption = 'Vector maps (V)'
      TabOrder = 0
      OnClick = chkVectorMapsClick
    end
    object chkTextureMaps: TCheckBox
      Left = 8
      Top = 48
      Width = 241
      Height = 17
      Caption = 'Background image maps (B)'
      TabOrder = 1
      OnClick = chkTextureMapsClick
    end
    object chkRangeCircles: TCheckBox
      Left = 8
      Top = 80
      Width = 241
      Height = 17
      Caption = 'Range circles (C)'
      TabOrder = 2
      OnClick = chkRangeCirclesClick
    end
    object chkRulers: TCheckBox
      Left = 8
      Top = 112
      Width = 241
      Height = 17
      Caption = 'Map rulers (R)'
      TabOrder = 3
      OnClick = chkRulersClick
    end
    object chkHUD: TCheckBox
      Left = 8
      Top = 144
      Width = 241
      Height = 17
      Caption = 'Selected object HUD (H)'
      TabOrder = 4
      OnClick = chkHUDClick
    end
    object chkDestination: TCheckBox
      Left = 8
      Top = 176
      Width = 241
      Height = 17
      Caption = 'AI destination vector (D)'
      TabOrder = 5
      OnClick = chkDestinationClick
    end
    object chkViewFrustum: TCheckBox
      Left = 8
      Top = 208
      Width = 241
      Height = 17
      Caption = 'Show Dark Age renderer "visible" area'
      TabOrder = 6
      OnClick = chkViewFrustumClick
    end
  end
  object grpObjects: TGroupBox
    Left = 4
    Top = 0
    Width = 165
    Height = 137
    Caption = ' Show these types of objects'
    TabOrder = 0
    object chkRenderPlayers: TCheckBox
      Tag = 3
      Left = 8
      Top = 20
      Width = 141
      Height = 17
      Caption = 'Players (P)'
      TabOrder = 0
      OnClick = ObjectFilterClick
    end
    object chkRenderMobs: TCheckBox
      Tag = 2
      Left = 8
      Top = 48
      Width = 141
      Height = 17
      Caption = 'Monsters and NPCs (M)'
      TabOrder = 1
      OnClick = ObjectFilterClick
    end
    object chkRenderObjects: TCheckBox
      Tag = 1
      Left = 8
      Top = 76
      Width = 141
      Height = 17
      Caption = 'Graves and objects (O)'
      TabOrder = 2
      OnClick = ObjectFilterClick
    end
    object chkRenderUnknown: TCheckBox
      Left = 8
      Top = 104
      Width = 141
      Height = 17
      Caption = 'Unknown (U)'
      TabOrder = 3
      OnClick = ObjectFilterClick
    end
  end
  object btnOK: TBitBtn
    Left = 372
    Top = 16
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 3
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 372
    Top = 56
    Width = 75
    Height = 25
    Caption = 'C&ancel'
    TabOrder = 4
    Kind = bkCancel
  end
  object grpUIOptions: TGroupBox
    Left = 176
    Top = 0
    Width = 177
    Height = 137
    Caption = ' UI options'
    TabOrder = 2
    object chkTrackMapClick: TCheckBox
      Left = 8
      Top = 16
      Width = 153
      Height = 17
      Caption = 'Click in map selects object'
      TabOrder = 0
      OnClick = chkTrackMapClickClick
    end
    object chkTrackGameSelection: TCheckBox
      Left = 8
      Top = 36
      Width = 153
      Height = 17
      Caption = 'Track in-game selection'
      TabOrder = 1
      OnClick = chkTrackGameSelectionClick
    end
    object chkTypeTag: TCheckBox
      Left = 8
      Top = 56
      Width = 153
      Height = 17
      Caption = 'Overlay NPC type tag (Y)'
      TabOrder = 2
      OnClick = chkTypeTagClick
    end
    object chkStayOnTop: TCheckBox
      Left = 8
      Top = 76
      Width = 153
      Height = 17
      Caption = 'Keep window on top (T)'
      TabOrder = 3
      OnClick = chkStayOnTopClick
    end
    object chkRotateMap: TCheckBox
      Left = 8
      Top = 96
      Width = 153
      Height = 17
      Caption = 'Rotate map with player'
      TabOrder = 4
      OnClick = chkRotateMapClick
    end
    object chkAdjacentZones: TCheckBox
      Left = 8
      Top = 116
      Width = 161
      Height = 17
      Caption = 'Draw adjacent zones'
      TabOrder = 5
      OnClick = chkAdjacentZonesClick
    end
  end
end

object frmRenderPrefs: TfrmRenderPrefs
  Left = 350
  Top = 201
  BorderStyle = bsToolWindow
  Caption = 'Render Preferences'
  ClientHeight = 342
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 264
    Top = 312
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 1
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 340
    Top = 312
    Width = 75
    Height = 25
    Caption = 'C&ancel'
    TabOrder = 2
    Kind = bkCancel
  end
  object pagePrefs: TPageControl
    Left = 0
    Top = 0
    Width = 419
    Height = 305
    ActivePage = tabGraphics
    Align = alTop
    MultiLine = True
    TabIndex = 5
    TabOrder = 0
    object tabOptions: TTabSheet
      Caption = 'UI options'
      ImageIndex = 1
      object Label10: TLabel
        Left = 26
        Top = 24
        Width = 374
        Height = 13
        Caption = 
          'Clicking on the map will update the selection to the object clos' +
          'est to the mouse.'
      end
      object Label11: TLabel
        Left = 26
        Top = 60
        Width = 259
        Height = 13
        Caption = 'Selection follows whatever you have selected in-game.'
      end
      object Label12: TLabel
        Left = 26
        Top = 96
        Width = 324
        Height = 13
        Caption = 
          'NPCs will have their class typetag displayed on the map and mob ' +
          'list.'
      end
      object Label13: TLabel
        Left = 26
        Top = 136
        Width = 247
        Height = 13
        Caption = 'Keep the Excalibur window on top of other windows.'
      end
      object Label14: TLabel
        Left = 26
        Top = 176
        Width = 276
        Height = 13
        Caption = 'The map turns with the player instead of keeping North up.'
      end
      object Label15: TLabel
        Left = 26
        Top = 216
        Width = 277
        Height = 13
        Caption = 'Draw zones adjacent to your current zone in the map view.'
      end
      object Label8: TLabel
        Left = 26
        Top = 254
        Width = 320
        Height = 13
        Caption = 
          'Play invader.wav sound every time a new invader is seen.  Max fr' +
          'eq'
      end
      object Label20: TLabel
        Left = 392
        Top = 254
        Width = 5
        Height = 13
        Caption = 's'
      end
      object chkAdjacentZones: TCheckBox
        Left = 8
        Top = 200
        Width = 161
        Height = 17
        Caption = 'Draw adjacent zones'
        TabOrder = 5
        OnClick = chkAdjacentZonesClick
      end
      object chkTypeTag: TCheckBox
        Left = 8
        Top = 80
        Width = 153
        Height = 17
        Caption = 'Show NPC type tag (Y)'
        TabOrder = 2
        OnClick = chkTypeTagClick
      end
      object chkTrackMapClick: TCheckBox
        Left = 8
        Top = 8
        Width = 153
        Height = 17
        Caption = 'Click in map selects object'
        TabOrder = 0
        OnClick = chkTrackMapClickClick
      end
      object chkTrackGameSelection: TCheckBox
        Left = 8
        Top = 44
        Width = 153
        Height = 17
        Caption = 'Track in-game selection'
        TabOrder = 1
        OnClick = chkTrackGameSelectionClick
      end
      object chkStayOnTop: TCheckBox
        Left = 8
        Top = 120
        Width = 153
        Height = 17
        Caption = 'Keep window on top (T)'
        TabOrder = 3
        OnClick = chkStayOnTopClick
      end
      object chkRotateMap: TCheckBox
        Left = 8
        Top = 160
        Width = 153
        Height = 17
        Caption = 'Rotate map with player'
        TabOrder = 4
        OnClick = chkRotateMapClick
      end
      object chkInvaderWarn: TCheckBox
        Left = 8
        Top = 236
        Width = 241
        Height = 17
        Caption = 'Invader warning sound (A)'
        TabOrder = 6
        OnClick = chkInvaderWarnClick
      end
      object edtInvaderWarnTicks: TEdit
        Left = 352
        Top = 250
        Width = 37
        Height = 21
        TabOrder = 7
        Text = 'edtInvaderWarnTicks'
        OnExit = edtInvaderWarnTicksExit
        OnKeyPress = edtInvaderWarnTicksKeyPress
      end
    end
    object tabExtras: TTabSheet
      Caption = 'Extra information'
      ImageIndex = 2
      object Label7: TLabel
        Left = 26
        Top = 228
        Width = 339
        Height = 13
        Caption = 
          'Draw a green area which shows the visible area in DAOC (far clip' +
          ' plane)'
      end
      object Label6: TLabel
        Left = 26
        Top = 196
        Width = 258
        Height = 13
        Caption = 'Draw a line to an AI object'#39's destination while en-route.'
      end
      object Label5: TLabel
        Left = 26
        Top = 164
        Width = 329
        Height = 13
        Caption = 
          'The panel containing details about the selected object and the p' +
          'layer.'
      end
      object Label4: TLabel
        Left = 26
        Top = 132
        Width = 178
        Height = 13
        Caption = 'The vertical and horizontal crosshairs.'
      end
      object Label2: TLabel
        Left = 26
        Top = 100
        Width = 358
        Height = 13
        Caption = 
          'The image map.  Should be in DXTC1 format, no mipmap in maps\dds' +
          '\*.dds'
      end
      object Label1: TLabel
        Left = 26
        Top = 20
        Width = 290
        Height = 13
        Caption = 'Vector maps are the line art maps.  Place them in maps\*.map'
      end
      object Label16: TLabel
        Left = 26
        Top = 260
        Width = 239
        Height = 13
        Caption = 'Draw a 10k x 10k overlay grid on the current zone.'
      end
      object Label23: TLabel
        Left = 28
        Top = 68
        Width = 382
        Height = 13
        Caption = 
          'Display points added using the "Add Pushpin" dialog (press i fro' +
          'm the map dialog)'
      end
      object chkViewFrustum: TCheckBox
        Left = 8
        Top = 212
        Width = 241
        Height = 17
        Caption = 'Show Dark Age renderer "visible" area'
        TabOrder = 5
        OnClick = chkViewFrustumClick
      end
      object chkVectorMaps: TCheckBox
        Left = 8
        Top = 4
        Width = 241
        Height = 17
        Caption = 'Vector maps (V)'
        TabOrder = 0
        OnClick = chkVectorMapsClick
      end
      object chkTextureMaps: TCheckBox
        Left = 8
        Top = 84
        Width = 241
        Height = 17
        Caption = 'Background image maps (B)'
        TabOrder = 1
        OnClick = chkTextureMapsClick
      end
      object chkRulers: TCheckBox
        Left = 8
        Top = 116
        Width = 241
        Height = 17
        Caption = 'Map rulers (R)'
        TabOrder = 2
        OnClick = chkRulersClick
      end
      object chkHUD: TCheckBox
        Left = 8
        Top = 148
        Width = 241
        Height = 17
        Caption = 'Selected object and player  HUD (H)'
        TabOrder = 3
        OnClick = chkHUDClick
      end
      object chkDrawGrid: TCheckBox
        Left = 8
        Top = 244
        Width = 241
        Height = 17
        Caption = 'Draw location grid over background (G)'
        TabOrder = 6
        OnClick = chkDrawGridClick
      end
      object chkDestination: TCheckBox
        Left = 8
        Top = 180
        Width = 241
        Height = 17
        Caption = 'AI destination vector (D)'
        TabOrder = 4
        OnClick = chkDestinationClick
      end
      object chkPushpins: TCheckBox
        Left = 8
        Top = 52
        Width = 241
        Height = 17
        Caption = 'Pushpins'
        TabOrder = 7
        OnClick = chkPushpinsClick
      end
      object chkDrawInfoPoints: TCheckBox
        Left = 26
        Top = 32
        Width = 345
        Height = 17
        Caption = 'Include extra "Info" points'
        TabOrder = 8
        OnClick = chkDrawInfoPointsClick
      end
    end
    object tabFilter: TTabSheet
      Caption = 'Object filter'
      object Label9: TLabel
        Left = 8
        Top = 8
        Width = 421
        Height = 33
        AutoSize = False
        Caption = 
          'Set the object filter to limit what is displayed in both the mob' +
          ' list and the map view.  Checking an item indicates that you wou' +
          'ld like it to be displayed.'
        WordWrap = True
      end
      object grpFilterByType: TGroupBox
        Left = 8
        Top = 44
        Width = 189
        Height = 213
        Caption = '  By object type  '
        TabOrder = 0
        object chkRenderFriendlies: TCheckBox
          Left = 24
          Top = 44
          Width = 121
          Height = 17
          Caption = 'Friendly Players (F)'
          TabOrder = 1
          OnClick = chkRenderFriendliesClick
        end
        object chkRenderVehicles: TCheckBox
          Tag = 5
          Left = 8
          Top = 140
          Width = 141
          Height = 17
          Caption = 'Vehicles'
          TabOrder = 5
          OnClick = ObjectFilterClick
        end
        object chkRenderUnknown: TCheckBox
          Left = 8
          Top = 116
          Width = 141
          Height = 17
          Caption = 'Unknown (U)'
          TabOrder = 4
          OnClick = ObjectFilterClick
        end
        object chkRenderPlayers: TCheckBox
          Tag = 3
          Left = 8
          Top = 20
          Width = 141
          Height = 17
          Caption = 'Players (P)'
          TabOrder = 0
          OnClick = chkRenderPlayersClick
        end
        object chkRenderObjects: TCheckBox
          Tag = 1
          Left = 8
          Top = 92
          Width = 141
          Height = 17
          Caption = 'Graves and objects (O)'
          TabOrder = 3
          OnClick = ObjectFilterClick
        end
        object chkRenderMobs: TCheckBox
          Tag = 2
          Left = 8
          Top = 68
          Width = 141
          Height = 17
          Caption = 'Monsters and NPCs (M)'
          TabOrder = 2
          OnClick = ObjectFilterClick
        end
        object chkRenderUnkStealthers: TCheckBox
          Left = 8
          Top = 164
          Width = 141
          Height = 17
          Caption = 'Anonymous stealthers'
          TabOrder = 6
          OnClick = chkRenderUnkStealthersClick
        end
        object chkRenderDoors: TCheckBox
          Tag = 6
          Left = 8
          Top = 188
          Width = 97
          Height = 17
          Caption = 'Doors'
          TabOrder = 7
          OnClick = ObjectFilterClick
        end
      end
      object grpFilterByCon: TGroupBox
        Left = 212
        Top = 44
        Width = 189
        Height = 193
        Caption = '  By level con  '
        TabOrder = 1
        object chkShowGrays: TCheckBox
          Left = 12
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Grays'
          TabOrder = 0
          OnClick = ObjectConClick
        end
        object chkShowGreens: TCheckBox
          Tag = 1
          Left = 12
          Top = 44
          Width = 97
          Height = 17
          Caption = 'Greens'
          TabOrder = 1
          OnClick = ObjectConClick
        end
        object chkShowBlues: TCheckBox
          Tag = 2
          Left = 12
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Blues'
          TabOrder = 2
          OnClick = ObjectConClick
        end
        object chkShowYellows: TCheckBox
          Tag = 3
          Left = 12
          Top = 92
          Width = 97
          Height = 17
          Caption = 'Yellows'
          TabOrder = 3
          OnClick = ObjectConClick
        end
        object chkShowOranges: TCheckBox
          Tag = 4
          Left = 12
          Top = 116
          Width = 97
          Height = 17
          Caption = 'Oranges'
          TabOrder = 4
          OnClick = ObjectConClick
        end
        object chkShowReds: TCheckBox
          Tag = 5
          Left = 12
          Top = 140
          Width = 97
          Height = 17
          Caption = 'Reds'
          TabOrder = 5
          OnClick = ObjectConClick
        end
        object chkShowPurples: TCheckBox
          Tag = 6
          Left = 12
          Top = 164
          Width = 97
          Height = 17
          Caption = 'Purples'
          TabOrder = 6
          OnClick = ObjectConClick
        end
      end
    end
    object tabRangeCircles: TTabSheet
      Caption = 'Range circles'
      ImageIndex = 3
      object Label3: TLabel
        Left = 8
        Top = 28
        Width = 271
        Height = 13
        Caption = 'The cocentric range circles drawn around your character.'
      end
      object chkRangeCircles: TCheckBox
        Left = 8
        Top = 8
        Width = 241
        Height = 17
        Caption = 'Range circles (C)'
        TabOrder = 0
        OnClick = chkRangeCirclesClick
      end
      object lstRangeCircles: TListBox
        Left = 8
        Top = 52
        Width = 121
        Height = 217
        ItemHeight = 13
        TabOrder = 1
        OnClick = lstRangeCirclesClick
      end
      object GroupBox1: TGroupBox
        Left = 140
        Top = 52
        Width = 257
        Height = 181
        Caption = '  Selected Circle Details  '
        TabOrder = 2
        object Label17: TLabel
          Left = 8
          Top = 24
          Width = 73
          Height = 13
          Caption = 'Distance radius'
        end
        object Label18: TLabel
          Left = 108
          Top = 24
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object Label19: TLabel
          Left = 8
          Top = 72
          Width = 58
          Height = 13
          Caption = 'Smoothness'
        end
        object edtRangeDistance: TSpinEdit
          Left = 8
          Top = 40
          Width = 77
          Height = 22
          Increment = 50
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 500
          OnChange = edtRangeDistanceChange
        end
        object colorRange: TColorGrid
          Left = 108
          Top = 40
          Width = 140
          Height = 132
          BackgroundEnabled = False
          TabOrder = 1
          OnChange = colorRangeChange
        end
        object edtRangeSmoothness: TSpinEdit
          Left = 8
          Top = 88
          Width = 77
          Height = 22
          Increment = 2
          MaxValue = 360
          MinValue = 4
          TabOrder = 2
          Value = 24
          OnChange = edtRangeSmoothnessChange
        end
      end
      object btnAddCircle: TBitBtn
        Left = 140
        Top = 240
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 3
        OnClick = btnAddCircleClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333FF33333333FF333993333333300033377F3333333777333993333333
          300033F77FFF3333377739999993333333333777777F3333333F399999933333
          33003777777333333377333993333333330033377F3333333377333993333333
          3333333773333333333F333333333333330033333333F33333773333333C3333
          330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
          333333333337733333FF3333333C333330003333333733333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        Spacing = 8
      end
      object btnDelCircle: TBitBtn
        Left = 216
        Top = 240
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btnDelCircleClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333FF33333333333330003333333333333777333333333333
          300033FFFFFF3333377739999993333333333777777F3333333F399999933333
          3300377777733333337733333333333333003333333333333377333333333333
          3333333333333333333F333333333333330033333F33333333773333C3333333
          330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
          333333377F33333333FF3333C333333330003333733333333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        Spacing = 8
      end
    end
    object tabMobList: TTabSheet
      Caption = 'Mob list'
      ImageIndex = 4
      object Label21: TLabel
        Left = 12
        Top = 144
        Width = 328
        Height = 13
        Caption = 
          '-- The mob list can be undocked from the map window by dragging ' +
          'it. '
      end
      object Label22: TLabel
        Left = 12
        Top = 164
        Width = 393
        Height = 29
        AutoSize = False
        Caption = 
          '-- Press F2 to toggle the mob list on and off, or to show it if ' +
          'you'#39've undocked and closed it.'
        WordWrap = True
      end
      object chkGroupByRealm: TCheckBox
        Left = 8
        Top = 8
        Width = 241
        Height = 17
        Caption = 'Group list by realm'
        TabOrder = 0
        OnClick = chkGroupByRealmClick
      end
      object chkGroupByClass: TCheckBox
        Left = 8
        Top = 32
        Width = 241
        Height = 17
        Caption = 'Group list by object class'
        TabOrder = 1
        OnClick = chkGroupByClassClick
      end
      object grpListSort: TRadioGroup
        Left = 8
        Top = 60
        Width = 197
        Height = 73
        Caption = '  Sorting method  '
        Items.Strings = (
          'Sort by name'
          'Sort by distance')
        TabOrder = 2
        OnClick = grpListSortClick
      end
    end
    object tabGraphics: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 5
      object Label24: TLabel
        Left = 8
        Top = 76
        Width = 135
        Height = 13
        Caption = 'Mob and Player triangle size:'
      end
      object Label25: TLabel
        Left = 44
        Top = 92
        Width = 61
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Min'
      end
      object Label26: TLabel
        Left = 112
        Top = 92
        Width = 53
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Nominal'
      end
      object Label27: TLabel
        Left = 180
        Top = 92
        Width = 57
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Max'
      end
      object Label28: TLabel
        Left = 8
        Top = 206
        Width = 78
        Height = 13
        Caption = 'Redraw map on:'
      end
      object chkSmoothLines: TCheckBox
        Left = 8
        Top = 8
        Width = 241
        Height = 17
        Caption = 'Smooth lines'
        TabOrder = 0
        OnClick = chkSmoothLinesClick
      end
      object chkSmoothPolys: TCheckBox
        Left = 8
        Top = 32
        Width = 241
        Height = 17
        Caption = 'Smooth polygons'
        TabOrder = 1
        OnClick = chkSmoothPolysClick
      end
      object chkSmoothPoints: TCheckBox
        Left = 8
        Top = 56
        Width = 237
        Height = 17
        Caption = 'Smooth points'
        TabOrder = 2
        OnClick = chkSmoothPointsClick
      end
      object chkScaleMobTriangle: TCheckBox
        Left = 44
        Top = 132
        Width = 181
        Height = 17
        Caption = 'Adjust size with range scale'
        TabOrder = 3
        OnClick = chkScaleMobTriangleClick
      end
      object edtMobTriangleMin: TSpinEdit
        Left = 44
        Top = 108
        Width = 61
        Height = 22
        Increment = 5
        MaxValue = 10000
        MinValue = 1
        TabOrder = 4
        Value = 25
        OnChange = edtMobTriangleMinChange
      end
      object edtMobTriangleNom: TSpinEdit
        Left = 112
        Top = 108
        Width = 61
        Height = 22
        Increment = 5
        MaxValue = 10000
        MinValue = 1
        TabOrder = 5
        Value = 150
        OnChange = edtMobTriangleNomChange
      end
      object edtMobTriangleMax: TSpinEdit
        Left = 180
        Top = 108
        Width = 61
        Height = 22
        Increment = 5
        MaxValue = 10000
        MinValue = 1
        TabOrder = 6
        Value = 300
        OnChange = edtMobTriangleMaxChange
      end
      object chkEasyMouseOvers: TCheckBox
        Left = 8
        Top = 156
        Width = 237
        Height = 17
        Caption = 'Easy map mouse-overs'
        TabOrder = 7
        OnClick = chkEasyMouseOversClick
      end
      object chkAttemptMapDownloads: TCheckBox
        Left = 8
        Top = 180
        Width = 313
        Height = 17
        Caption = 'Attempt to download texture and vector maps as needed'
        TabOrder = 8
        OnClick = chkAttemptMapDownloadsClick
      end
      object chkRedrawOnAdd: TCheckBox
        Left = 92
        Top = 204
        Width = 49
        Height = 17
        Caption = 'Add'
        TabOrder = 9
        OnClick = chkRedrawOnAddClick
      end
      object chkRedrawOnUpdate: TCheckBox
        Left = 140
        Top = 204
        Width = 61
        Height = 17
        Caption = 'Update'
        TabOrder = 10
        OnClick = chkRedrawOnUpdateClick
      end
      object chkRedrawOnDelete: TCheckBox
        Left = 200
        Top = 204
        Width = 61
        Height = 17
        Caption = 'Delete'
        TabOrder = 11
        OnClick = chkRedrawOnDeleteClick
      end
      object chkRedrawOnTimer: TCheckBox
        Left = 92
        Top = 224
        Width = 89
        Height = 17
        Caption = 'Min FPS timer'
        TabOrder = 12
        OnClick = chkRedrawOnTimerClick
      end
      object trackMinFPS: TTrackBar
        Left = 188
        Top = 224
        Width = 217
        Height = 17
        Max = 20
        Min = 1
        Orientation = trHorizontal
        Frequency = 1
        Position = 2
        SelEnd = 0
        SelStart = 0
        TabOrder = 13
        ThumbLength = 10
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = trackMinFPSChange
      end
    end
  end
end

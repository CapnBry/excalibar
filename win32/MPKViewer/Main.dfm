object frmMain: TfrmMain
  Left = 231
  Top = 149
  Width = 481
  Height = 403
  Caption = 'Mythic MPK file viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object treeDirectory: TVirtualStringTree
    Left = 0
    Top = 22
    Width = 473
    Height = 354
    Align = alClient
    Ctl3D = False
    DefaultNodeHeight = 16
    DrawSelectionMode = smBlendedRectangle
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
    Header.Style = hsPlates
    LineMode = lmBands
    LineStyle = lsSolid
    NodeDataSize = 4
    ParentCtl3D = False
    PopupMenu = pumListActions
    RootNodeCount = 5
    SelectionBlendFactor = 64
    SelectionCurveRadius = 2
    TabOrder = 1
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect]
    OnDblClick = treeDirectoryDblClick
    OnGetText = treeDirectoryGetText
    Columns = <
      item
        Position = 0
        Width = 151
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 130
        WideText = 'Modified'
      end
      item
        Position = 2
        Width = 75
        WideText = 'Size'
      end
      item
        Position = 3
        Width = 40
        WideText = 'Ratio'
      end
      item
        Position = 4
        Width = 75
        WideText = 'Packed'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 5
        Width = 70
        WideText = 'CRC'
      end>
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      473
      22)
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 75
      Height = 13
      Caption = 'MPK / NPK File'
    end
    object btnBrowse: TSpeedButton
      Left = 449
      Top = 1
      Width = 21
      Height = 20
      Hint = 'Browse for file'
      Anchors = [akTop, akRight]
      Caption = '...'
      OnClick = btnBrowseClick
    end
    object edtFileName: TEdit
      Left = 84
      Top = 1
      Width = 362
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      Text = 'c:\mythic\camelot\gamedata.mpk'
      OnKeyPress = edtFileNameKeyPress
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mpk'
    Filter = 
      'MPAK / NPAK files (*.mpk, *.npk)|*.mpk;*.npk|MPAK files (*.mpk)|' +
      '*.mpk|NPAK files (*.npk)|*.npk|All files (*.*)|*.*'
    Left = 420
    Top = 16
  end
  object ActionList: TActionList
    Left = 164
    Top = 220
    object atnViewWithNotepad: TAction
      Caption = 'View with internal viewer'
      OnExecute = atnViewWithNotepadExecute
    end
    object atnExtractToDir: TAction
      Caption = 'Extract...'
      OnExecute = atnExtractToDirExecute
    end
    object atnView: TAction
      Caption = 'View with associated viewer'
      OnExecute = atnViewExecute
    end
  end
  object pumListActions: TPopupMenu
    Left = 164
    Top = 248
    object Viewwithassociatedviewer1: TMenuItem
      Action = atnView
      Default = True
    end
    object Viewwithinternalviewer1: TMenuItem
      Action = atnViewWithNotepad
    end
    object Extract1: TMenuItem
      Action = atnExtractToDir
    end
  end
end

object frmMain: TfrmMain
  Left = 550
  Top = 223
  Width = 511
  Height = 332
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'DAOC Skilla'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    503
    285)
  PixelsPerInch = 96
  TextHeight = 13
  object lblUpdates: TLabel
    Left = 332
    Top = 76
    Width = 154
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'lblUpdates'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    OnClick = lblUpdatesClick
  end
  object lblChatLogFile: TLabel
    Left = 336
    Top = 112
    Width = 66
    Height = 13
    Caption = 'lblChatLogFile'
  end
  object Memo1: TMemo
    Left = 0
    Top = 140
    Width = 494
    Height = 143
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  inline frmDStrmServerList1: TfrmDStrmServerList
    Left = 4
    Top = 4
    Width = 313
    Height = 129
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    inherited pnlBottom: TPanel
      Top = 113
      Width = 313
      Height = 16
      DesignSize = (
        313
        16)
      inherited lblAdd: TLabel
        Left = 203
      end
      inherited lblEdit: TLabel
        Left = 235
      end
      inherited lblRemove: TLabel
        Left = 263
      end
    end
    inherited lstServers: TListBox
      Width = 313
      Height = 76
    end
    inherited Panel1: TPanel
      Width = 313
    end
  end
  object tmrUpdateCheck: TTimer
    OnTimer = tmrUpdateCheckTimer
    Left = 420
    Top = 8
  end
  object httpUpdateChecker: TIdHTTP
    Request.Accept = 'text/html, */*'
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ProxyPort = 0
    Request.UserAgent = 'Mozilla/3.0 (compatible; DaocSkilla)'
    Left = 420
    Top = 36
  end
  object mnuMain: TMainMenu
    Left = 348
    Top = 20
    object Network1: TMenuItem
      Caption = 'Network'
      object atnRemoteTelnetServer1: TMenuItem
        Action = atnRemoteAdminEnable
      end
      object Processpackets1: TMenuItem
        Action = atnProcessPackets
      end
    end
    object Logging1: TMenuItem
      Caption = 'Logging'
      object atnRealTimeChatLog1: TMenuItem
        Action = atnChatLog
      end
      object Setrealtimechatlogfile1: TMenuItem
        Action = atnChangeChatLogFile
      end
      object Capturemobupdatestomobseen1: TMenuItem
        Action = atnCaptureMobseen
      end
      object Capturedelveinfotodelveseen1: TMenuItem
        Action = atnCaptureDelveseen
      end
      object Dumppacketdatatolog1: TMenuItem
        Action = atnDumpPacketDataToLog
      end
      object atnDumpMobList1: TMenuItem
        Action = atnDumpMobList
      end
    end
    object Windows1: TMenuItem
      Caption = 'Windows'
      object Viewradar1: TMenuItem
        Action = atnRadar
      end
      object Autolaunchradarwindow1: TMenuItem
        Action = atnAutoLaunchRadar
      end
      object SetDAoCpath1: TMenuItem
        Action = atnSetDaocPath
      end
    end
    object Quicklaunch1: TMenuItem
      Caption = 'Quicklaunch'
      object rackcharacterlogins1: TMenuItem
        Action = atnTrackLogins
      end
    end
  end
  object atlMain: TActionList
    Left = 380
    Top = 24
    object atnRadar: TAction
      Category = 'Windows'
      Caption = 'View radar'
      OnExecute = atnRadarExecute
    end
    object atnAutoLaunchRadar: TAction
      Category = 'Windows'
      Caption = 'Autolaunch radar window'
    end
    object atnChatLog: TAction
      Category = 'Logging'
      Caption = 'Real time chat log'
    end
    object atnChangeChatLogFile: TAction
      Category = 'Logging'
      Caption = 'Set realtime chat log file...'
    end
    object atnSetDaocPath: TAction
      Category = 'Windows'
      Caption = 'Set DAoC path'
      OnExecute = atnSetDaocPathExecute
    end
    object atnRemoteAdminEnable: TAction
      Category = 'Network'
      Caption = 'Remote telnet server'
    end
    object atnDumpMobList: TAction
      Category = 'Logging'
      Caption = 'Dump current mob list to log'
    end
    object atnDumpPacketDataToLog: TAction
      Category = 'Logging'
      Caption = 'Dump packet data to log'
    end
    object atnProcessPackets: TAction
      Category = 'Network'
      Caption = 'Process packets'
    end
    object atnTrackLogins: TAction
      Category = 'Quicklaunch'
      Caption = 'Track character logins'
    end
    object atnCaptureMobseen: TAction
      Category = 'Logging'
      Caption = 'Capture mob updates to mobseen'
    end
    object atnCaptureDelveseen: TAction
      Category = 'Logging'
      Caption = 'Capture delve info to delveseen'
    end
  end
end

object frmMain: TfrmMain
  Left = 550
  Top = 223
  Width = 384
  Height = 183
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
  PixelsPerInch = 96
  TextHeight = 13
  object lblUpdates: TLabel
    Left = 0
    Top = 0
    Width = 376
    Height = 13
    Align = alTop
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
  inline frmDStrmServerList1: TfrmDStrmServerList
    Left = 0
    Top = 13
    Width = 376
    Height = 117
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    inherited pnlBottom: TPanel
      Top = 113
      Width = 376
      Height = 4
      DesignSize = (
        376
        4)
      inherited lblAdd: TLabel
        Left = 266
      end
      inherited lblEdit: TLabel
        Left = 298
      end
      inherited lblRemove: TLabel
        Left = 326
      end
    end
    inherited lstServers: TListBox
      Width = 376
      Height = 76
    end
    inherited Panel1: TPanel
      Width = 376
    end
  end
  object tmrUpdateCheck: TTimer
    OnTimer = tmrUpdateCheckTimer
    Left = 184
    Top = 65532
  end
  object httpUpdateChecker: TIdHTTP
    Request.Accept = 'text/html, */*'
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ProxyPort = 0
    Request.UserAgent = 'Mozilla/3.0 (compatible; DaocSkilla)'
    Left = 212
    Top = 65532
  end
  object mnuMain: TMainMenu
    Left = 128
    Top = 65532
    object Network1: TMenuItem
      Caption = 'Network'
      object atnRemoteTelnetServer1: TMenuItem
        Action = atnRemoteAdminEnable
        AutoCheck = True
      end
      object Processpackets1: TMenuItem
        Action = atnProcessPackets
        AutoCheck = True
      end
      object Forceversioncheck1: TMenuItem
        Action = atnForceVersionsUpdate
      end
    end
    object Logging1: TMenuItem
      Caption = 'Logging'
      object atnRealTimeChatLog1: TMenuItem
        Action = atnChatLog
        AutoCheck = True
      end
      object Setrealtimechatlogfile1: TMenuItem
        Action = atnChangeChatLogFile
      end
      object Dumppacketdatatolog1: TMenuItem
        Action = atnDumpPacketDataToLog
        AutoCheck = True
      end
      object atnDumpMobList1: TMenuItem
        Action = atnDumpMobList
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Capturemobupdatestomobseen1: TMenuItem
        Action = atnCaptureMobseen
        AutoCheck = True
      end
      object Appendmobseenfile1: TMenuItem
        Action = atnAppendMobseen
        AutoCheck = True
      end
      object Capturedelveinfotodelveseen1: TMenuItem
        Action = atnCaptureDelveseen
        AutoCheck = True
      end
      object Appenddelveseenfile1: TMenuItem
        Action = atnAppendDelveseen
        AutoCheck = True
      end
    end
    object Windows1: TMenuItem
      Caption = 'Windows'
      object Viewradar1: TMenuItem
        Action = atnRadar
      end
      object Autolaunchradarwindow1: TMenuItem
        Action = atnAutoLaunchRadar
        AutoCheck = True
      end
      object SetDAoCpath1: TMenuItem
        Action = atnSetDaocPath
      end
      object ViewDaocSkillainternallog1: TMenuItem
        Action = atnSkillaLog
      end
    end
  end
  object atlMain: TActionList
    Left = 156
    Top = 65532
    object atnRadar: TAction
      Category = 'Windows'
      Caption = 'View radar'
      OnExecute = atnRadarExecute
    end
    object atnAutoLaunchRadar: TAction
      Category = 'Windows'
      AutoCheck = True
      Caption = 'Autolaunch radar window'
      OnExecute = atnAutoLaunchRadarExecute
    end
    object atnChatLog: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Real time chat log'
      OnExecute = atnChatLogExecute
    end
    object atnChangeChatLogFile: TAction
      Category = 'Logging'
      Caption = 'Set realtime chat log file...'
      OnExecute = atnChangeChatLogFileExecute
    end
    object atnSetDaocPath: TAction
      Category = 'Windows'
      Caption = 'Set DAoC path'
      OnExecute = atnSetDaocPathExecute
    end
    object atnRemoteAdminEnable: TAction
      Category = 'Network'
      AutoCheck = True
      Caption = 'Remote telnet server'
      OnExecute = atnRemoteAdminEnableExecute
    end
    object atnDumpMobList: TAction
      Category = 'Logging'
      Caption = 'Dump current mob list to internal log'
      Enabled = False
      OnExecute = atnDumpMobListExecute
    end
    object atnDumpPacketDataToLog: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Dump packet data to internal log'
      OnExecute = atnDumpPacketDataToLogExecute
    end
    object atnProcessPackets: TAction
      Category = 'Network'
      AutoCheck = True
      Caption = 'Process packets'
      OnExecute = atnProcessPacketsExecute
    end
    object atnCaptureMobseen: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Capture mob updates to mobseen.csv'
      OnExecute = atnCaptureMobseenExecute
    end
    object atnAppendMobseen: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Append mobseen file'
      Checked = True
      OnExecute = atnAppendMobseenExecute
    end
    object atnCaptureDelveseen: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Capture delve info to delveseen.csv'
      OnExecute = atnCaptureDelveseenExecute
    end
    object atnSkillaLog: TAction
      Category = 'Windows'
      Caption = 'View DaocSkilla internal log'
      OnExecute = atnSkillaLogExecute
    end
    object atnForceVersionsUpdate: TAction
      Category = 'Network'
      Caption = 'Force version check'
      OnExecute = atnForceVersionsUpdateExecute
    end
    object atnAppendDelveseen: TAction
      Category = 'Logging'
      AutoCheck = True
      Caption = 'Append delveseen file'
      Checked = True
      OnExecute = atnAppendDelveseenExecute
    end
  end
end

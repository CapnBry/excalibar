object frmMain: TfrmMain
  Left = 550
  Top = 223
  Width = 510
  Height = 331
  Caption = 'DAOC Skilla'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    502
    304)
  PixelsPerInch = 96
  TextHeight = 13
  object lblServerPing: TLabel
    Left = 344
    Top = 0
    Width = 155
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Disconnected'
  end
  object Label1: TLabel
    Left = 56
    Top = 4
    Width = 124
    Height = 16
    Caption = 'Set up connection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 56
    Top = 20
    Width = 288
    Height = 41
    AutoSize = False
    Caption = 
      'Set up which network adapter to listen on, or which machine to s' +
      'end your data to.  You must select an adapter before starting DA' +
      'oC.'
    WordWrap = True
  end
  object lblUpdates: TLabel
    Left = 344
    Top = 16
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
  object btnLogin: TBitBtn
    Left = 368
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Launch DAoC'
    Enabled = False
    TabOrder = 1
    OnClick = btnLoginClick
  end
  object Memo1: TMemo
    Left = 4
    Top = 164
    Width = 493
    Height = 137
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnDebugging: TButton
    Left = 76
    Top = 88
    Width = 75
    Height = 25
    Caption = '&Debugging'
    TabOrder = 3
    OnClick = btnDebuggingClick
  end
  object chkAutolaunchExcal: TCheckBox
    Left = 4
    Top = 64
    Width = 165
    Height = 17
    Caption = 'Autolaunch Excalibur windows'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkChatLog: TCheckBox
    Left = 4
    Top = 138
    Width = 105
    Height = 17
    Caption = 'Realtime chat.log'
    TabOrder = 5
    OnClick = chkChatLogClick
  end
  object edtChatLogFile: TEdit
    Left = 116
    Top = 136
    Width = 381
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = 'c:\mythic\isles\realchat.log'
  end
  object btnResume: TButton
    Left = 4
    Top = 116
    Width = 145
    Height = 17
    Caption = 'Resume last connection'
    TabOrder = 7
    OnClick = btnResumeClick
  end
  object btnConnectionOpts: TBitBtn
    Left = 4
    Top = 12
    Width = 45
    Height = 41
    TabOrder = 8
    OnClick = btnConnectionOptsClick
    Glyph.Data = {
      1A040000424D1A040000000000005A00000028000000200000001E0000000100
      080000000000C0030000120B0000120B0000090000000900000000000000FFFF
      FF00FF00FF00FFFF00008484000000FFFF0000848400C6C6C600848484000202
      0202020202020202020202020202020202020202000000000000020202020202
      0202020202020202020202020202020200000000060606060600000000000202
      0202020202020000000000020202020207070707060105050600070707070202
      0202020202000202020202020202020201010101060105050600010101010202
      0202020202000202020202020202020208080808060101010600080808080202
      0202020202000202020202020202020202020202060606060600020202020202
      0202020202000202020202020202020202020202020802070002020202020202
      0202020202000202020202020202020202020202020802070002020202020202
      0202020202000202020202020202020202020202020802070002020202020202
      0202020202000202020202020202020202020202020802070002020202020202
      0202020202000202020202020202020202020202020202020202020202020202
      0202020202020202020202020202020202020202020202020202020202020200
      0000000000000000000000020202020202020202020202020202020202020808
      0808080808080808080808000202020202020202020202000202020202020801
      0707070707070000000708000202020202020202020202000202020202020801
      0101010101010101010108000202020202020202020202000202020202020208
      0000000000000000000008000000000000000002020202000202020202020208
      0707070707070707070800080808080808080800020202000202020202020208
      0704040404040404070800070707000000070800020000020202020202020208
      0700030303030303070800010101010101010800020202020202020202020208
      0700030303030303070800000000000000000800020202020202020202020208
      0700010303030303070800070707070707080002020202020202020202020208
      0700010303030303070800040404040407080002020202020202020202020208
      0700000000000000070800030303030307080002020202020202020202020208
      0707070707070707070800030303030307080002020202020202020202020202
      0808080808080808080801030303030307080002020202020202020202020202
      0202020202020208070001030303030307080002020202020202020202020202
      0202020202020208070000000000000007080002020202020202020202020202
      0202020202020208070707070707070707080002020202020202020202020202
      020202020202020208080808080808080808020202020202020202020202}
  end
  object btnGLRender: TBitBtn
    Left = 4
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Excalibur'
    TabOrder = 9
    OnClick = btnGLRenderClick
  end
  object btnMacroing: TBitBtn
    Left = 152
    Top = 88
    Width = 69
    Height = 25
    Caption = 'Macroing'
    TabOrder = 10
    OnClick = btnMacroingClick
  end
  object cbxAutoLogin: TComboBox
    Left = 304
    Top = 84
    Width = 197
    Height = 21
    DropDownCount = 12
    ItemHeight = 13
    TabOrder = 0
    OnKeyPress = cbxAutoLoginKeyPress
  end
  object chkTrackLogins: TCheckBox
    Left = 304
    Top = 68
    Width = 193
    Height = 17
    Caption = 'Track character logins'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = chkTrackLoginsClick
  end
  object btnDeleteChar: TBitBtn
    Left = 444
    Top = 108
    Width = 55
    Height = 25
    Caption = 'Remove'
    Enabled = False
    TabOrder = 12
    OnClick = btnDeleteCharClick
  end
  object tcpCollectorClient: TIdTCPClient
    OnStatus = tcpCollectorClientStatus
    MaxLineAction = maException
    OnDisconnected = tcpCollectorClientDisconnected
    Host = '127.0.0.1'
    Port = 9867
    Left = 256
    Top = 104
  end
  object tcpCollectorServer: TIdTCPServer
    OnStatus = tcpCollectorServerStatus
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 9867
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnConnect = tcpCollectorServerConnect
    OnExecute = tcpCollectorServerExecute
    OnDisconnect = tcpCollectorServerDisconnect
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 224
    Top = 104
  end
  object tmrReconnect: TTimer
    Enabled = False
    OnTimer = tmrReconnectTimer
    Left = 224
    Top = 72
  end
  object tmrUpdateCheck: TTimer
    OnTimer = tmrUpdateCheckTimer
    Left = 256
    Top = 72
  end
  object httpUpdateChecker: TIdHTTP
    MaxLineAction = maException
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; DaocSkilla)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 284
    Top = 104
  end
end

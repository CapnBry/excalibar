object dmdRemoteAdmin: TdmdRemoteAdmin
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 658
  Top = 316
  Height = 118
  Width = 215
  object tcpRemoteAdmin: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 9023
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnConnect = tcpRemoteAdminConnect
    OnExecute = tcpRemoteAdminExecute
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 40
    Top = 12
  end
end

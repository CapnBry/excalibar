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
    DefaultPort = 9023
    OnConnect = tcpRemoteAdminConnect
    OnExecute = tcpRemoteAdminExecute
    Left = 40
    Top = 12
  end
end

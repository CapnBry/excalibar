object dmdRemoteAdmin: TdmdRemoteAdmin
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 1150
  Top = 498
  Height = 150
  Width = 215
  object tcpRemoteAdmin: TIdTCPServer
    Active = True
    Bindings = <>
    DefaultPort = 9023
    OnConnect = tcpRemoteAdminConnect
    OnExecute = tcpRemoteAdminExecute
    Left = 44
    Top = 12
  end
end

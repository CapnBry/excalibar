Sub DAOCControl_OnChatSendIncoming(ByVal Who, ByVal Message)
  If Who = "Complainer" Then
  	' SendKeys "/send " + Who + " STFU about your " + Message
    ChatSend Who, "STFU about your " + Message
  End If
End Sub

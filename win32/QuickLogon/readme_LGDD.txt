-- Put LaunchGameDLL.exe in your DAoC directory.
-- Create a shortcut to LaunchGameDLL with parameters:

LaunchGameDLL <whichdll> <server> <port> <account> <password> <character> <realm>
  whichdll - name of dll to launch (game.dll or tgame.dll)
  server - IP address of server (check user.dat for some list of servers)
  port - port to connect to the server on (usually 10622)
  account - Account name
  password - Account char
  character - Character name (may be case-sensitive)
  realm - 1=Albion 2=Midgard 3=Hibernia

++ What does it do?
   LaunchGameDLL jumps you directly into a windowed-mode Dark Age of Camelot.
   It skips the following steps:
     "Enter CD Key"  (why?)
     "Checking for new version..."
     Mythic splash screen
     Select account, password, character
     "You are a new user or the EUALA has been changed..."
     "EUAUA"
     "Are you sure you accept the EUALA?"
     "You are a new user or the ROC has been changed..."
     "ROC"
     "Are you sure you accept the ROC?"
     Select server

++ Disadvantages?
   You still have to use the regular patcher to get new versions.  You
   cannot run an old game.dll against a newer version server.
   
++ Why would you want to do that?
   Because it's friggin annoying to have to go through the new EUALA
   speiel every time you log in another account.

++ I tried to log on and DAOC is telling me I have the wrong version.
   You need to kick off the real DAOC so the patcher runs then cancel
   at the login screen.

++ Can I run 2 copies of DAOC using this program?
   The multi-instance patch only appears to work for SI.  So if you
   are running SI yes, if Classic no.  If you have 1 SI and 1 Classic
   start the Classic first then the SI.
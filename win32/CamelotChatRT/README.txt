                              -===============-
                               Camelot Chat RT
                              -===============-

============
INSTALLATION
============
This program required WinPCAP 3.0 be installed on your machine.
http://winpcap.polito.it/

-- Unzip the executable to any directory on your machine.
-- Launch CamelChatRT.exe ***on your DAoC machine***.
-- Select the Ethernet Adapter from the list which you expect the
   Dark Age traffic to go on.
-- The Ethernet Adapter status should change to Open.
-- Launch DAoC.
-- The connection status should go from Disconnected to Connected to
   Active.
-- Your chat log is now rolling.  It will continue to run until you log
   out.

===
FAQ
===
Q.  What is Camelot Chat RT?
A.  Camelot Chat RT is a real-time packet sniffer for Dark Age of Camelot
    which writes a chat.log file in real time, circumventing the buffered
    writes which Mythic added in patch v1.55.

Q.  Why would they do that?
A.  To prevent real-time macro programs which rely on the chat.log for
    their event notification.  Unfortunately it also broke many semi-
    legitimate chat log tail programs which are not macros.

Q.  I don't see any adapters in the list!
A.  One of two things is happening here.  Either I've got a bug or you
    have an SMP machine.  Sorry but SMP machines are not supported at
    this time.  If you do not have a multiprocessor machine, follow the
    steps in NO_ADAPTERS.txt to troubleshoot.

Q.  When I start DAoC, it runs *very* slowly now and Connection Status
    never goes to Connected.  CamelotChatRT is using 100% CPU time.
A.  This is a known issue right now, with no cause.  Early speculation
    is that it has to do something with the Win9x platform.  Not sure
    though and something that is being looked in to.

Q.  Does Camelot Chat RT work on a dial-up connection?
A.  Most likely not.  I have no dial-up connection to test on, and this
    is from the WinPCAP FAQ:
      We have tested WinPcap on PPP connections under Windows 95, Windows
      98 and Windows ME. In Windows 95, due to a bug in NDIS, WinPcap
      sometimes resets the PPP connection. In Windows 98/ME this bug
      appears to be corrected, and WinPcap seems to receive correctly,
      however it is not able to send packets. Under Windows NT/2000/XP
      there are problems with the binding process, that prevent a
      protocol driver from working properly on the WAN adapter. The
      problem is caused by the PPP driver of WinNTx, ndiswan, that
      doesn't provide a standard interface to capture.
    Either way, I'd still have to fix the code to strip the PPP
    protocol header and I'd definately need a test system to try that
    on.

Q.  I ues a wireless network card, is that supported?
A.  I do all my development on a wireless network card (802.11b), but also
    test with a wired, so yes wireless network cards are supported.

Q.  What platform does Camelot Chat RT run on?
A.  The same platform DAoC does.  Windows / Win32.

Q.  Is there any way to pause the chat log like turning it on and off
    with the L key?
A.  No, the chat log starts and runs until you disconnect.

Q.  Can I run CamelotChatRT from another machine on the network?
A.  Yes, but you must make sure that machine can see the network
    packets from the DAoC box.  This means you cannot be on a switch
    unless you route the packets through the CamelotChatRT box.  If you
    are confused by all this, just run CamelotChatRT on the DAoC box.

Q.  Is this program against the TOS?
A.  Definately.

Q.  Is this program detectable?
A.  Not directly, no.  It is a passive ethernet packet sniffer.  The
    only way it can be detected is if Mythic starts scanning your
    machine's process list.

Q.  I put an invalid file name in the file name box and when I started
    DAoC CamelotChatRT threw an exception and died.
A.  Yes.  If you don't put a valid file name in the filename box the
    program will throw an exception and die.  Don't do that.

Q.  What is an invalid file name?
A.  If the chat log filename you specify does not exist, CamelotChatRT
    will create it with a CreateFile shared exclusive mode, and then
    close it.  Once the file exists, the file will be opened Write-Only
    and shared read/write access (deny none).

Q.  What language is CamelotChatRT written in?
A.	I use Delphi 6 from Borland (http://borland.com/delphi/index.html)


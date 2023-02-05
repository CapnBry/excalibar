Git repository holding a conversion of the [SourceForge Excalibur](https://sourceforge.net/projects/excalibar/) project. I (CapnBry) originally created this project under the name `teerex`. The CVS there doesn't seem to work any more, even from a command line, so I created this GitHub repository from an old CVS backup. The originaly repository was named `excalibar` and the misspelling was some sort of dumb joke, but I can't remember why that was funny to me in 2002.

### Excalibur

Excalibur is the commonly used Linux GUI radar application for viewing real time nearby monter information from sniffing Dark Age of Camelot network data. It is based on Odin's Eye by "Slicer/Hackersquest" and development was continued for several years and new features and updated maps were added.

### daoccrypt / exCrypt

Originally the DaoC "encryption" decrypters were provided in binary-only form, to prevent leaking the secret mastermind code of packet encryption. The same home-grown encryption was also used by Mythic's billing frontend and sent your credit card information in the dumbest way possible (see the [public security advisory](http://capnbry.net/daoc/advisory20031211/) I co-wrote on it).

### win32/DaocSkilla

Many cheaters complained that Linux was too hard, and honestly command-line C++ GUI development on my underpowered Linux box (which also ran MythTV) was a little cumbersome, so everything was rewritten from scratch in Delphi for Win32. DaocSkilla was so named because it also contained many tradeskill functions, as well as a fully automating doing tradeskill tasks / buying materials / map navigation / craft queues. Monster location data was also collected and used to [create a beastiary](http://capnbry.net/daoc/). Its signature feature however was still the radar functionality and being available for Windows really opened the floodgates for rampant cheating, or as I called it "Knowing where the heck I am".

Binary DaocSkilla downloads are available:
[http://capnbry.net/daoc/daocskilla.php](http://capnbry.net/daoc/daocskilla.php)

### DStreamServer

DaocSkilla originally would capture broadcast network data with PCAP, but as network switches became more common a way was needed to capture the network data from another machine. Computers with multiple monitors really weren't a thing back then, so a normal use case would have been to run the DAOC game on the gaming PC and copy the network traffic over to a laptop to display the radar. DStreamServer eventually just went to injecting itself into the game client to get the packets without requiring the user to install WinPCAP, which could be a dicey operation as Windows was still not super-cool with it. DStreamServer feeds game packets (the "DStream") to connected clients, and multiple DaocSkilla instances can connect, allowing sharing of radar data between friends to see a larger area.

### win32/DaocInject

Source code for the DLL that was injected into the game process, which fed the packets back to DStreamServer.

### win32/QuickLogon

An application created to allow the user to jump right into the game, skipping the 90 "press OK to continue" popups that Mythic thought seemed reasonable for someone to go through every time the game was launched. Check [the readme](blob/main/win32/QuickLogon/readme_LGDD.txt) for more details.

### win32/CamelotChatRT

Creates an unbufferd realtime chat.log useful for macroing things. The in-game chat.log only flushes to disk occasionaly and is therefore unsuitable for use by cheaters, this fixes that.

### win32/MPKViewer

A tree view utility for viewing game MPK data file data. Not terribly useful except for viewing some assets.

### win32/FakeLoginDLL

Similar to QuickLogon, allows launching game.dll from my own controlled process. The Mythic login.dll did some sort of weirdness to attempt to block cheaters like me, although I can't remember the methods they employed.

### win32/Cheyenne

Cheyenne was a C++ suite of Win32 stuff as an alternative to DaocSilla. I didn't write any of this and I can't remember why it was created.

### win32/PCAPDStream

I also can't remember why this exists. I believe it is a command line app for being a DStreamServer, so a lot of the code from DaocInject is duplicated here, and then C++ implementations were added to provide a compatible "DStream".
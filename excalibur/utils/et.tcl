#!/bin/sh
# \
exec wish $0 \
#
# \
exit
# anony.mous
# Event Timer 1.1
# Mon Aug 12 13:25:03 CDT 2002

wm title . "ET v1.0"
# the name of the save data file
set fname {et.data}

frame .display

set fnt "10x20"

# global variable declaration/initialization

set bw 4	;# button width
set bh 2	;# button height

# the widgets

button .display.quit -text Quit -command { exit }
button .display.save -text Save -command "save $fname"
button .display.restore -text Restore -command "source $fname"
menubutton .display.nt -text {Num Timers} -menu .display.ntmenu

set numT 10
set m [menu .display.ntmenu -tearoff 0]
for {set i 0} {$i < $numT} {incr i} {
	set j [expr $i + 1]
	$m add radiobutton -label $j -variable numT -value $j
	set loop($i) 1
	set run($i) 0
	set name($i) {}
	set timer($i) {0:00.0}
	set val($i) {0:00.0}

	entry .display.text$i -textvariable name($i)
	label .display.timer$i -text $timer($i) -font $fnt -borderwidth 3 -anchor e -pady 4 -padx 4 -width 7
	entry .display.val$i -textvariable val($i) -width 7
	button .display.sycn$i -text Sync -command "syncT $i"
	checkbutton .display.loop$i -text L -variable loop($i)
	checkbutton .display.run$i -text R -variable run($i)
}

if [file exists $fname] {
  source $fname
} else {
  set name(0) {Sva Gothis}
  set val(0) {10:20}
  set loop(0) 1
}

# now packem
# grid .display.quit .display.save .display.restore .display.nt
grid .display.quit .display.save .display.restore
for {set i 0} {$i < $numT} {incr i} {
	grid .display.text$i .display.timer$i .display.val$i .display.sycn$i .display.loop$i .display.run$i
}

# readback normal background
set bg [lindex [.display.text0 config -background] 4]
pack .display -fill both -expand yes

# error

# bindings
#bind . <Return> {.pad.equ invoke}
#bind . <KP_Enter> {.pad.equ invoke}

# initialize window

update idletask
wm minsize . [winfo reqwidth .] [winfo reqheight .]

proc syncT {t} {
global run val timer
	set x [split $val($t) :]
	set l [llength $x]
	switch $l { 
		1       { set min 0; set sec [lindex $x 0] }
		default { set min [lindex $x [expr $l - 2]]; set sec [lindex $x [expr $l - 1]]}
	}
	if {"$min" == "" || "$min" < 0} {set min 0}
	set val($t) [format "%2d:%3.1f" $min $sec]
	set timer($t) $val($t)
	.display.timer$t configure -text $timer($t)
	set run($t) 1
}

proc dec_timer {t} {
global timer loop val run bg
	set x [split $timer($t) :]
	set l [llength $x]
	switch $l { 
		1 { set min 0; set sec [lindex $x 0] }
		default { set min [lindex $x [expr $l - 2]]; set sec [lindex $x [expr $l - 1]]}
	}
	if {"$min" == "" || "$min" < 0} {set min 0}
	set sec [expr $sec -.1]
	if {$sec < 0} {
		if {$min > 0} {
			incr min -1
			set sec 59
		} else {
			set run($t) 0
			set sec 0
		}
	}
	if {$sec == 0 && $min == 0} {
		.display.text$t configure -background red
		bell
		after 2000 .display.text$t configure -background $bg
		if {$loop($t) == 1} {
			set x [split $val($t) :]
			switch $l { 
				1 { set sec [lindex $x 0] }
				default { set min [lindex $x [expr $l - 2]]; set sec [lindex $x [expr $l - 1]]}
			}
			if { "$min" == ""} {set min 0}
		} else {
			set run($t) 0
		}
	}
	set timer($t) [format "%2d:%3.1f" $min $sec]
	.display.timer$t configure -text $timer($t)
}

proc save {fname} {
global numT name val loop

	set f [open $fname w]
	puts $f "set numT $numT"
	for {set i 0} {$i < $numT} {incr i} {
		puts $f "set name($i) {$name($i)}"
		puts $f "set val($i) {$val($i)}"
		puts $f "set loop($i) $loop($i)"
	}
	close $f
}

proc mainloop {} {
global numT run

	after 100 mainloop
	for {set i 0} {$i < $numT} {incr i} {
		if {$run($i) == 1} {
			dec_timer $i
		}
	}
}
after 1000 mainloop

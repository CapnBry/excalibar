#!/bin/sh
# \
exec wish $0 \
#
# \
exit
# the Tcl/Tk canvas example modified to display .map files (a quick and dirty hack)
# Dec 22 2003 more public domain software by the MastaMappa
#
# Example 31-1
# A large scrolling canvas.
#

proc Scrolled_Canvas { c args } {
	frame $c
	eval {canvas $c.canvas -xscrollcommand [list $c.xscroll set] \
		-yscrollcommand [list $c.yscroll set] -highlightthickness 0 -borderwidth 0} $args
	scrollbar $c.xscroll -orient horizontal -command [list $c.canvas xview]
	scrollbar $c.yscroll -orient vertical  -command [list $c.canvas yview]
	grid $c.canvas $c.yscroll -sticky news
	grid $c.xscroll -sticky ew
	grid rowconfigure $c 0 -weight 1
	grid columnconfigure $c 0 -weight 1
	return $c.canvas
}

#tk scaling 121 ;# <- this doesnt work as documented
set scale 32
set size 800
set size 600
set gridon 0
set w [expr 4 * 65535 / $scale]

Scrolled_Canvas .c -width $size -height $size -scrollregion "0 0 $w $w" -background black
#=> .c.canvas
pack .c -fill both -expand true
frame .f
button .f.q -text Quit -command exit
label .f.null
button .f.clr -text Clear -command {.c.canvas delete "all"}
label .f.l -text {file name}
set filename {}
entry .f.fn -textvariable filename
button .f.load -text load -command {dofile $filename}
button .f.save -text save -command {wrfile $filename}
button .f.b -text bigger -command {.c.canvas scale "all" 0 0 1.5 1.5}
button .f.s -text smaller -command {.c.canvas scale "all" 0 0 .75 .75}
button .f.g -text grid_on -command { tog_grid }
grid .f.q .f.null .f.clr .f.l .f.fn .f.load .f.save .f.b .f.s .f.g
pack .f

proc tog_grid {} {
global gridon

	set gridon [expr 1 - $gridon]
	if {$gridon == 1} {
		.f.g configure -text grid_off
	} else {
		.f.g configure -text grid_on
	}
}

#nawk ' { for (i = 4; i < 8; i++) $i = f($i); print $0 }; func f(n,m) { m = n / 32; return int(m) + (m - int(m) >= .5 ? 1 : 0) }'

proc dofile {n} {
global scale gridon
	set fd [open "|emap $n" r]
	while {[gets $fd line] >= 0} {
		set a [split $line ,]
		if {[lindex $a 0] == "M"} {
			set x1 [expr [lindex $a 4] / $scale]
			set y1 [expr [lindex $a 5] / $scale]
			set x2 [expr [lindex $a 7] / $scale]
			set y2 [expr [lindex $a 8] / $scale]
			set clr [lindex $a 2]
			.c.canvas create line $x1 $y1 $x2 $y2 -fill $clr
		}
		if {[lindex $a 0] == "P"} {
			set x [expr [lindex $a 3] / $scale]
			set y [expr [lindex $a 4] / $scale]
			set label [lindex $a 1]
			set clr [lindex $a 2]
			set x1 [expr $x - 2]
			set y1 [expr $y - 2]
			set x2 [expr $x + 2]
			set y2 [expr $y + 2]
			.c.canvas create oval $x1 $y1 $x2 $y2 -fill $clr -outline white
			.c.canvas create text $x2 $y2 -fill $clr -anchor sw -text $label
		}
	}
	close $fd
	if { $gridon == 1 } {
		set l_begin [expr 0 / $scale]
		set l_end [expr 65535 / $scale]
		set l_step [expr 10000 / $scale]
		for {set i $l_begin } {$i < $l_end} {incr i $l_step} {
				.c.canvas create line $l_begin $i $l_end $i -fill gray
				.c.canvas create line $i $l_begin $i $l_end -fill gray
		}
	}
}

#dofile /tmp/zz2

#! /bin/bash

if [ ! -x odinseye ]; then
  echo "Where's the binary?"
  exit
fi


if [ "$1" == "" ]; then
  echo Argument must be a core file
  exit
fi

if [ ! -r $1 ]; then
  echo "Where's the core file?"
  exit
fi

if ! gdb -x commands.gdb -n -batch ./odinseye $1 > .gdb.result 2> .gdb.errors; then
  echo Failed to run debugger
  exit
fi

if grep newer .gdb.errors; then
  echo "Executable doesn't match core file"
  exit
fi

echo > .gdb.report "Please describe what you were doing when the crash occured"
$EDITOR .gdb.report

clear

BUGREPORT=$( cat .gdb.report ; echo -e "\n\n\n"; cat .gdb.result)

echo "=================="
echo "$BUGREPORT"
echo "=================="
echo -e "\n\n"
echo -n "Do you wish to send this report? "

read affirm

affirm=$(echo $affirm | tr [:upper:] [:lower:])

if echo $affirm | grep y > /dev/null; then
  echo Sending...
  echo "$BUGREPORT" | mail -s "OE Bug Report" slicer@ethernalquest.org
  echo Sent
else
  echo Not sending.
fi

#!/usr/bin/env bash

# -conf configfile

cur_dir="${0%/*}"
name="${1%.asm}"
com_name="$name.com"
com_name="${com_name^^}"
rm $com_name
dosbox -c "mount C $cur_dir" -c "C:" -c "cd nasm-2~1.01" -c "nasm.exe -i ..\\ ..\\$name.asm -o ..\\$name.com" -c "cd .." -c "$name.com"
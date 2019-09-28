#!/bin/bash
IFS='
'
while read line
do
	src=$(echo $line | cut -f 1 -d '	')
	dst=$(echo $line | cut -f 2 -d '	')
	dst=${dst/#\~/$HOME}
	ln -s -v -- $(realpath $src) $dst
done < files.lst

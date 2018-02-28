#!/bin/bash
IFS='
'
while read line
do
	src=$(echo $line | cut -f 2 -d '	')
	dst=$(echo $line | cut -f 1 -d '	')
	src=${src/#\~/$HOME}
	cp -v -- $src $dst
done < files.lst

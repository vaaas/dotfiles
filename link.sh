#!/bin/bash
IFS='
'
alias lns='ln -s -v --'

# while read line
# do
# 	src=$(echo $line | cut -f 1 -d '	')
# 	dst=$(echo $line | cut -f 2 -d '	')
# 	dst=${dst/#\~/$HOME}
# 	ln -s -v -- $(realpath $src) $dst
# done < files.lst

for x in home/*
do lns $(realpath $x) ~/.$(basename $x)
done

mkdir -p ~/Binaries
for x in bin/*
do lns $(realpath $x) ~/Binaries/$(basename $x)
done

mkdir -p ~/.config/emacs
for x in emacs/*
do lns $(realpath $x) ~/.config/emacs/$(basename $x)
done

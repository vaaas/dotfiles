#!/bin/bash
IFS='
'

lns() {
	ln -s -v -- "$@"
}

# ~ files
for x in home/*
do lns $(realpath $x) ~/.$(basename $x)
done

# binaries
mkdir -p ~/Binaries
for x in bin/*
do lns $(realpath $x) ~/Binaries/$(basename $x)
done

# ~/.config
for x in $(find config -type d | tail -n +2)
do mkdir -p ~/.$x
done

for x in $(find config -type f)
do lns $(realpath $x) ~/.$x
done

# ~/.local/share
for x in $(find share -type d | tail -n +2)
do mkdir -p ~/.local/$x
done

for x in $(find share -type f)
do lns $(realpath $x) ~/.local/$x
done

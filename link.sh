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

# firefox
ffprofile=$(realpath ~/.mozilla/firefox/*.default*)
mkdir -p $ffprofile/chrome
lns $(realpath firefox/userChrome.css) $ffprofile/chrome

# systemd
for x in $(find systemd/services -type f)
do sudo cp -v $(realpath $x) /etc/systemd/system/$(basename $x)
done

for x in $(find systemd/nspawn -type f)
do sudo cp -v $(realpath $x) /etc/systemd/nspawn/$(basename $x)
done

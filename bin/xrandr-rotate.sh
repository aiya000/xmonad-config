#!/bin/bash

status_file=/tmp/aiya000-xmonad-has-rotated

if [[ -f $status_file ]] ; then
    xrandr --output eDP1 --rotate normal
    rm "$status_file"
else
    xrandr --output eDP1 --rotate left
    touch "$status_file"
fi

xrandr --output DVI-I-1-1 --above eDP1 || true

if [[ -e ~/.dotfiles/.private/archlinux/xinput-map-surface-screen-to-eDP-1.sh ]] ; then
    ~/.dotfiles/.private/archlinux/xinput-map-surface-screen-to-eDP-1.sh
fi

#!/bin/bash

png="$HOME/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"

case "$1" in
FullScreen)
    window=root
    ;;
ActiveWindow)
    if ! window=$(xdotool getwindowfocus -f) ; then
        notify-send 'ScreenShot' "the focus of the current window couldn't be gotten x("
        exit 1
    fi
    ;;
*)
    notify-send 'ScreenShot' "unknown screenShot kind: '$1'"
    exit 1
    ;;
esac

if log=$(import -window "$window" "$png" 2>&1) ; then
  notify-send 'ScreenShot' 'Shot'
  espeak -s 150 -v +fex 'Shot'
else
  notify-send 'ScreenShot' "$log"
  espeak -s 150 -v +fex 'Error!'
fi

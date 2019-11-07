#!/bin/bash

case "$1" in
FullScreen)
    target=fullscrenn
    ;;
ActiveWindow)
    target=window
    ;;
*)
    notify-send 'ScreenShot' "unknown screenShot kind: '$1'"
    exit 1
    ;;
esac

if ! xfce4-screenshooter --"$target" --save ~/Picture ; then
    notify-send 'ScreenShot' 'failed'
    espeak -s 150 -v +fex '>>>>>>>>>>>>>>>>>> Error! <<<<<<<<<<<<<<<<<<'
fi

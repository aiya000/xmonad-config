#!/bin/bash
# shellcheck disable=SC2088
cd ~/.xmonad || (notify-send '~/.xmonad is not found' ; exit 1)
{
    echo '.xmonad/replace.sh starts'
    stack install
    stack exec -- xmonad-config --recompile
    stack exec -- xmonad-config --restart
    killall xmonad-x86_64-linux
    stack exec -- xmonad-config > /dev/null 2>&1 &
} >> ~/.xmonad/xmonad-config.log 2>&1

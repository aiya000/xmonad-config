#!/bin/bash
# shellcheck disable=SC2088
xmonad_dir=$(dirname "$0")

cd "$xmonad_dir" || (notify-send "$xmonad_dir is not found" ; exit 1)

{
    echo "$xmonad_dir/replace.sh starts" &&
    stack clean &&
    stack install &&
    stack exec -- xmonad-config --recompile &&
    stack exec -- xmonad-config --restart &&
    killall xmonad-x86_64-linux &&
    stack exec -- xmonad-config > /dev/null 2>&1 &
} | tee -a "$xmonad_dir/xmonad-config.log" 2>&1

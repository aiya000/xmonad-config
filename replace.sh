#!/bin/bash
stack install \
    && stack exec -- xmonad-config --recompile \
    && stack exec -- xmonad-config --restart \
    && killall xmonad-x86_64-linux \
    && xmonad-config > /dev/null 2>&1 &

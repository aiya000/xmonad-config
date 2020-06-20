#!/bin/bash

stack install
stack exec -- xmonad-config --recompile  # Execute ./build
stack exec -- xmonad-config --restart

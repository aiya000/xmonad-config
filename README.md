# xmonad-config
My xmonad configuration (xmonad.hs) with haskell-stack


# How to install

```shell
$ git clone {this repository} ~/.xmonad
$ stack install
```

- `$XMONAD_CONFIG_DIR` maybe able to use instead of `~/.xmonad` (I have not checked)


# How to use
Add the following this line into your .xinitrc

```shell
exec stack exec xmonad-config
```

and execute `startx`


# How to restart ?
This maybe succeed :dog2:

```console
$ stack install && stack exec -- xmonad-config --recompile && stack exec -- xmonad-config --restart && killall xmonad-x86_64-linux && xmonad-config > /dev/null 2>&1 &
```

# Another informations
- `./xmonad{,-contrib,-extras}` and `X11` are used by haskell-stack and `xmonad-config --recompile`

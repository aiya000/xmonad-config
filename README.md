# xmonad-config
My xmonad configuration (xmonad.hs) with haskell-stack


# How to install

```shell
$ stack install && xmonad-config-recompile
```


# How to use
Add the following this line into your .xinitrc

```shell
exec stack exec xmonad-config
```

and execute `startx`


# Another informations
- `./xmonad{,-contrib,-extras}` and `X11` are exists for debugging
- `xmonad-config-recompile` only do what is copying needed files
- You can quickly restart xmonad-config by `stack install && xmonad-config-recompile && stack exec -- xmonad-config --restart`

# xmonad-config
My xmonad configuration (xmonad.hs) with haskell-stack


# How to install

```shell
$ git clone {this repository} ~/.xmonad
$ stack install
```

- `$XMONAD_CONFIG_DIR` maybe able to use instead of `~/.xmonad` (I have not checked)


# How to use
Add the following this line into your .xinitrc after `stack install`

```shell
exec ~/.local/bin/xmonad-config
```

and execute `startx`


# How to restart ? <a name="xmonad-restart"></a>
like below :dog2:

```console
$ stack install && stack exec xmonad-config --recompile && stack exec xmonad-config --restart && killall xmonad-x86_64-linux && xmonad-config > /dev/null 2>&1 &
```


# I [restarted xmonad](#xmonad-restart), but anything wasn't changed
Please try :point_down: and [restart xmonad](#xmonad-restart) again

```shell-session
$ rm $(find .stack-work/install -type f -name xmonad-config | head -1)
```

# :diamond_shape_with_a_dot_inside: xmonad-config :diamond_shape_with_a_dot_inside:

This is either

- A xmonad confiuration
- A windows manager implementation
    - that is implemented by xmonad as a window manager buliding library

To build this project, using `haskell-stack`.

## How to install

```shell-session
$ git clone {this repository} ~/.xmonad
$ cd ~/.xmonad
$ stack install
```

## How to run

Add the following this line into your .xinitrc after `stack install`

```shell-session
exec ~/.local/bin/xmonad-config
```

Also execute `startx`

## How to restart (like xmonad's `--replace`)

```shell-session
$ ./replace.sh
```

or

```shell-session
$ xmonad-config --replace
```

## :exclamation: NOTICE :exclamation:

The xmonad-config's build script (`./build`) finds only a runnable binary 'xmonad-config' from .stack-work.

After the stack resolver version upgraded, we must run :point_down:

```shell-session
$ rm -rf .stack-work
```

## Implementations

- [standalone](https://github.com/aiya000/xmonad-config/tree/standalone): To use conky and dzen2
- [with-xfce4](https://github.com/aiya000/xmonad-config/tree/with-xfce4): To use xfce4-panel

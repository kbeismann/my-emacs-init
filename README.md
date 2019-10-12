# Emacs initialization file

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)


## Commentary

This Emacs initialization file creates my personalized version of GNU Emacs: a
non-invasive editor with a minimalist design.  It highlights relevant
information only while hiding non-essential elements whenever possible.
Currently, it uses `leaf` to create a modular system.  Dependent on the
system, the bitmap fonts have to be installed and configured upfront.  Note
that Pango removed support for bitmap fonts with version 1.33.

Some optional settings, e.g. my `mu4e` setup, are not part of this repository.



## Installation (UNIX-based)

This setup is currently tested with GNU Emacs 26.3.

1. Install Emacs:
```bash
$ sudo pacman -S emacs
```
2. Install fonts, e.g. Dina:
```bash
$ sudo pacman -S dina-font
```
3. Create a symbolic link to `~/.emacs` or
  `~/.emacs.d/init.el`:
```bash
$ mkdir ~/.emacs.d/
$ cd ~/gitdir/emacs-init/
$ ln -s ./init.el ~/.emacs.d/init.el
```
4. Start Emacs:
```bash
$ emacs
```


## Working font options

* Hack:
  * `"Hack-9"` ; Arch
  * `"Hack:pixelsize=14"` ; Arch

* DejaVu Sans Mono:
  * `"DejaVu Sans Mono-10"`; Arch

* Inconsolata:
  * `"Inconsolata-11"`
  * `"Inconsolata:pixelsize=14"` ; Arch

* Dina:
  * `"Dina-9"`
  * `"Dina:pixelsize=12"` ; Arch, Manjaro
  * `"-*-dina-medium-r-*-*-12-*-*-*-*-*-*-*"`

* Terminus:
  * `"Terminus-12"`
  * `"xos4 Terminus-10"` ; Arch, Manjaro
  * `"-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"`
  * `"-xos4-terminus-medium-r-normal-*-14-120-*-*-*-*-*-*"` ; Ubuntu
  * `"-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*"` ; Ubuntu


## Sources (incomplete)

* https://github.com/rememberYou/.emacs.d/blob/master/config.org/
* https://github.com/conao3/dotfiles/blob/master/.dotfiles/.emacs.d/init.el
* https://gitlab.com/k-bps/mesk/blob/master/src/init.org

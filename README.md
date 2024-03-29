# Emacs initialization file

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)


## Commentary

Currently, this Emacs setup uses `leaf` to create a modular system.  Dependent
on the system, the bitmap fonts have to be installed and configured upfront.
Note that Pango removed support for bitmap fonts with version 1.33.

Some optional settings, e.g. my `mu4e` setup, are not part of this repository.


## Installation (UNIX-based)

This setup is currently tested with GNU Emacs 28.0.50.

1. Install Emacs:
```bash
$ sudo pacman -S emacs
```
Or to use the most up-to-date version:
```bash
git clone https://github.com/emacs-mirror/emacs.git emacs
```


2. Install fonts, e.g. Dina, compatible with Pango 1.44, from the AUR via
   `yay`:

```bash
$ sudo yay -S dina-font-otb
```

Keep in mind bitmap fonts do not work with Pango 1.44 if they are not in an
`.otb` format.

3. Clone the repository:

```
cd ~/gitdir/
git clone https://github.com/kbeismann/emacs-init emacs-init
```

4. Create a symbolic link to `~/.emacs` or
  `~/.emacs.d/init.el`:

```bash
$ mkdir ~/.emacs.d/
$ cd ~/gitdir/emacs-init/
$ ln -s ./init.el ~/.emacs.d/init.el
```
5. Start Emacs:
```bash
$ emacs
```


## Some working font options

Out of date!

* Dina:
  * `"Dina-9"`-- Working with Pango 1.44 when using the AUR version dina-font-otb.
  * `"Dina:pixelsize=12"` -- Arch, Manjaro
  * `"-*-dina-medium-r-*-*-12-*-*-*-*-*-*-*"`

* Terminus:
  * `"Terminus-12"`
  * `"xos4 Terminus-10"` -- Arch, Manjaro
  * `"-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"`
  * `"-xos4-terminus-medium-r-normal-*-14-120-*-*-*-*-*-*"` -- Ubuntu
  * `"-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*"` -- Ubuntu

* Hack:
  * `"Hack-9"` -- Arch
  * `"Hack:pixelsize=14"` -- Arch

* DejaVu Sans Mono:
  * `"DejaVu Sans Mono-10"` -- Arch

* Inconsolata:
  * `"Inconsolata-11"`
  * `"Inconsolata:pixelsize=14"` -- Arch


## To-do

* TODO: Structure > External repository for snippets.
* TODO: Fix > The org/org-ref setup is too messy.
* TODO: Minor > Add global settings for Org Columns view.
* TODO: Formatting > Format this file with a formatter.
* TODO: Packages > Fix YASnippet.
* TODO: Use straight.el.
* TODO: Optimize startup time.


## Sources (incomplete)

* https://github.com/rememberYou/.emacs.d/blob/master/config.org/
* https://github.com/conao3/dotfiles/blob/master/.dotfiles/.emacs.d/init.el
* https://gitlab.com/k-bps/mesk/blob/master/src/init.org
* https://github.com/rememberYou/.emacs.d/
* https://github.com/zamansky/using-emacs
* https://ladicle.com/post/config/
* https://www.gnu.org/software/emacs/
* https://github.com/kimim/kimim-emacs
* https://github.com/MatthewZMD/.emacs.d
* https://github.com/progfolio/.emacs.d

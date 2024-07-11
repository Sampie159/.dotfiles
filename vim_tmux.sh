#!/bin/bash

ln -sf ~/.dotfiles/.vimrc ~/
ln -sf ~/.dotfiles/.vim ~/
ln -sf ~/.dotfiles/tmux ~/.config/
curl -O http://gnu.c3sl.ufpr.br/ftp/emacs/emacs-29.4.tar.gz
tar xf emacs-29.4.tar.gz
cd emacs-29.4/
mkdir build && cd build
../configure --with-gnutls=ifavailable --with-gif=ifavailable
make -j4
ln -sf ~/.dotfiles/emacs ~/.config/
rm -rf ~/.emacs.d/
setxkbmap -option ctrl:nocaps

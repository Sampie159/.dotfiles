#!/usr/bin/env bash

set -e

mkdir -p ~/.config ~/.local/bin

ln -sf ~/.dotfiles/bin/* ~/.local/bin/
ln -sf ~/.dotfiles/templates/* ~/.config/wal/templates

configs=(
    tmux
    hypr
    waybar
    nvim
    rofi
    emacs
    fish
    Kvantum
)

for config in "${configs[@]}"; do
    if [ ! -d ~/.config/"${config}" ]; then
        ln -sf ~/.dotfiles/"${config}" ~/.config/
    fi
done

home_configs=(
    .gitconfig
    Wallpapers
)

for home_config in "${home_configs[@]}"; do
    if [ ! -d ~/"${home_config}" ]; then
        ln -sf ~/.dotfiles/"${home_config}" ~/
    fi
done

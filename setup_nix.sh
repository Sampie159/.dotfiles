#!/usr/bin/env bash

cp /etc/nixos/hardware-configuration.nix ./
git add -A
sudo nixos-rebuild switch --flake ~/.dotfiles#nixos

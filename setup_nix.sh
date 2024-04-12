#/bin/bash

cp /etc/nixos/hardware-configuration.nix .
git add hardware-configuration.nix
sudo nixos-rebuild switch --flake .

mako_example="background-color=#00000000
text-color=#000000
border-color=#000000
border-radius=8
default-timeout=5000
width=500"

mkdir ~/.config/mako
echo "$mako_example" > ~/.config/mako/config

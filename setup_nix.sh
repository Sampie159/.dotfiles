#/bin/bash

cp /etc/hardware-configuration.nix .
git add hardware-configuration.nix
sudo nixos-rebuild switch --flake .

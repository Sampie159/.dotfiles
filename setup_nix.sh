#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

flakes="--extra-experimental-features nix-command --extra-experimental-features flakes"

cp /etc/nixos/hardware-configuration.nix .

nix $flakes flake update

git add -A -- \
    hardware-configuration.nix \
    flake.nix flake.lock \
    configuration.nix home.nix \
    hypr templates \
    waybar rofi ghostty tmux nvim fish bin Wallpapers

sudo nixos-rebuild switch $flakes --flake .

seed_home=/home/sampie
install -d -o sampie -g users "$seed_home/.config/mako"
if [ ! -e "$seed_home/.config/mako/config" ]; then
    cat > "$seed_home/.config/mako/config" <<'EOF'
background-color=
text-color=
border-color=
border-radius=8
default-timeout=5000
width=500
EOF
    chown sampie:users "$seed_home/.config/mako/config"
fi

#!/usr/bin/env bash

ln -sf ~/.dotfiles/discord-flags.conf ~/.config/discord-flags.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -sf ~/.dotfiles/bin/* ~/.local/bin/
ln -sf ~/.dotfiles/templates/* ~/.config/wal/templates
rm -rf ~/.emacs.d/

mako_example="background-color=#00000000
text-color=#000000
border-color=#000000
border-radius=8
default-timeout=5000
width=500"

mkdir ~/.config/mako
echo "$mako_example" > ~/.config/mako/config

programs="tmux pipewire pipewire-alsa pipewire-jack pipewire-pulse alsa-firmware lazygit alacritty git discord linux-headers nvidia-dkms firefox neovim steam btop fzf ripgrep gamemode mpv mangohud lib32-mangohud rofi mako pass slurp grim zoxide qt6ct qt5ct kvantum kvantum-qt5 sccache v4l2loopback-dkms python-pywal"

sudo pacman -S --needed $programs --noconfirm

directories=(.config .local/bin)

for dir in "${directories[@]}"; do
    if [ ! -d "${dir}" ]; then
	mkdir ~/"${dir}"
    fi
done

if [ ! -d ~/.config ]; then
    mkdir ~/.config
fi

if [ ! -d ~/.local/bin ]; then
    mkdir ~/.local/bin
fi

git clone https://aur.archlinux.org/paru.git && cd paru
makepkg -si --noconfirm
cd .. && rm -rf paru

programs_aur="python-pywalfox swww xdg-desktop-portal-hyprland waybar wl-clipboard rar emacs-wayland vencord-desktop pyprland all-repository-fonts matugen-bin"

paru -S --needed $programs_aur --noconfirm

configs=(
    tmux
    alacritty
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

curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install | fish
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
gsettings set org.gnome.desktop.interface gtk-theme Arc-Dark
fisher install rafaelrinaldi/pure
curl -sS https://starship.rs/install.sh | sh

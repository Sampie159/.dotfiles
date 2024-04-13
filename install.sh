#!/usr/bin/env bash

programs=(
    tmux
    screenkey
    flameshot
    redshift
    pipewire
    pipewire-alse
    pipewire-jack
    pipewire-pulse
    alsa-firmware
    lazygit
    alacritty
    awesome
    git
    discord
    linux-headers
    picom
    firefox
    steam
    lutris
    btop
    fzf
    ripgrep
    gamemode
    mpv
    mangohud
    lib32-mangohud
    dmenu
    rofi
    mako
    pass
    slurp
    grim
    zoxide
)

for program in "${programs[@]}"; do
    if ! command -v $program &> /dev/null; then
        echo "Installing $program"
        sudo pacman -S --needed $program --noconfirm
    fi
done

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

programs_aur=(
    python-pywalfox
    pywal-16-colors
    hyprland-git
    swww
    xdg-desktop-portal-hyprland
    waybar wl-clipboard
    kmonad-bin
    rar
    emacs-wayland
    vencord-desktop
    pyprland
)

for program in "${programs_aur[@]}"; do
    if ! command -v $program &> /dev/null; then
        echo "Installing $program"
        paru -S --needed $program --noconfirm
    fi
done

configs=(
    tmux
    conky
    alacritty
    awesome
    hypr
    waybar
    nvim
    picom
    rofi
    emacs
    fish
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

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install | fish
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
fisher install rafaelrinaldi/pure
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
ln -sf ~/.dotfiles/.zshrc ~/.zshrc
ln -sf ~/.dotfiles/.p10k.zsh ~/.p10k.zsh
ln -sf ~/.dotfiles/.zshenv ~/.zshenv
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

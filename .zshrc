# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
export RUSTC_WRAPPER=sccache

ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(
  archlinux
  # pass
  zsh-autosuggestions
  zsh-syntax-highlighting
)

. /opt/asdf-vm/asdf.sh

source $ZSH/oh-my-zsh.sh

# Current working directory name
dir_name() {
  local result=${PWD##*/}
  result=${result:-/}

  echo $result
}

# Pywal
# cat ~/.cache/wal/sequences
# source ~/.cache/wal/colors-tty.sh

# General aliases
alias nv="nvim"
alias po="poweroff"
alias rb="reboot"
alias sk="screenkey -p fixed -g 15%x5%+84%-15% &"
alias skk="killall screenkey"
alias btop="bpytop"
alias sd="shutdown now"
alias hx="helix"
alias ls="eza"
alias cat="bat"

# Git aliases
alias ga="git add"
alias gaa="git add ."
alias gc="git commit -m"
alias gck="git checkout"
alias gcb="git checkout -b"
alias gf="git fetch"
alias gm="git merge"
alias gpl="git pull"
alias gps="git push"
alias gr="git rebase"
alias gs="git status"

# GitHub CLI aliases
alias ghrn="gh repo create"
alias ghrc="gh repo clone"

# Rust aliases
alias ca="cargo add"
alias cb="cargo build"
alias cbr="cargo build --release"
alias cbp="cargo build --profile"
alias cr="cargo run"
alias crr="cargo run --release"
alias crp="cargo run --profile"
alias cw="cargo watch -x"
alias cwb="cargo watch -x build"
alias cwr="cargo watch -x run"
alias cwt="cargo watch -x test"

# MariaDB alias
alias mdb="mariadb -u root -p"

# Makefile aliases
# alias mc="make clean"
alias mr="make run"
alias mrl="make release"
alias mt="make test"

# PNPM aliases
alias nin="pnpm install"
alias ning="pnpm install -g"
alias nrb="pnpm run build"
alias nrbp="pnpm run build && pnpm run preview"
alias nrd="pnpm run dev"
alias pnpx="pnpm dlx"

# Prisma aliases
alias prin="pnpm install prisma @prisma/client"
alias prinit="pnpx prisma init --datasource-provider sqlite"
alias pps="pnpx prisma db push && pnpx prisma generate"

# Svelte aliases
alias svnew="pnpm create svelte@latest"
alias twin="pnpm install -D tailwindcss postcss autoprefixer"
alias twcfg="pnpx tailwindcss init tailwind.config.cjs -p"
alias luin="pnpm add lucia-auth @lucia-auth/adapter-prisma"

# T3 stack aliases
alias t3new="pnpm create t3-app@latest"

# Tmux aliases
alias t="tmux"
alias ta="tmux attach -t"
alias tns="tmux new -s"
alias tks="tmux kill-session"
alias tls="tmux ls"

function tmux_last_session() {
  LAST_TMUX_SESSION=$(tmux list-sessions | awk -F ":" '{print$1}' | tail -n1);
  tmux attach -t $LAST_TMUX_SESSION
}
bindkey -s '^s' 'tmux_last_session ^M'

# Haskell aliases
alias cblin="cabal install --ghc-options=-dynamic"
alias stin="stack install"

# Meson aliases
alias min="meson init build"
alias ms="meson setup build"
alias msw="meson setup --wipe build"
alias mcb="meson compile -C build"
alias mswcb="meson setup --wipe build && meson compile -C build"

# CMake aliases
alias cmin="cmake -S . -B debug -DCMAKE_BUILD_TYPE=Debug -G Ninja"
alias cmd="cmake --build debug"
alias cmi="sudo cmake --install release --prefix /usr/local"
alias cminr="cmake -S . -B release -DCMAKE_BUILD_TYPE=Release -G Ninja"
alias cmr="cmake --build release"
alias cminrd="cmake -S . -B release-debug -DCMAKE_BUILD_TYPE=RelWithDebInfo -G Ninja"
alias cmrd="cmake --build release-debug"

# pnpm
export PNPM_HOME="/home/sampie/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Ocaml aliases
alias ofmt="echo \"profile = janestreet\nversion = \`ocamlformat --version\`\n\
\nmargin = 90\ndoc-comments-padding = 100\nexp-grouping = preserve\"\
> .ocamlformat"
alias dip="dune init project"
alias db="dune build"
alias dbr="dune build --release"
alias de="dune exec \$(dir_name)"
alias dbe="dune build && dune exec \$(dir_name)"
alias dbre="dune build --release && dune exec \$(dir_name)"

# Zig aliases
alias zb="zig build -Doptimize=Debug"
alias zr="zig build run -Doptimize=Debug"
alias zt="zig build test -Doptimize=Debug"
alias zbr="zig build -Doptimize=ReleaseFast"
alias zrr="zig build -Doptimize=ReleaseFast run"
alias ztr="zig build -Doptimize=ReleaseFast test"
alias zig="zig"

# Pass aliases
alias pg="pass generate"
alias psc="pass show -c"

# Hyprland
alias hypr="Hyprland"

# opam configuration
[[ ! -r /home/sampie/.opam/opam-init/init.zsh ]] || source /home/sampie/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# Turso
export PATH="/home/sampie/.turso:$PATH"

[ -f "/home/sampie/.ghcup/env" ] && source "/home/sampie/.ghcup/env" # ghcup-env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/home/sampie/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<

eval "$(starship init zsh)"
eval "$(zoxide init --cmd cd zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

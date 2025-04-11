if status is-interactive
    # Commands to run in interactive sessions can go here
end

# cat /home/sampie/.cache/wal/sequences

abbr -a nv 'neovide &'
# abbr -a nv 'nvim'
abbr -a 4cd '4ed &'
abbr -a focus '~/Downloads/focus-linux &'
abbr -a po poweroff
abbr -a rb reboot
abbr -a sd 'shutdown now'
abbr -a hx helix
alias ls eza
alias cat bat
abbr -a hypr Hyprland
abbr -a pacin 'sudo pacman -S --needed'
abbr -a pacrem 'sudo pacman -Rns'

abbr -a ga 'git add'
abbr -a gaa 'git add .'
abbr -a gc 'git commit -m'
abbr -a gck 'git checkout'
abbr -a gcb 'git checkout -b'
abbr -a gf 'git fetch'
abbr -a gm 'git merge'
abbr -a gpl 'git pull'
abbr -a gps 'git push'
abbr -a gr 'git rebase'
abbr -a gs 'git status'

abbr -a ghrn 'gh repo create'
abbr -a ghrc 'gh repo clone'

abbr -a ca 'cargo add'
abbr -a cb 'cargo build'
abbr -a cbr 'cargo build --release'
abbr -a cbp 'cargo build --profile'
abbr -a cr 'cargo run'
abbr -a crr 'cargo run --release'
abbr -a crp 'cargo run --profile'
abbr -a cw 'cargo watch -x'
abbr -a cwb 'cargo watch -x build'
abbr -a cwr 'cargo watch -x run'
abbr -a cwt 'cargo watch -x test'
abbr -a musl-build 'RUSTFLAGS="-C linker=ld.lld -C relocation-model=static -C strip=symbols" cargo build --release --target x86_64-unknown-linux-musl'

abbr -a mr 'make run'
abbr -a mrl 'make release'
abbr -a mt 'make test'

abbr -a nin 'pnpm install'
abbr -a ning 'pnpm install -g'
abbr -a nrb 'pnpm run build'
abbr -a nrbp 'pnpm run build && pnpm run preview'
abbr -a nrd 'pnpm run dev'
abbr -a pnpx 'pnpm dlx'

abbr -a prin 'pnpm install prisma @prisma/client'
abbr -a prinit 'pnpx prisma init --datasource-provider sqlite'
abbr -a pps 'pnpx prisma db push && pnpx prisma generate'

abbr -a svnew 'pnpx sv create'
abbr -a twin 'pnpm install -D tailwindcss postcss autoprefixer'
abbr -a twcfg 'pnpx tailwindcss init tailwind.config.cjs -p'
abbr -a luin 'pnpm add lucia-auth @lucia-auth/adapter-prisma'

abbr -a t3new 'pnpm create t3-app@latest'

abbr -a t 'tmux'
abbr -a ta 'tmux attach -t'
abbr -a tns 'tmux new -s'
abbr -a tks 'tmux kill-session'
abbr -a tls 'tmux ls'

abbr -a cblin 'cabal install --ghc-options=-dynamic'
abbr -a stin 'stack install'

abbr -a min 'meson init build'
abbr -a ms 'meson setup build'
abbr -a msw 'meson setup --wipe build'
abbr -a mcb 'meson compile -C build'
abbr -a mswcb 'meson setup --wipe build && meson compile -C build'

abbr -a cmin 'cmake -S . -B debug -DCMAKE_BUILD_TYPE=Debug -G Ninja'
abbr -a cmd 'cmake --build debug --parallel'
abbr -a cmi 'sudo cmake --install release --prefix /usr/local'
abbr -a cminr 'cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -G Ninja'
abbr -a cmb 'cmake --build build --parallel'

abbr dip 'dune init project'
abbr db 'dune build'
abbr dbr 'dune build --release'
abbr dbw 'dune build --watch'

abbr zb 'zig build -Doptimize=Debug'
abbr zr 'zig build -Doptimize=Debug run'
abbr zt 'zig build -Doptimize=Debug test'
abbr zbr 'zig build -Doptimize=ReleaseFast'
abbr zrr 'zig build -Doptimize=ReleaseFast run'
abbr ztr 'zig build -Doptimize=ReleaseFast test'
abbr zf 'zig fetch'

abbr -a pg 'pass generate -c'
abbr -a psc 'pass show -c'

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/sampie/.ghcup/bin # ghcup-env

# opam configuration
source /home/sampie/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

fish_vi_key_bindings 2>/dev/null

# pnpm
set -gx PNPM_HOME "/home/sampie/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

zoxide init fish --cmd cd | source
COMPLETE=fish jj | source
starship init fish | source

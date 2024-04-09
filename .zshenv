export EDITOR="nvim"
export CC="clang"
export CXX="clang++"
export RUSTC_WRAPPER="sccache"
export ZVM_INSTALL="$HOME/.zmv/self"

typeset -U path PATH
path=($HOME/.mix/escripts $HOME/.zvm/self $ZVM_INSTALL/ $HOME/.nimble/bin $HOME/.config/emacs/bin $HOME/.local/bin $HOME/.nimble/bin $HOME/.cabal/bin $HOME/.npm-global/bin $(go env GOBIN) $(go env GOPATH)/bin $HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin $HOME/.cargo/bin $path)
. "$HOME/.cargo/env"

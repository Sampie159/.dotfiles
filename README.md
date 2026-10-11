# Installation Script for Sam's Needs

## To install everything simply run:

```sh
./install.sh
```

## Doom Emacs trial on NixOS

First run `git add doom/` so the Git-backed flake includes the new files.
Rebuild with `sudo nixos-rebuild switch --flake .#nixos`, then run `doom-emacs`.
The regular `emacs` command keeps using the existing configuration.

`doom/` starts with Doom's stock modules and settings plus direnv and C/C++ LSP
through Eglot, including Vim bindings, Corfu/Vertico completion, and the `doom-one`
theme. It imports none of `emacs/`. Other programming-language modules are
disabled by default.

Edit `doom/init.el` to select modules, `doom/packages.el` to add packages, and
`doom/config.el` to change settings. Rebuild Nix after edits; Nix manages the
packages, so do not run `doom sync` or `doom upgrade`. Update Doom with
`nix flake update nix-doom-emacs-unstraightened`, then rebuild.

Doom's `:tools direnv` module loads each project's environment into its buffers.
Home Manager already enables direnv and nix-direnv. For a project with a flake
development shell, put `use flake` in its `.envrc` and run `direnv allow` in that
project. Use `use nix` instead for a `shell.nix`. Open a project file in Doom;
its subprocesses use that environment. Run `M-x envrc-reload` to refresh it.

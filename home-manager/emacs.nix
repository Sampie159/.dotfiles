{
  lib,
  pkgs,
  inputs,
  link,
  ...
}:
let
  # packages missing from nixpkgs, pinned by flake.lock
  fromInput =
    epkgs: name:
    epkgs.trivialBuild {
      pname = name;
      version = "0-unstable";
      src = inputs.${name};
    };
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages =
      epkgs:
      with epkgs;
      [
        almost-mono-themes
        base16-theme
        cmake-mode
        corfu
        counsel
        doom-themes
        doric-themes
        ef-themes
        envrc
        exec-path-from-shell
        general
        glsl-mode
        go-mode
        highlight-numbers
        ivy
        kaolin-themes
        lsp-ivy
        lsp-mode
        lua-mode
        magit
        modus-themes
        multiple-cursors
        nofrils-acme-theme
        parchment-theme
        parinfer-rust-mode
        plain-theme
        projectile
        ripgrep
        rust-mode
        seq
        sql-indent
        sudo-edit
        tao-theme
        transient
        treesit-auto
        vterm
        vterm-toggle
        which-key
        zig-mode
        (treesit-grammars.with-grammars (g: [
          g.tree-sitter-go
          g.tree-sitter-rust
        ]))
      ]
      ++ map (fromInput epkgs) [
        "odin-mode"
        "slang-mode"
      ];
  };

  # per entry, not the whole dir: runtime files (custom-file.el, caches) stay out of the repo
  xdg.configFile =
    lib.genAttrs
      [
        "emacs/init.el"
        "emacs/config.el"
        "emacs/packages.el"
        "emacs/themes"
      ]
      (path: {
        source = link path;
      });
}

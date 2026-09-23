{ pkgs, inputs, ... }:
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

  # recursive: runtime files (custom-file.el, caches) stay writable next to these
  xdg.configFile.emacs = {
    source = ../emacs;
    recursive = true;
  };
}

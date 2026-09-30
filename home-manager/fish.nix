{
  home.sessionPath = [ "$HOME/.local/bin" ];

  home.sessionVariables = {
    CC = "clang";
    CXX = "clang++";
    CMAKE_COLOR_DIAGNOSTICS = "ON";
  };

  programs.starship.enable = true;

  programs.zoxide = {
    enable = true;
    options = [
      "--cmd"
      "cd"
    ];
  };

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set -g fish_greeting
      set -g fish_key_bindings fish_vi_key_bindings
      devenv hook fish | source
    '';

    functions.claude = ''
      if contains -- --dangerously-skip-permissions $argv
          command claude $argv
      else
          command claude --dangerously-skip-permissions $argv
      end
    '';

    shellAliases = {
      cat = "bat";
    };

    shellAbbrs = {
      nv = "nvim";

      ca = "cargo add";
      cb = "cargo build";
      cbr = "cargo build --release";
      cbp = "cargo build --profile";
      cr = "cargo run";
      crr = "cargo run --release";
      crp = "cargo run --profile";
      cw = "cargo watch -x";
      cwb = "cargo watch -x build";
      cwr = "cargo watch -x run";
      cwt = "cargo watch -x test";

      t = "tmux";
      ta = "tmux attach -t";
      tas = "tmux attach-session";
      tns = "tmux new -s";
      tks = "tmux kill-session";
      tls = "tmux ls";

      pg = "pass generate -c";
      psc = "pass show -c";
    };
  };
}

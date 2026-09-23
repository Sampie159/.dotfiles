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
    '';

    functions.build = "./build.sh $argv";

    shellAliases = {
      ls = "eza";
      cat = "bat";
    };

    shellAbbrs = {
      nv = "nvim";
      "4cd" = "4ed &";
      focus = "~/Downloads/focus-linux &";
      po = "poweroff";
      rb = "reboot";
      sd = "shutdown now";
      hx = "helix";
      hypr = "Hyprland";

      ga = "git add";
      gaa = "git add .";
      gc = "git commit -m";
      gck = "git checkout";
      gcb = "git checkout -b";
      gf = "git fetch";
      gm = "git merge";
      gpl = "git pull";
      gps = "git push";
      gr = "git rebase";
      gs = "git status";

      ghrn = "gh repo create";
      ghrc = "gh repo clone";

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
      musl-build = ''RUSTFLAGS="-C linker=ld.lld -C relocation-model=static -C strip=symbols" cargo build --release --target x86_64-unknown-linux-musl'';

      mr = "make run";
      mrl = "make release";
      mt = "make test";

      nin = "pnpm install";
      ning = "pnpm install -g";
      nrb = "pnpm run build";
      nrbp = "pnpm run build && pnpm run preview";
      nrd = "pnpm run dev";
      pnpx = "pnpm dlx";

      prin = "pnpm install prisma @prisma/client";
      prinit = "pnpx prisma init --datasource-provider sqlite";
      pps = "pnpx prisma db push && pnpx prisma generate";

      svnew = "pnpx sv create";
      twin = "pnpm install -D tailwindcss postcss autoprefixer";
      twcfg = "pnpx tailwindcss init tailwind.config.cjs -p";
      luin = "pnpm add lucia-auth @lucia-auth/adapter-prisma";

      t3new = "pnpm create t3-app@latest";

      t = "tmux";
      ta = "tmux attach -t";
      tns = "tmux new -s";
      tks = "tmux kill-session";
      tls = "tmux ls";

      cblin = "cabal install --ghc-options=-dynamic";
      stin = "stack install";

      min = "meson init build";
      ms = "meson setup build";
      msw = "meson setup --wipe build";
      mcb = "meson compile -C build";
      mswcb = "meson setup --wipe build && meson compile -C build";

      cmin = "cmake -S . -B debug -DCMAKE_BUILD_TYPE=Debug -G Ninja";
      cmd = "cmake --build debug --parallel";
      cmi = "sudo cmake --install release --prefix /usr/local";
      cminr = "cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -G Ninja";
      cmb = "cmake --build build --parallel";

      dip = "dune init project";
      db = "dune build";
      dbr = "dune build --release";
      dbw = "dune build --watch";

      zb = "zig build -Doptimize=Debug";
      zr = "zig build -Doptimize=Debug run";
      zt = "zig build -Doptimize=Debug test";
      zbr = "zig build -Doptimize=ReleaseFast";
      zrr = "zig build -Doptimize=ReleaseFast run";
      ztr = "zig build -Doptimize=ReleaseFast test";
      zf = "zig fetch";

      pg = "pass generate -c";
      psc = "pass show -c";
    };
  };
}

{ config, lib, pkgs, inputs, isVM, ... }:

{
    imports = [
        ./hardware-configuration.nix
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # unset (stock kernel) on the vm host - cachyos targets real hardware.
    boot.kernelPackages = lib.mkIf (!isVM) pkgs.linuxPackages_cachyos;

    # FAT32 has no Unix permission bits, so the ESP always mounts world-
    # readable regardless of dir perms elsewhere - bootctl warns the random
    # seed file it stores there is exposed. umask=0077 makes the mount
    # itself root-only, silences it.
    fileSystems."/boot".options = [ "umask=0077" ];

    boot.kernelModules = [ "v4l2loopback" ];
    boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

    networking.hostName = "nixos";
    networking.networkmanager.enable = true;

    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    zramSwap = {
        enable = true;
        algorithm = "zstd";
        memoryPercent = 50;
    };

    time.timeZone = "America/Sao_Paulo";
    i18n.defaultLocale = "en_US.UTF-8";
    console.keyMap = "us";

    services = {
        displayManager.ly = {
            enable = true;
            x11Support = false;
        };

        pipewire = {
            enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
            jack.enable = true;
        };

        udisks2.enable = true;
        gvfs.enable = true;
    };

    # user "sampie" created manually post-install, not declared here.

    hardware.graphics = {
        enable = true;
        enable32Bit = true;
    };

    # RX 9070 XT (RDNA4) - Mesa from git via Chaotic Nyx (binary cache), plus
    # current firmware blobs for Navi 48. chaotic.mesa-git.enable itself is
    # set in flake.nix (only for the real-hardware host - the option doesn't
    # exist at all unless chaotic.nixosModules.default is imported, so it
    # can't just be mkIf'd from here on the vm host).
    hardware.enableRedistributableFirmware = true;

    fonts.packages = with pkgs; [
        noto-fonts
        noto-fonts-color-emoji
        noto-fonts-cjk-sans
        liberation_ttf
        google-fonts
        nerd-fonts.fira-code
        nerd-fonts.caskaydia-mono
    ];

    qt = {
        enable = true;
        platformTheme = "qt5ct";
        style = "kvantum";
    };

    # bare-install bootstrap: home-manager's programs.git is user-scoped
    # and only takes effect after a rebuild - root needs git on PATH too.
    environment.systemPackages = [ pkgs.git ];

    programs = {
        fish.enable = true;
        dconf.enable = true;
        nix-ld.enable = true;

        hyprland = {
            enable = true;
            package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
            portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
        };

        steam = {
            enable = true;
            remotePlay.openFirewall = true;
        };

        gamemode.enable = true;
    };

    security = {
        polkit.enable = true;
        rtkit.enable = true;
    };

    system.stateVersion = "26.05";
    nixpkgs.config.allowUnfree = true;

    nix = {
        settings = {
            experimental-features = [ "nix-command" "flakes" ];
            substituters = [ "https://hyprland.cachix.org" ];
            trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
        };
        optimise.automatic = true;
        gc = {
            automatic = true;
            dates = "weekly";
            options = "--delete-older-than 7d";
        };
    };
}

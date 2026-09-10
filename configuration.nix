{ config, lib, pkgs, inputs, ... }:

{
    imports = [
        ./hardware-configuration.nix
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.kernelPackages = pkgs.linuxPackages_cachyos;

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
    console.keyMap = "br-abnt2";

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

    users.users.sampie = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" ];
        shell = pkgs.fish;
        # bootstrap only - plaintext, world-readable in /nix/store.
        # `passwd` on first login to replace it (mutableUsers stays true).
        initialPassword = "changeme";
    };

    hardware.graphics = {
        enable = true;
        enable32Bit = true;
    };

    # RX 9070 XT (RDNA4) - Mesa from git via Chaotic Nyx (binary cache), plus
    # current firmware blobs for Navi 48.
    chaotic.mesa-git.enable = true;
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

    programs = {
        fish.enable = true;
        dconf.enable = true;
        nix-ld.enable = true;

        hyprland = {
            enable = true;
            package = inputs.hyprland.packages.${pkgs.system}.hyprland;
            portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
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

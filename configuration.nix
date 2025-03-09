{ config, lib, pkgs, inputs, ... }:

{
    imports = [
        ./hardware-configuration.nix
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.kernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" "uinput" ];

    networking.hostName = "nixos";
    networking.networkmanager.enable = true;

    time.timeZone = "Brazil/East";

    i18n.defaultLocale = "en_US.UTF-8";

    services = {
        xserver = {
            enable = true;
            videoDrivers = [ "nvidia" ];
            displayManager.sddm = {
                enable = true;
                wayland.enable = true;
            };
        };
        pipewire = {
            enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
            jack.enable = true;
        };
        pcscd.enable = true;
        udev.extraRules = ''
        KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput", GROUP="input", MODE="0660"
        '';
    };

    sound.enable = true;

    home-manager.users.sampie = {
        home.stateVersion = "23.11";
    };

    users.users.sampie = {
        isNormalUser = true;
        extraGroups = [ "wheel" "uinput" ];
        shell = pkgs.fish;
    };

    hardware = {
        opengl ={
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
        };

        nvidia = {
            modesetting.enable = true;
            nvidiaSettings = true;
            package = config.boot.kernelPackages.nvidiaPackages.stable;
        };
    };

    programs = {
        fish = {
            enable = true;
        };

        hyprland = {
            enable = true;
            package = inputs.hyprland.packages.${pkgs.system}.hyprland;
        };
        
        gnupg.agent = {
            enable = true;
            enableSSHSupport = true;
        };
    };

    xdg.portal.wlr.enable = true;
    security = {
        polkit.enable = true;
        rtkit.enable = true;
    };
    networking.firewall.enable = false;

    system.stateVersion = "23.11";
    nixpkgs.config.allowUnfree = true;
    nix = {
        settings = {
            experimental-features = [ "nix-command" "flakes" ];
            substituters = [ "https://hyprland.cachix.org" ];
            trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
            auto-optimise-store = true;
        };
        optimise.automatic = true;
        gc = {
            automatic = true;
            dates = "weekly";
            options = "--delete-older-than 7d";
        };
    };
}


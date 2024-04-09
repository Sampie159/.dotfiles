{ config, lib, pkgs, ... }:

{
    imports = [
        ./hardware-configuration.nix
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.kernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];

    networking.hostName = "nixos";
    networking.networkmanager.enable = true;

    time.timeZone = "Brazil/East";

    i18n.defaultLocale = "en_US.UTF-8";

    services = {
        xserver = {
            enable = true;
            displayManager.sddm.enable = true;
            videoDrivers = [ "nvidia" ];
        };
        pipewire = {
            enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
            jack.enable = true;
        };
    };

# Configure keymap in X11
# services.xserver.xkb.layout = "us";
# services.xserver.xkb.options = "eurosign:e,caps:escape";

    sound.enable = true;
# hardware.pulseaudio.enable = true;

    home-manager.users.sampie = {
        home.stateVersion = "23.11";
    };

    users.users.sampie = {
        isNormalUser = true;
        extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
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

    environment.systemPackages = with pkgs; [
        neovim
        wget
        gcc
        clang
        llvm
        rustup
        cmake
        gnumake
        libtool
        pkg-config
    ];

    programs = {
        fish = {
            enable = true;
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
    system.stateVersion = "23.11";
    nix.settings.experimental-features = [ "nix-command" "flakes" ];
    nixpkgs.config.allowUnfree = true;
}


;; Guix System config, VM test only - not the active provisioning path.
;; Rough parity with configuration.nix + home.nix, trimmed to what's
;; realistic for a first `guix system vm` boot. Known gaps vs the real
;; machine, left as-is on purpose:
;;   - kernel/firmware: nonguix's `linux` + `linux-firmware` swapped in
;;     (proves the channel resolves); the real RX 9070 XT + chaotic
;;     mesa-git story has no Guix equivalent, irrelevant in a VM anyway.
;;   - ghostty isn't packaged in Guix yet (vendors deps, zig fetch
;;     immature) - using `foot` as the Wayland-terminal stand-in.
;;   - nvim/ config assumes a *nightly* build (`vim._core.ui2`); Guix's
;;     `neovim` package is stable-branch only and will error on it.
;;   - audio/pipewire and login-manager wiring is left at %desktop-services
;;     defaults, not tuned to match the pipewire.jack/alsa32 setup in
;;     configuration.nix.
;;
;; Dotfiles are wired declaratively too - see home.scm (Guix Home), not a
;; separate shell script.
;;
;; Build/run: guix system vm config.scm

(use-modules (gnu)
             (gnu system nss)
             (gnu packages)
             (gnu packages shells)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules desktop networking linux)

(operating-system
  (host-name "guix-vm")
  (timezone "America/Sao_Paulo")
  (locale "en_US.utf8")
  (keyboard-layout (keyboard-layout "us"))

  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))))

  (file-systems (cons (file-system
                        (mount-point "/")
                        (device (file-system-label "guix-root"))
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "sampie")
                (comment "Samuel")
                (group "users")
                (shell (file-append fish "/bin/fish"))
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

  (packages (append (specifications->packages
                      '("hyprland" "waybar" "rofi" "mako" "foot"
                        "git" "emacs-pgtk" "tmux" "nss-certs"))
                    %base-packages))

  (services (append (list (service gdm-service-type)
                          (service network-manager-service-type)
                          (service zram-device-service-type))
                    %desktop-services)))

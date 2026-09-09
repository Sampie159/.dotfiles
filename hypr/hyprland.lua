-- Hyprland Lua config (Hyprland 0.55+). Lua is loaded INSTEAD of hyprland.conf
-- when this file exists; hyprland.conf is kept as a fallback (delete this file to
-- revert). Migrated 1:1 from hyprland.conf. See https://wiki.hypr.land/Configuring/
-- `hl` is an auto-injected global (no require needed).

-----------------------------------------------------------------------
-- Pywal colors. The old config did `source = ~/.cache/wal/colors-hyprland.conf`,
-- which Lua cannot do (require's package.path is the hypr config dir only, and a
-- .lua config cannot source a hyprlang .conf). Instead, pywal renders the template
-- templates/colors-hyprland.lua -> ~/.cache/wal/colors-hyprland.lua (a Lua table),
-- which we dofile by absolute path. pcall-guarded so a missing/broken cache file
-- can never crash the whole config.
-----------------------------------------------------------------------
local function load_wal()
  local ok, t = pcall(dofile, os.getenv("HOME") .. "/.cache/wal/colors-hyprland.lua")
  if ok and type(t) == "table" then return t end
  return { color11 = "rgb(5056AB)" } -- fallback (last-known value) if pywal hasn't run
end
local wal = load_wal()

-----------------------------------------------------------------------
-- Monitor
-----------------------------------------------------------------------
hl.monitor({ output = "", mode = "2560x1440@180.00", position = "auto", scale = 1 }) -- ponytail: wildcard, survives port swaps (panel now on DP-2, was HDMI-A-1)

-----------------------------------------------------------------------
-- Environment variables
-----------------------------------------------------------------------
hl.env("XCURSOR_SIZE", "24")
--- hl.env("QT_IM_MODULE", "fcitx")
--- hl.env("XMODIFIERS", "@im=fcitx")

-----------------------------------------------------------------------
-- Autostart (old exec-once). One hl.exec_cmd per line, in the hyprland.start event.
-----------------------------------------------------------------------
hl.on("hyprland.start", function()
  hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
  hl.exec_cmd("xdph.sh")
  hl.exec_cmd("pypr")
  hl.exec_cmd("waybar")
  hl.exec_cmd("awww-daemon")
  hl.exec_cmd("discord --enable-wayland-ime")
  hl.exec_cmd("wl-clipboard")
  hl.exec_cmd("dropbox")
  hl.exec_cmd("mywal -r")
  hl.exec_cmd("mako")
  hl.exec_cmd("bash -c ~/.updatemirrorlist.sh &")
  -- hl.exec_cmd("fcitx5")
end)

-----------------------------------------------------------------------
-- Input
-----------------------------------------------------------------------
hl.config({
  input = {
    kb_layout  = "us,br",
    kb_variant = ",abnt2",
    kb_model   = "",
    kb_options = "grp:ctrls_toggle",
    kb_rules   = "",

    follow_mouse = 1,
    sensitivity  = 0, -- -1.0 - 1.0, 0 means no modification.

    touchpad = {
      natural_scroll = false,
    },
  },
})

-----------------------------------------------------------------------
-- General (col.inactive_border pulls the pywal color)
-----------------------------------------------------------------------
hl.config({
  general = {
    gaps_in  = 4,
    gaps_out = 8,
    border_size = 2,
    col = {
      active_border   = "rgba(ffffffee)",
      inactive_border = wal.color11 or "rgb(5056AB)",
    },
    layout = "dwindle",
  },
})

-----------------------------------------------------------------------
-- Decoration
-----------------------------------------------------------------------
hl.config({
  decoration = {
    rounding = 8,
    blur = {
      enabled = true,
      size    = 3,
      passes  = 1,
    },
    shadow = {
      enabled = true,
      color   = "rgba(1a1a1aee)",
    },
  },
})

-----------------------------------------------------------------------
-- Animations
-----------------------------------------------------------------------
hl.config({ animations = { enabled = true } })

hl.curve("myBezier", { type = "bezier", points = { {0.05, 0.9}, {0.1, 1.05} } })

hl.animation({ leaf = "windows",     enabled = true, speed = 7,  bezier = "myBezier" })
hl.animation({ leaf = "windowsOut",  enabled = true, speed = 7,  bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border",      enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 8,  bezier = "default" })
hl.animation({ leaf = "fade",        enabled = true, speed = 7,  bezier = "default" })
hl.animation({ leaf = "workspaces",  enabled = true, speed = 6,  bezier = "default" })

-----------------------------------------------------------------------
-- Dwindle
-----------------------------------------------------------------------
hl.config({ dwindle = { preserve_split = true } })

-----------------------------------------------------------------------
-- Window rules: route apps to fixed workspaces
-----------------------------------------------------------------------
hl.window_rule({ name = "workspace-1",   match = { class = "(firefox)$" },                             workspace = "1" })
hl.window_rule({ name = "workspace-2",   match = { class = "(discord)$" },                             workspace = "2" })
hl.window_rule({ name = "workspace-3",   match = { class = "(steam|lutris)$" },                        workspace = "3" })
hl.window_rule({ name = "workspace-3-2", match = { title = "(Saiyans Chronicles)$" }, workspace = "3" })
hl.window_rule({ name = "workspace-4", match = { title = "((Telegram)(.*))$" }, workspace = "4" })
hl.window_rule({ name = "workspace-4-2", match = { class = "(mpv)$" },                                 workspace = "4" })
hl.window_rule({ name = "workspace-9",   match = { title = "(Spotify)(.*)" },                          workspace = "9" })

-----------------------------------------------------------------------
-- Keybinds
-----------------------------------------------------------------------
local mainMod = "SUPER"

-- Apps / exec
hl.bind(mainMod .. " + CTRL + Return", hl.dsp.exec_cmd("ghostty"))
hl.bind(mainMod .. " + Return",        hl.dsp.exec_cmd("pypr toggle term && hyprctl dispatch bringactivetotop")) -- quick terminal
hl.bind(mainMod .. " + E",             hl.dsp.exec_cmd("thunar"))
hl.bind(mainMod .. " + B",             hl.dsp.exec_cmd("firefox"))
hl.bind(mainMod .. " + SHIFT + Return", hl.dsp.exec_cmd("pypr toggle rofi && hyprctl dispatch bringactivetotop"))
hl.bind(mainMod .. " + SHIFT + B",      hl.dsp.exec_cmd("pypr toggle btop && hyprctl dispatch bringactivetotop"))
hl.bind(mainMod .. " + W",              hl.dsp.exec_cmd("mywal -r")) -- re-theme from random wallpaper
hl.bind(mainMod .. " + SHIFT + P",      hl.dsp.exec_cmd("hyprpicker -a -f hex"))
-- Previously broken in hyprland.conf (typo `$mainMode`, an undefined variable, so
-- these binds did nothing). Restored to the obviously-intended SUPER bindings:
hl.bind(mainMod .. " + I",             hl.dsp.exec_cmd("pypr toggle irssi && hyprctl dispatch bringactivetotop"))
hl.bind(mainMod .. " + CTRL + P",      hl.dsp.exec_cmd("pypr reload"))

-- Window management
hl.bind(mainMod .. " + Q",             hl.dsp.window.close())                                    -- killactive
hl.bind(mainMod .. " + SHIFT + Q",     hl.dsp.exit())                                            -- exit
hl.bind(mainMod .. " + V",             hl.dsp.window.float({ action = "toggle" }))               -- togglefloating
hl.bind(mainMod .. " + P",             hl.dsp.window.pseudo())                                   -- pseudo (dwindle)
hl.bind(mainMod .. " + F",             hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))

-- Move focus (H/L/K/J -> left/right/up/down)
hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "down" }))

-- Move window (SHIFT + H/L/K/J)
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "left" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "right" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "up" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "down" }))

-- Workspaces: SUPER+1..9 -> ws1..9, SUPER+0 -> ws10 (SHIFT variant moves the window there).
for i = 1, 9 do
  hl.bind(mainMod .. " + " .. i,         hl.dsp.focus({ workspace = i }))
  hl.bind(mainMod .. " + SHIFT + " .. i, hl.dsp.window.move({ workspace = i }))
end
hl.bind(mainMod .. " + 0",         hl.dsp.focus({ workspace = 10 }))
hl.bind(mainMod .. " + SHIFT + 0", hl.dsp.window.move({ workspace = 10 }))

-- Scroll through workspaces with mainMod + mouse wheel
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize with mainMod + LMB/RMB drag
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Screenshot / screen recording (quotes -> Lua long brackets)
hl.bind("Print",                       hl.dsp.exec_cmd([[grim -g "$(slurp)" - | wl-copy]]))
hl.bind(mainMod .. " + Print",         hl.dsp.exec_cmd([[bash -c 'notify-send "Recording..." && wf-recorder -g "$(slurp)" -f ~/Videos/recording_$(date +%s).mp4']]))
hl.bind(mainMod .. " + SHIFT + Print", hl.dsp.exec_cmd([[bash -c 'pkill -INT wf-recorder && notify-send "Recording stopped"']]))

-- Zoom (pyprland). If Equal/Minus silently fail, switch to lowercase equal/minus.
hl.bind(mainMod .. " + Equal",         hl.dsp.exec_cmd("pypr zoom"))
hl.bind(mainMod .. " + SHIFT + Equal", hl.dsp.exec_cmd("pypr zoom +1"))
hl.bind(mainMod .. " + Minus",         hl.dsp.exec_cmd("pypr zoom -1"))

-- Media keys (locked = fire even while screen is locked)
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02+"), { locked = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02-"), { locked = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),   { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl --ignore-player=firefox play-pause"), { locked = true })
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl --ignore-player=firefox next"),       { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl --ignore-player=firefox previous"),   { locked = true })
hl.bind(mainMod .. " + XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 1"),   { locked = true })
hl.bind(mainMod .. " + XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.4"), { locked = true })

-- Brightness
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl set +5%"), { locked = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 5%-"), { locked = true })

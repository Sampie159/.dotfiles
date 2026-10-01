-- require() not dofile(): Hyprland watches required files, so pywal rewrites reload the config
local ok, wal = pcall(require, "~/.cache/wal/colors-hyprland.lua")
if not ok or type(wal) ~= "table" then wal = {} end

local mod = "SUPER"

hl.monitor({ output = "", mode = "2560x1440@180.00", position = "auto", scale = 1 })

hl.env("XCURSOR_SIZE", "24")
hl.env("XCURSOR_THEME", "Adwaita")
hl.env("QT_QPA_PLATFORMTHEME", "qt5ct")
hl.env("QT_STYLE_OVERRIDE", "kvantum")

hl.on("hyprland.start", function()
  hl.exec_cmd("pypr")
  hl.exec_cmd("awww-daemon")
  hl.exec_cmd("mywal -r")
  hl.exec_cmd("quickshell")
  hl.exec_cmd("discord --enable-wayland-ime")
  hl.exec_cmd("dropbox")
end)

hl.config({
  input = {
    kb_layout = "us,br",
    kb_variant = ",abnt2",
    kb_model = "",
    kb_options = "grp:ctrls_toggle",
    kb_rules = "",
    follow_mouse = 1,
    sensitivity = 0,
    touchpad = { natural_scroll = false },
  },

  general = {
    gaps_in = 4,
    gaps_out = 8,
    border_size = 2,
    col = {
      active_border = "rgba(ffffffee)",
      inactive_border = wal.color11 or "rgb(5056AB)",
    },
    layout = "dwindle",
  },

  decoration = {
    rounding = 8,
    blur = { enabled = true, size = 3, passes = 1 },
    shadow = { enabled = true, color = "rgba(1a1a1aee)" },
  },

  animations = { enabled = true },
  dwindle = { preserve_split = true },
})

hl.curve("myBezier", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.05 } } })

hl.animation({ leaf = "windows", enabled = true, speed = 7, bezier = "myBezier" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 7, bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 8, bezier = "default" })
hl.animation({ leaf = "fade", enabled = true, speed = 7, bezier = "default" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 6, bezier = "default" })

hl.window_rule({ name = "workspace-1", match = { class = "(firefox)$" }, workspace = "1" })
hl.window_rule({ name = "workspace-2", match = { class = "(discord)$" }, workspace = "2" })
hl.window_rule({ name = "workspace-3", match = { class = "(steam|lutris)$" }, workspace = "3" })
hl.window_rule({ name = "workspace-3-2", match = { title = "(Saiyans Chronicles)$" }, workspace = "3" })
hl.window_rule({ name = "workspace-4", match = { title = "((Telegram)(.*))$" }, workspace = "4" })
hl.window_rule({ name = "workspace-4-2", match = { class = "(mpv)$" }, workspace = "4" })
hl.window_rule({ name = "workspace-9", match = { title = "(Spotify)(.*)" }, workspace = "9" })

local function exec(key, cmd)
  hl.bind(key, hl.dsp.exec_cmd(cmd))
end

local function scratch(key, name)
  exec(key, "pypr toggle " .. name .. " && hyprctl dispatch bringactivetotop")
end

local function locked(key, cmd)
  hl.bind(key, hl.dsp.exec_cmd(cmd), { locked = true })
end

exec(mod .. " + CTRL + Return", "ghostty")
scratch(mod .. " + Return", "term")
exec(mod .. " + E", "nautilus")
exec(mod .. " + B", "firefox")
exec(mod .. " + SHIFT + Return", "qs ipc call launcher toggle")
scratch(mod .. " + SHIFT + B", "btop")
exec(mod .. " + W", "mywal -r")
exec(mod .. " + SHIFT + P", "hyprpicker -a -f hex")
scratch(mod .. " + I", "irssi")
exec(mod .. " + CTRL + P", "pypr reload")

hl.bind(mod .. " + Q", hl.dsp.window.close())
hl.bind(mod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mod .. " + P", hl.dsp.window.pseudo())
hl.bind(mod .. " + F", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))

for key, dir in pairs({ H = "left", J = "down", K = "up", L = "right" }) do
  hl.bind(mod .. " + " .. key, hl.dsp.focus({ direction = dir }))
  hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ direction = dir }))
end

for ws = 1, 10 do
  local key = tostring(ws % 10)
  hl.bind(mod .. " + " .. key, hl.dsp.focus({ workspace = ws }))
  hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = ws }))
end

hl.bind(mod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

exec("Print", [[grim -g "$(slurp)" - | wl-copy]])
exec(mod .. " + Print", [[bash -c 'notify-send "Recording..." && wf-recorder -g "$(slurp)" -f ~/Videos/recording_$(date +%s).mp4']])
exec(mod .. " + SHIFT + Print", [[bash -c 'pkill -INT wf-recorder && notify-send "Recording stopped"']])

exec(mod .. " + Equal", "pypr zoom")
exec(mod .. " + SHIFT + Equal", "pypr zoom +1")
exec(mod .. " + Minus", "pypr zoom -1")

locked("XF86AudioRaiseVolume", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02+")
locked("XF86AudioLowerVolume", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.02-")
locked("XF86AudioMute", "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
locked("XF86AudioPlay", "playerctl --ignore-player=firefox play-pause")
locked("XF86AudioNext", "playerctl --ignore-player=firefox next")
locked("XF86AudioPrev", "playerctl --ignore-player=firefox previous")
locked(mod .. " + XF86AudioRaiseVolume", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1")
locked(mod .. " + XF86AudioLowerVolume", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.4")

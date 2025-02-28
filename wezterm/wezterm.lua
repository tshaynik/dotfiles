local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.hide_tab_bar_if_only_one_tab = true
config.font = wezterm.font 'Fira Code'
config.default_prog = { 'nu' }
config.xcursor_theme = 'Adwaita'
config.bidi_enabled = true
config.front_end = "WebGpu"
config.enable_wayland = false

config.keys = {
  {
    key = 'H',
    mods = 'CTRL',
    action = act.ActivatePaneDirection 'Left',
  },
  {
    key = 'L',
    mods = 'CTRL',
    action = act.ActivatePaneDirection 'Right',
  },
  {
    key = 'K',
    mods = 'CTRL',
    action = act.ActivatePaneDirection 'Up',
  },
  {
    key = 'J',
    mods = 'CTRL',
    action = act.ActivatePaneDirection 'Down',
  },
}

return config

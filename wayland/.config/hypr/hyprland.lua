-- Hyprland Lua config — converted from hyprland.conf (hyprlang, deprecated since 0.55)
-- Wiki: https://wiki.hypr.land/Configuring/Start/
-- Editor LSP stubs: /usr/share/hypr/stubs (add to Lua.workspace.library)

------------------
---- MONITORS ----
------------------
-- hl.monitor({ output = "Unknown-1", disabled = true })               -- Unknown autodetect
-- hl.monitor({ output = "DP-1", mode = "3440x1440@60", position = "0x0", scale = 1.0 }) -- External Dell
hl.monitor({ output = "eDP-1", scale = 1.0 })--, disabled = true })                   -- Laptop panel

---------------------
---- MY PROGRAMS ----
---------------------
local terminal  = "/usr/bin/alacritty"
local emacs     = os.getenv("HOME") .. "/.guix-profile/bin/emacsclient"
local helper    = os.getenv("HOME") .. "/bin/hypr-helper.py"

-------------------------
---- HELPER FUNCTIONS ----
-------------------------
-- Live in include/helper_functions.lua (require resolves relative to the
-- config dir). Add new helpers there, then destructure them here.
local helpers           = require("include/helper_functions")
local toggle_brightness = helpers.toggle_brightness
local toggle_bluetooth  = helpers.toggle_bluetooth
local cycle_display     = helpers.cycle_display
local send_to_special   = helpers.send_to_special
local toggle_special_ws = helpers.toggle_special_ws

-------------------
------ Events -----
-------------------
local function set_wallpaper(n)
    hl.exec_cmd("hyprctl hyprpaper wallpaper ,~/.config/hypr/workspace-" .. n .. ".png")
end

-- TODO: some of these should move into user services, the way we did hyprpaper
hl.on("hyprland.start", function()
    hl.exec_cmd("hypridle")
    hl.exec_cmd("waybar")
    hl.exec_cmd("/usr/bin/blueman-applet")
    hl.exec_cmd("/usr/bin/nm-applet")
    set_wallpaper(1)
end)

hl.on("workspace.active", function(ws)
    set_wallpaper(ws.name)
end)

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("EDITOR", "~/.guix-profile/bin/emacsclient")
hl.env("WLR_NO_HARDWARE_CURSORS", "1")

-----------------------
---- LOOK AND FEEL ----
-----------------------
hl.config({
    general = {
        -- These look great on the external display:
        -- gaps_in = 10, gaps_out = 6, border_size = 6
        -- TODO: figure out how to auto-switch based on monitor?
        -- These look better on the laptop:
        gaps_in     = 4,
        gaps_out    = 6,
        border_size = 4,

        col = {
            active_border   = "rgba(073642ff)", -- base02
            inactive_border = "rgba(FDF6E3ff)",
        },

        resize_on_border = false,
        allow_tearing    = false,
        layout           = "master",
    },

    decoration = {
        rounding         = 10,
        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        blur = {
            enabled  = false,
            size     = 3,
            passes   = 1,
            vibrancy = 0.1696,
        },

        shadow = {
            enabled        = true,
            render_power   = 1,
            color          = "rgba(44444455)",
            color_inactive = "rgba(44444455)",
            offset         = { 4, 4 },
        },
    },

    animations = { enabled = true },

    dwindle = {
        preserve_split = true, -- You probably want this
    },

    master = {
        new_status = "slave",
    },

    misc = {
        force_default_wallpaper = -1,  -- Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo   = true,
    },
})

-- Animations (speed is in ds: 7 = 700ms)
hl.animation({ leaf = "windows",     enabled = true, speed = 7,  bezier = "default", style = "popin 60%" })
hl.animation({ leaf = "windowsOut",  enabled = true, speed = 7,  bezier = "default", style = "popin 60%" })
hl.animation({ leaf = "border",      enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 8,  bezier = "default" })
hl.animation({ leaf = "fade",        enabled = true, speed = 7,  bezier = "default" })
hl.animation({ leaf = "workspaces",  enabled = true, speed = 6,  bezier = "default" })

---------------
---- INPUT ----
---------------
hl.config({
    input = {
        kb_layout  = "us",
        kb_variant = "dvorak",
        kb_model   = "",
        kb_options = "",
        kb_rules   = "",

        follow_mouse = 0,
        sensitivity  = 0.0, -- -1.0 - 1.0, 0 means no modification

        touchpad = {
            natural_scroll      = false,
            disable_while_typing = true,
            clickfinger_behavior = true,
            tap_to_click         = false,
        },
    },

    cursor = {
        -- Set while debugging disappearing cursor; may not actually help
        no_hardware_cursors = true,
        inactive_timeout    = 0,
    },
})

hl.device({
    name        = "ydotoold-virtual-device",
    kb_layout   = "us",
    kb_variant  = "basic",
    kb_options  = "",
})

-------------------
---- LID SWITCH ----
-------------------
-- Old lid binds caused problems with hyprlock; left out on purpose:
-- hl.bind("switch:on:Lid Switch", hl.dsp.exec_cmd('hyprctl keyword monitor "eDP-1, disable"'))
-- hl.bind("switch:off:Lid Switch", ...)

---------------------
---- KEYBINDINGS ----
---------------------
local mainMod = "SUPER"

-- hyprland functions
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.close())

-- Launching programs
hl.bind(mainMod .. " + return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + F",      hl.dsp.exec_cmd("librewolf -P default-release"))
hl.bind(mainMod .. " + I",      hl.dsp.exec_cmd("librewolf -P Fap"))
hl.bind(mainMod .. " + R",      hl.dsp.exec_cmd("/usr/bin/rofi -show run"))
hl.bind(mainMod .. " + escape", hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + E",      hl.dsp.exec_cmd(emacs .. " -n -c -a emacs"))
hl.bind(mainMod .. " + A",      hl.dsp.exec_cmd(emacs .. [[ -n -e '(+org-capture/open-frame)']]))
hl.bind(mainMod .. " + L",      hl.dsp.exec_cmd(emacs .. [[ -e "(my/llm-from-anywhere)"]]))
hl.bind(mainMod .. " + T",      hl.dsp.exec_cmd(emacs .. [[ -e "(command-execute 'google-translate-from-anywhere)"]]))
-- TODO: fix, and I think this needs -n
hl.bind(mainMod .. " + minus",  hl.dsp.exec_cmd(emacs .. [[ --eval '(emacs-everywhere)']]))
hl.bind(mainMod .. " + Z",      hl.dsp.exec_cmd("/usr/bin/rofimoji -s light -a type --keybinding-copy Control+y"))

-- Window management
hl.bind(mainMod .. " + D", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + W", hl.dsp.window.center())
hl.bind(mainMod .. " + A", hl.dsp.window.pin({ action = "toggle" }))
hl.bind(mainMod .. " + M",         send_to_special)     -- window -> special:N for current ws N
hl.bind(mainMod .. " + SHIFT + M", toggle_special_ws)   -- N <-> special:N
hl.bind(mainMod .. " + bracketright",      hl.dsp.window.cycle_next())
hl.bind(mainMod .. " + bracketleft",       hl.dsp.window.cycle_next({ next = false }))
hl.bind(mainMod .. " + SHIFT + bracketright", hl.dsp.layout("swapnext"))
hl.bind(mainMod .. " + SHIFT + bracketleft",  hl.dsp.layout("swapprev"))
hl.bind(mainMod .. " + 0",         hl.dsp.layout("focusmaster"))
hl.bind(mainMod .. " + SHIFT + 0", hl.dsp.layout("swapwithmaster"))

-- Layout orientation
hl.bind(mainMod .. " + space",       hl.dsp.layout("orientationnext"))
hl.bind(mainMod .. " + SHIFT + space", hl.dsp.layout("orientationprev"))

-- Resize
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.resize({ x = -60, y = 0,   relative = true }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.resize({ x = 0,   y = 60,  relative = true }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.resize({ x = 0,   y = -60, relative = true }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.resize({ x = 60,  y = 0,   relative = true }))

for i = 1, 9 do
  -- Switch workspaces with mainMod + [1-9]
  hl.bind(mainMod .. " + " .. i, hl.dsp.focus({ workspace = i }))

  -- Move active window to a workspace with mainMod + SHIFT + [1-9] (silent: don't follow)
  hl.bind(mainMod .. " + SHIFT + " .. i, hl.dsp.window.move({ workspace = i, follow = false }))
end

-- Scroll through existing workspaces with mainMod + n/p
hl.bind(mainMod .. " + N", hl.dsp.focus({workspace = 'e+1'}))
hl.bind(mainMod .. " + P",hl.dsp.focus({workspace = 'e-1'}))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Laptop multimedia keys (bindel == locked + repeating)
local mediaOpts = { locked = true, repeating = true }
-- F1
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd(emacs .. [[ -n -c -e "(command-execute 'dirvish)"]]), mediaOpts)
-- F2 cycle display: laptop -> external -> both (locked but NOT repeating —
-- holding the key shouldn't skip through states)
hl.bind("XF86AudioNext", cycle_display, { locked = true })
-- F3
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("~/bin/volume_change.sh mute"), mediaOpts)
-- F4
hl.bind("XF86AudioPlay", function() toggle_brightness("system76_acpi::kbd_backlight") end, mediaOpts)
-- F5
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("~/bin/volume_change.sh down"), mediaOpts)
-- F6
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("~/bin/volume_change.sh up"), mediaOpts)
-- F7: built-in keyboard sends TouchpadToggle; moonlander sends Stop
hl.bind("XF86TouchpadToggle", hl.dsp.exec_cmd("hyprpicker --autocopy --render-inactive"), mediaOpts)
hl.bind("XF86AudioStop",      hl.dsp.exec_cmd("hyprpicker --autocopy --render-inactive"), mediaOpts)
-- F8/F9: moonlander brightness (BIOS handles built-in keyboard)
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 5%-"), mediaOpts)
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl s +5%"), mediaOpts)
-- F10
hl.bind("pause",       hl.dsp.exec_cmd(helper .. " screenshot region"))
hl.bind("SHIFT + pause", hl.dsp.exec_cmd("dunstify -u normal 'TODO: Bind Full Screenshot'"))
hl.bind("ALT + pause",   hl.dsp.exec_cmd("dunstify -u normal 'TODO: Bind Window Screenshot'"))
-- F11
hl.bind("Scroll_Lock", toggle_bluetooth, mediaOpts)
-- F12
hl.bind("insert", hl.dsp.exec_cmd("/usr/bin/systemctl suspend"), mediaOpts)

--------------------------------
------- WORKSPACE RULES -------
--------------------------------
for i = 1, 9 do
  hl.workspace_rule({ workspace = i, persistent = true })
  -- One named special workspace per numbered workspace (special:1..special:9).
  -- Not persistent: created on demand, destroyed when emptied.
  hl.workspace_rule({ workspace = "special:" .. i })
end


--------------------------------
------- WINDOWS RULES  ---------
--------------------------------
hl.window_rule({
    name  = "dmenu-position",
    match = { initial_class = "(dmenu)" },
    move  = "0% 31",
})

-- Ignore maximize requests from apps. You'll probably like this.
hl.window_rule({
    name  = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
})

-- Fix some dragging issues with XWayland
hl.window_rule({
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },
    no_focus = true,
})

-- Emacs popups: float + center
local emacsPopupTitles = "^(?:doom-capture|Emacs Everywhere|emacs-dmenu-popup|emacs-gptel-popup)$"
hl.window_rule({
    name   = "emacs-popups",
    match  = { title = emacsPopupTitles },
    float  = true,
    center = true,
})

hl.window_rule({
    name  = "rofi-float",
    match = { class = "Rofi" },
    float = true,
})

hl.window_rule({
    name  = "google-translate-popup",
    match = { title = "emacs-google-translate-popup" },
    float = true,
    move  = "60% 50%",
})

-----------------
---- PLUGINS ----
-----------------
hl.config({

})

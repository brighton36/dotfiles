-- Helper functions for hyprland.lua
-- Loaded from the main config via: require("include/helper_functions")
-- (Hyprland resolves require paths relative to the config directory.)
--
-- Note: `hl` is Hyprland's config API global, available at call time.

local M = {}

local LAPTOP_OUTPUT = "eDP-1"

-- Run a command and return its stdout. Only use for fast commands (sysfs /
-- local D-Bus reads); bind callbacks must not block the compositor.
function M.sh_read(cmd)
    local f = assert(io.popen(cmd))
    local out = f:read("*a")
    f:close()
    return out
end

function M.notify(text)
    hl.notification.create({ text = text, timeout = 3000 })
end

-- Toggle a brightness device between off and max (was hypr-helper.py togglebrightness)
function M.toggle_brightness(device)
    local cur = tonumber(M.sh_read("brightnessctl -d '" .. device .. "' g")) or 0
    if cur == 0 then
        local max = M.sh_read("brightnessctl -d '" .. device .. "' m"):match("^%s*(%d+)")
        hl.exec_cmd("brightnessctl -d '" .. device .. "' s " .. (max or "max"))
    else
        hl.exec_cmd("brightnessctl -d '" .. device .. "' s 0")
    end
end

-- Toggle bluetooth power (was hypr-helper.py togglebluetooth)
function M.toggle_bluetooth()
    local powered = M.sh_read("bluetoothctl show"):match("Powered:%s+(%S+)")
    if powered then
        hl.exec_cmd("bluetoothctl power " .. (powered == "yes" and "off" or "on"))
    end
end

-- Any connected output that isn't the laptop panel, or nil. Uses
-- `monitors all` because disabled-but-connected outputs don't show up in
-- hl.get_monitors().
local function external_output()
    for name in M.sh_read("hyprctl monitors all"):gmatch("Monitor ([^ ]+) %(") do
        if name ~= LAPTOP_OUTPUT then
            return name
        end
    end
    return nil
end

-- Cycle display mode: laptop -> external -> both (was hypr-helper.py toggledisplay).
-- State is derived from the actual monitor layout each press, so it can't drift
-- (hotplug, lid close, etc). Applying hl.monitor() rules at runtime schedules a
-- monitor reload; safe here because one monitor always stays active.
function M.cycle_display()
    local ext = external_output()
    if not ext then
        M.notify("No external display connected")
        return
    end

    local laptop_on, ext_on = false, false
    for _, m in ipairs(hl.get_monitors()) do
        if m.name == LAPTOP_OUTPUT then
            laptop_on = true
        elseif m.name == ext then
            ext_on = true
        end
    end

    if laptop_on and ext_on then
        -- both -> laptop only
        hl.monitor({ output = ext, disabled = true })
        M.notify("Display: laptop only")
    elseif laptop_on then
        -- laptop only -> external only
        hl.monitor({ output = LAPTOP_OUTPUT, disabled = true })
        hl.monitor({ output = ext, mode = "preferred", position = "0x0", scale = 1.0 })
        M.notify("Display: external only (" .. ext .. ")")
    else
        -- external only (or neither) -> both
        hl.monitor({ output = LAPTOP_OUTPUT, mode = "preferred", position = "0x0", scale = 1.0 })
        hl.monitor({ output = ext, mode = "preferred", position = "auto-right", scale = 1.0 })
        M.notify("Display: both")
    end
end

-- Send the focused window to the special workspace paired with the current
-- numbered workspace (on ws 3 -> window goes to special:3). No-op when
-- already on a special workspace.
function M.send_to_special()
    local ws = hl.get_active_workspace()
    local n = ws and not ws.special and tonumber(ws.name) or nil
    if n then
        -- follow = false: stash silently, don't open/follow into the special ws
        hl.dispatch(hl.dsp.window.move({ workspace = "special:" .. n, follow = false }))
    end
end

-- Toggle between numbered workspace N and its special:N.
-- From N -> open special:N; from special:N -> focus N (deterministic, unlike
-- toggle_special's "return to previous" behavior).
function M.toggle_special_ws()
    local ws = hl.get_active_workspace()
    if not ws then return end
    if ws.special then
        local n = ws.name:match("^special:(%d+)$")
        if n then
            hl.dispatch(hl.dsp.focus({ workspace = tonumber(n) }))
        end
    else
        local n = tonumber(ws.name)
        if n then
            hl.dispatch(hl.dsp.workspace.toggle_special(tostring(n)))
        end
    end
end

return M

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

-- Toggle a brightness device between off and max
function M.toggle_brightness(device)
    local cur = tonumber(M.sh_read("brightnessctl -d '" .. device .. "' g")) or 0
    if cur == 0 then
        local max = M.sh_read("brightnessctl -d '" .. device .. "' m"):match("^%s*(%d+)")
        hl.exec_cmd("brightnessctl -d '" .. device .. "' s " .. (max or "max"))
    else
        hl.exec_cmd("brightnessctl -d '" .. device .. "' s 0")
    end
end

-- Toggle bluetooth power
function M.toggle_bluetooth()
    local powered = M.sh_read("bluetoothctl show"):match("Powered:%s+(%S+)")
    if powered then
        hl.exec_cmd("bluetoothctl power " .. (powered == "yes" and "off" or "on"))
    end
end

------------------------
---- VOLUME (F5/F6/F3) ----
------------------------
-- Ported from ~/bin/volume_change.sh (that script stays for the X/xmonad
-- session). pactl is a Pulse/PipeWire client, NOT a Hyprland-IPC client, so
-- shelling out to it from a bind callback is safe (no self-IPC deadlock).
--
-- Volume is set with a BLOCKING sh_read (not hl.exec_cmd) so the read-back
-- below can't race the async set. Both are fast local IPC round-trips.
--
-- Hyprland's native notification icons are a fixed set of 8 built-in glyphs
-- (warning/info/hint/error/ok/none/...); freedesktop icon names like
-- audio-volume-muted are NOT accepted by hl.notification.create, so the
-- volume note is text-only (the dunstify icons don't carry over).
local VOLUME_NOTE_TIMEOUT_MS = 1500
local function volume_beep()
    hl.exec_cmd("mpv --no-video --really-quiet /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
end

local function volume_pct()
    -- "Volume: front-left: 81941 / 125% / 5.82 dB,   front-right: ..."
    -- %% is a literal % in a Lua pattern; keep it INSIDE the capture to keep the symbol
    return M.sh_read("pactl get-sink-volume 0"):match("(%d+%%)") or "0%"
end

local function volume_muted()
    return M.sh_read("pactl get-sink-mute 0"):find("yes") ~= nil
end

-- Replace the on-screen "Volume ..." note in place (the Hyprland equivalent
-- of dunst's x-dunst-stack-tag, so F5/F6 repeats don't stack a note per
-- press). If it already expired, create a fresh one.
local function volume_notify(text)
    for _, n in ipairs(hl.notification.get()) do
        if n:get_text() and n:get_text():sub(1, 7) == "Volume:" then
            n:set_text(text)
            n:set_timeout(VOLUME_NOTE_TIMEOUT_MS) -- resets the timer
            return
        end
    end
    hl.notification.create({ text = text, timeout = VOLUME_NOTE_TIMEOUT_MS })
end

-- direction: "up" | "down" | "mute"
function M.volume_change(direction)
    local commands = {
        up = "pactl set-sink-volume 0 +5%",
        down = "pactl set-sink-volume 0 -5%",
        mute = "pactl set-sink-mute 0 toggle",
    }
    local cmd = commands[direction]
    if not cmd then return end

    M.sh_read(cmd) -- blocking: guarantees the read-back sees the new state
    local pct = volume_pct()
    if pct == "0%" or volume_muted() then
        volume_notify("Volume: muted")
    else
        volume_notify("Volume: " .. pct)
    end
    volume_beep()
end

-- Any connected output that isn't the laptop panel, or nil.
--
-- Reads DRM connector state from /sys/class/drm, NOT `hyprctl monitors all`.
-- Shelling out to hyprctl from a bind callback deadlocks: hyprctl is a client
-- of Hyprland's own IPC socket, and the callback blocks the main thread that
-- would service that request, so the child hangs and the callback never
-- returns. (Local-file reads like the lid/brightness ones are fine — they
-- don't round-trip through Hyprland's IPC.) sysfs tells us whether a
-- connector is physically plugged in, which is all we need; sysfs connector
-- basenames are Hyprland's output names with the `cardN-` prefix stripped.
local function external_output()
    for path, status in M.sh_read("for c in /sys/class/drm/*; do [ -f \"$c/status\" ] && printf '%s %s\\n' \"$c\" \"$(cat \"$c/status\" 2>/dev/null)\"; done 2>/dev/null")
        :gmatch("(/sys/class/drm/[%w%-]+) (%S+)") do
        if status == "connected" then
            local name = path:match("^/sys/class/drm/card%d+%-(.+)$")
            if name and name ~= LAPTOP_OUTPUT then
                return name
            end
        end
    end
    return nil
end

-- Is the laptop lid physically closed? Local file read (no IPC round-trip).
-- Matters because Hyprland keeps eDP-1 an enabled-but-invisible monitor with
-- the lid shut, so the "laptop on" state is meaningless while it's closed.
local function lid_closed()
    return M.sh_read("cat /proc/acpi/button/lid/*/state 2>/dev/null"):find("closed") ~= nil
end

-- Per-display gaps/borders (see the look-and-feel comment in hyprland.lua).
-- 0.56 has NO per-monitor gap config (monitor spec has no gap fields, window
-- rules can't match on monitor), so these are global values that we SWITCH
-- when the display mode changes: big ones when the external is up (it looks
-- great at 3440px), the compact ones when it's laptop-only. general:gaps_in /
-- gaps_out / border_size are dynamic config values (REFRESH_LAYOUTS /
-- REFRESH_WINDOW_STATES), so a runtime hl.config applies live — and it's a
-- local state change, safe from a bind/start callback (no IPC round-trip).
local GAPS = {
    external = { gaps_in = 10, gaps_out = 6, border_size = 6 },
    laptop   = { gaps_in = 4,  gaps_out = 6, border_size = 4 },
}

-- Set the gaps/border values for a display mode ("external" | "laptop").
-- Pass the TARGET mode from cycle_display — hl.monitor() applies on the next
-- render tick, so re-deriving from hl.get_monitors() right after the change
-- would read the OLD layout. When called without a mode (e.g. at startup)
-- the current layout decides: external on -> external gaps.
function M.apply_mode_gaps(mode)
    if not mode then
        mode = "laptop"
        local ext = external_output()
        if ext then
            for _, m in ipairs(hl.get_monitors()) do
                if m.name == ext then
                    mode = "external"
                    break
                end
            end
        end
    end
    local g = GAPS[mode] or GAPS.laptop
    hl.config({ general = {
        gaps_in     = g.gaps_in,
        gaps_out    = g.gaps_out,
        border_size = g.border_size,
    } })
end

-- Cycle display mode: laptop -> external -> both.
-- State is derived from the actual monitor layout each press, so it can't drift
-- on hotplug. Applying hl.monitor() rules at runtime schedules a monitor reload;
-- safe here because one monitor always stays active.
-- Lid closed: no-op — "laptop only"/"both" are invisible states and cycling
-- could disable the only visible display.
--
-- The F2 bind for this is DISABLED in hyprland.lua pending an upstream fix
-- (hyprwm/aquamarine#240 / #382, Hyprland discussion #13224): disabling a
-- connector with an in-flight page-flip can leave its flip-pending flag stale,
-- so the re-enable's modeset is rejected ("Cannot commit when a page-flip is
-- awaiting") and the display stays black until a compositor restart. A 500ms
-- re-enable delay (hl.timer) was tried and did NOT dodge the wedge on the
-- user's i915 Tiger Lake system — the function below is plain immediate mode
-- (disables first within a batch), kept wired and unit-tested so re-enabling
-- is one line once the fix ships.
function M.cycle_display()
    local ext = external_output()
    if not ext then
        M.notify("No external display connected")
        return
    end
    if lid_closed() then
        M.notify("Lid closed — display cycling skipped")
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
        -- both -> laptop only (eDP-1 is on by definition here, but stated
        -- explicitly so every branch owns its full target layout)
        hl.monitor({ output = ext, disabled = true })
        hl.monitor({ output = LAPTOP_OUTPUT, mode = "preferred", position = "0x0", scale = 1.0 })
        M.apply_mode_gaps("laptop")
        M.notify("Display: laptop only")
    elseif laptop_on then
        -- laptop only -> external only
        hl.monitor({ output = LAPTOP_OUTPUT, disabled = true })
        hl.monitor({ output = ext, mode = "preferred", position = "0x0", scale = 1.0 })
        M.apply_mode_gaps("external")
        M.notify("Display: external only (" .. ext .. ")")
    else
        -- external only (or neither) -> both
        hl.monitor({ output = ext, mode = "preferred", position = "auto-right", scale = 1.0 })
        hl.monitor({ output = LAPTOP_OUTPUT, mode = "preferred", position = "0x0", scale = 1.0 })
        M.apply_mode_gaps("external")
        M.notify("Display: both")
    end
end

-- The workspace the focused window is actually on. On 0.56.x a monitor tracks
-- its special separately from its regular active workspace, so
-- hl.get_active_workspace() reports the regular ws UNDERNEATH an open special
-- (its .special is always false there). The focused window's own .workspace
-- is the only reliable "am I on special:N?" signal. Falls back to the active
-- workspace when no window is focused.
local function focused_workspace()
    local win = hl.get_active_window()
    if win and win.workspace then return win.workspace end
    return hl.get_active_workspace()
end

-- Send the focused window to the special workspace paired with the current
-- numbered workspace (on ws 3 -> window goes to special:3). Toggles: when the
-- focused window sits on special:3, send it back to the (unspecial) numbered
-- regular workspace 3. No-op on a non-numbered workspace (or a named special
-- with no numbered pair).
function M.send_to_special()
    local ws = focused_workspace()
    if not ws then return end
    if ws.special then
        local n = ws.name:match("^special:(%d+)$")
        if n then
            hl.dispatch(hl.dsp.window.move({ workspace = n, follow = false }))
        end
    else
        local n = tonumber(ws.name)
        if n then
            -- follow = false: stash silently, don't open/follow into the special ws
            hl.dispatch(hl.dsp.window.move({ workspace = "special:" .. n, follow = false }))
        end
    end
end


function M.set_wallpaper(n)
    hl.exec_cmd("hyprctl hyprpaper wallpaper ,~/.config/hypr/workspace-" .. n .. ".png")
end

-- Toggle between numbered workspace N and its special:N.
-- From N -> open special:N; from special:N -> focus N (deterministic, unlike
-- toggle_special's "return to previous" behavior).
function M.toggle_special_ws()
    local ws = focused_workspace()
    if not ws then return end
    if ws.special then
        local n = ws.name:match("^special:(%d+)$")
        if n then
            hl.dispatch(hl.dsp.focus({ workspace = n }))
        end
    else
        local n = tonumber(ws.name)
        if n then
            hl.dispatch(hl.dsp.workspace.toggle_special(tostring(n)))
        end
    end
end

return M

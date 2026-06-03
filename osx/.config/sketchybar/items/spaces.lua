local sbar = require("sketchybar")

-- Define your workspaces (match these exactly to your aerospace.toml)
local workspaces = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
local space_items = {}

-- Helper to safely run shell commands and read output
local function exec(cmd)
    local handle = io.popen(cmd)
    local result = handle:read("*a")
    handle:close()
    return result:gsub("%s+", "") -- Remove whitespace
end

-- Draw the workspace items on the bar
for _, name in ipairs(workspaces) do
    local space = sbar.add("item", "space." .. name, {
        position = "left",
        icon = {
            string = name,
            color = 0xff7a88cf, -- Inactive workspace color
            highlight_color = 0xffffffff, -- Active workspace color
            padding_left = 8,
            padding_right = 8,
        },
        label = {
            drawing = false
        },
        background = {
            color = 0x00000000,
            height = 26,
            corner_radius = 6,
        }
    })
    
    -- Allow clicking the bar item to switch AeroSpace workspaces
    space:subscribe("mouse.clicked", function()
        os.execute("aerospace workspace " .. name)
    end)

    space_items[name] = space
end

-- Create the custom event listener
sbar.add("event", "aerospace_workspace_changed")

-- Background listener dummy item to process the event
local event_listener = sbar.add("item", "aerospace.listener", {
  drawing = false,
  updates = true
})

event_listener:subscribe("aerospace_workspace_changed", function(env)
   -- print('Debug: changed')
    -- Fallback to terminal query if the payload environment variable fails
    local focused = env.FOCUSED_WORKSPACE or exec("aerospace list-workspaces --focused")

    for name, item in pairs(space_items) do
        local is_focused = (name == focused)
        item:set({
            icon = { highlight = is_focused },
            background = {
                color = is_focused and 0x33ffffff or 0x00000000 -- Highlight background if focused
            }
        })
    end
end)

-- Initial run to highlight the correct workspace on startup
-- TODO If we uncomment this, startup takes a weirdly long time
-- sbar.trigger("aerospace_workspace_changed")


---------------------------------------------------------------------------
--- Vimium hotkeys for awful.hotkeys_widget
--- From: https://github.com/philc/vimium/blob/master/README.md
---------------------------------------------------------------------------

local hotkeys_popup = require("awful.hotkeys_popup.widget")
local vimium_rule_any = {name={"vimium", "Vimium"}}
for group_name, group_data in pairs({
    ["Vimium: navigation"] = { color = "#0088cc", rule_any = vimium_rule_any },
}) do
    hotkeys_popup.add_group_rules(group_name, group_data)
end

local vimium_keys = {
  ["Vimium: navigation"] = {
    {
      modifiers = {"Ctrl"},
      keys = {
        ['tab'] = "Move to the chat below",
        ['0'] = "Jump to saved messages",
        ['1-5'] = "Jump between folders",
        O = "Send file",
        w = "Minimize to system tray",
        m = "Minimize vimium",
        l = "Lock vimium",
        q = "Quit vimium",
        }
    },{
      modifiers = {},
      keys = {
        ['?'] = "Show help (in chrome)",
        Escape = "Go Back/Cancel/Exit",
      }
    }
  },
}

hotkeys_popup.add_hotkeys(vimium_keys)

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

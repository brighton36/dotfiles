---------------------------------------------------------------------------
--- Telegram hotkeys for awful.hotkeys_widget
--- From: https://usethekeyboard.com/telegram/
---------------------------------------------------------------------------

local hotkeys_popup = require("awful.hotkeys_popup.widget")
local telegram_rule_any = {name={"telegram", "Telegram"}}
for group_name, group_data in pairs({
    ["Telegram: chats"] = { color = "#0088cc", rule_any = telegram_rule_any },
    ["Telegram: messages"] = { color = "#0088cc", rule_any = telegram_rule_any }
}) do
    hotkeys_popup.add_group_rules(group_name, group_data)
end

local telegram_keys = {
  ["Telegram: chats"] = {
    {
      modifiers = {"Ctrl"},
      keys = {
        ['tab'] = "Move to the chat below",
        ['0'] = "Jump to saved messages",
        ['1-5'] = "Jump between folders",
        O = "Send file",
        w = "Minimize to system tray",
        m = "Minimize telegram",
        l = "Lock telegram",
        q = "Quit telegram",
        }
    },{
      modifiers = {"Ctrl+Shift"},
      keys = {
        ['tab'] = "Move to the chat above",
        Down = "Jump to the next folder",
        Up = "Jump to the previous folder",
        }
    },{
      modifiers = {},
      keys = {
        ['↑/↓'] = "In-Chat/Stickers/Emojis Navigation",
        Escape = "Go Back/Cancel/Exit",
      }
    }
  },
  ["Telegram: messages"] = {
    {
      modifiers = {"Ctrl"},
      keys = {
        ['↑/↓'] = "Reply to a message",
        Down = "Cancel Reply",
        e = "Edit Media",
        f = "Search selected chat"
      }
    },{
      modifiers = {"Alt"},
      keys = {
        Return = "Jump to the bottom of chat",
      }
    },{
      modifiers = {},
      keys = {
        Up = "Edit last message sent",
        Down = "Delete currently selected message",
      }
    }
  },
}

hotkeys_popup.add_hotkeys(telegram_keys)

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

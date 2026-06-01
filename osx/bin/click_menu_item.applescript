#!/usr/bin/osascript

-- clickmenu.applescript - Click a menu item in the active (frontmost) application.
-- Usage: ./clickmenu.applescript "Menu" "Menu Item"
--   e.g. ./clickmenu.applescript "File" "Save"

on run argv
if (count of argv) is not 2 then
  error "Usage: clickmenu.applescript \"Menu\" \"Menu Item\""
end if

set theMenu to item 1 of argv
set theItem to item 2 of argv

tell application "System Events"
  set frontApp to first application process whose frontmost is true
    tell frontApp
      click menu item theItem of menu theMenu of menu bar item theMenu of menu bar 1
    end tell
  end tell
end run


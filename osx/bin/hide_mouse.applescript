#!/usr/bin/osascript

use framework "CoreGraphics"
use scripting additions

-- Move mouse to bottom-left corner of the screen
tell application "Finder"
    set screenSize to bounds of window of desktop
    set screenHeight to item 4 of screenSize
end tell

current application's CGWarpMouseCursorPosition(current application's CGPointMake(0, screenHeight-5))

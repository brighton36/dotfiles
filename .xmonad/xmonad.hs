import Data.Map
import System.Exit
import Text.Printf

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.CopyWindow
-- We were using this with the key remapping... which would be nice to get working
-- import XMonad.Actions.PerWindowKeys
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks(avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Gaps
import XMonad.StackSet
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Paste

-- Colors ---------------------------------------------------------------------
data ColorSchemes = ColorSchemes{base03, base02, base01, base00, base0, base1, 
  base2, base3, black, white, yellow, orange, red, magenta, violet, blue, cyan, 
  green :: String}

mySolarized :: ColorSchemes
mySolarized = ColorSchemes {
  base03   = "#002b36",
  base02   = "#073642",
  base01   = "#586e75",
  base00   = "#657b83",
  base0   = "#839496",
  base1   = "#93a1a1",
  base2   = "#eee8d5",
  base3   = "#fdf6e3",
  black   = "#073642", -- TODO: nix
  white   = "#fdf6e3", -- TODO: nix
  yellow  = "#b58900",
  orange  = "#cb4b16",
  red     = "#dc322f",
  magenta = "#d33682",
  violet  = "#6c71c4",
  blue    = "#268bd2",
  cyan    = "#2aa198",
  green   = "#859900"
 }

-- Variables ------------------------------------------------------------------
home = "/home/cderose"                :: String
myModMask              = mod4Mask     :: KeyMask
myFocusFollowsMouse    = False        :: Bool
myBorderWidth          = 6            :: Dimension
myWindowGap            = 12           :: Integer
myColor                = mySolarized  :: ColorSchemes
myFocusedBorderColor   = blue myColor :: String
myUnFocusedBorderColor = white myColor :: String
myTerminal             = "alacritty"  :: String
myFilemanager          = "pcmanfm"    :: String
myBitmapsDir           = home++"/.xmonad/icons"

-- Keys -----------------------------------------------------------------------
toggleFloat w = windows (\s -> if Data.Map.member w (XMonad.StackSet.floating s)
  then XMonad.StackSet.sink w s
  else (XMonad.StackSet.float w (XMonad.StackSet.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

myKeys conf@(XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $
  -- xmonad commands:
  [ ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
  -- TODO: Recompile xmobar here as well probably make this a bin script
  , ((modMask              , xK_q ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ((modMask .|. shiftMask, xK_c ), kill)
  , ((modMask,               xK_a ), windows copyToAll)
  , ((modMask .|. shiftMask, xK_a ), killAllOtherCopies)
  , ((modMask,               xK_m ), withFocused minimizeWindow)
  , ((modMask,               xK_d ), withFocused toggleFloat)
  , ((modMask .|. shiftMask, xK_m ), withLastMinimized maximizeWindowAndFocus)
  , ((modMask,               xK_g ), gotoMenu)
  -- I decided to move xK_b to 'new' emacs buffer
  -- , ((modMask,               xK_b ), bringMenu)

  -- Launching Programs
  , ((modMask, xK_Return ), spawn $ terminal conf)
  , ((modMask, xK_f      ), spawn "firefox -P default-release")
  , ((modMask, xK_i      ), spawn "firefox -P Fap")
  , ((modMask, xK_c      ), spawn "/usr/bin/google-chrome-stable")
  , ((modMask, xK_r      ), spawn "/home/cderose/bin/dmenu_run_history")
  , ((modMask, xK_Escape ), spawn "xscreensaver-command -lock")

  -- Emacs bindings:
  , ((modMask, xK_e      ), spawn "emacsclient --eval \"(emacs-everywhere)\"")
  -- TODO: I think xk_b is probably the better shortrcut, but, defToggleStrutsKey
  -- seems to be registering this 
  , ((modMask, xK_minus), spawn "emacsclient -c -a emacs")

  -- Switch-to/Open Telegram
  -- I guess we decided against all this:
  -- , ((modMask, xK_t ), spawn "emacsclient -c -e '(telega)'")
  --, ((modMask, xK_t      ), runOrRaiseAndDo "/usr/bin/telegram-desktop" 
  --    (className =? "TelegramDesktop") (maximizeWindowAndFocus))

  -- Arrow Keys using modMask+(hjkl)
  -- I guess we decided against all this:
  -- , ((modMask, xK_h      ), (XMonad.Util.Paste.sendKey noModMask xK_Left))
  -- , ((modMask, xK_l      ), (XMonad.Util.Paste.sendKey noModMask xK_Right))
  -- , ((modMask, xK_k  ), (XMonad.Util.Paste.sendKey noModMask xK_Up))
  -- , ((modMask, xK_j  ), (XMonad.Util.Paste.sendKey noModMask xK_Down))

  -- Open Switchto/Open Mail
  -- NOTE: I didn't really use this, and I wanted meta-m for 'minimize'
  -- , ((modMask, xK_m      ), sequence_ [
  --     (runOrRaiseAndDo "/usr/bin/firefox http://mail.derosetechnologies.com" 
  --     (className =? "firefox") (maximizeWindowAndFocus)), 
  --     -- TODO: This isn't exactly working. Seemingly if you're not on the same
  --     -- workspace as firefox, it doesn't do what you'd expect...
  --     -- Maybe try xdokey...
  --     (XMonad.Util.Paste.sendKey mod1Mask xK_1)
  --     ])

  -- I disabled these-per application keys in lieu of autohotkey aroudn 2022-04-24
  -- Control-Right-Bracket to Control Tab:
  -- , ((controlMask, xK_bracketright ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome" <||> 
  --     className =? "TelegramDesktop", 
  --     XMonad.Util.Paste.sendKey controlMask xK_Tab), 
  --     (pure True, XMonad.Util.Paste.sendKey controlMask xK_bracketright) ])

  -- Control-Left-Bracket to Control Shift-Tab:
  -- , ((controlMask, xK_bracketleft ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome" <||> 
  --     className =? "TelegramDesktop", 
  --     XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Tab), 
  --     (pure True, XMonad.Util.Paste.sendKey controlMask xK_bracketleft) ])

  -- Control-Shift-Right-Bracket to Control-Shift-Pagedown:
  -- , ((controlMask .|. shiftMask, xK_bracketright ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome", 
  --    XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Page_Down), 
  --     (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_bracketright) ])

  -- Control-Shift-Left-Bracket to Control-Shift-Pageup:
  -- , ((controlMask .|. shiftMask, xK_bracketleft ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome", 
  --    XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Page_Up), 
  --     (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_bracketleft) ])

  -- Ctrl-Shift-l in browsers, to F6:
  -- (This toggles location-bar/body focus)
  -- , ((controlMask .|. shiftMask, xK_l ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome", 
  --     XMonad.Util.Paste.sendKey noModMask xK_F6), 
  --     (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_l) ])

  -- Ctrl-backspace in browsers, to Ctrl-F4:
  -- (This closes the browser tab)
  --, ((controlMask , xK_BackSpace ), bindFirst [ (
  --    className =? "firefox" <||> className =? "Google-chrome", 
  --    XMonad.Util.Paste.sendKey controlMask xK_F4), 
  --    (pure True, XMonad.Util.Paste.sendKey controlMask xK_BackSpace) ])

  -- Ctrl-shift-backspace in browsers, to Ctrl-shift-t:
  -- (This undo's the close-tab)
  -- , ((controlMask .|. shiftMask, xK_BackSpace ), bindFirst [ (
  --     className =? "firefox" <||> className =? "Google-chrome", 
  --     XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_t), 
  --     (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_BackSpace) ])

  -- Function Keys
  , ((noModMask, xK_F1 ), spawn "pcmanfm") -- FileManager
  , ((noModMask, xK_F2 ), spawn "/bin/false" )
  , ((noModMask, xK_F3 ), spawn "/home/cderose/bin/volume_change.sh mute")
  , ((noModMask, xK_F4 ), spawn "/home/cderose/bin/system76_kbd_backlight_toggle.sh" )
  , ((noModMask, xK_F5 ), spawn "/home/cderose/bin/volume_change.sh down")
  , ((noModMask, xK_F6 ), spawn "/home/cderose/bin/volume_change.sh up")
  , ((noModMask, xK_F8 ), spawn "/usr/bin/xbacklight -dec 10") -- Bright-
  , ((noModMask, xK_F9 ), spawn "/usr/bin/xbacklight -inc 10") -- Bright+
  , ((noModMask, xK_F10 ), spawn "/home/cderose/bin/screenshot.sh")

  -- Layouts
  -- %! Rotate through the available layout algorithms
  , ((modMask,               xK_space ), sendMessage NextLayout) 
  -- %!  Reset the layouts on the current workspace to default
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  -- Focus
  , ((modMask, xK_bracketright ), windows XMonad.StackSet.focusDown)
  , ((modMask, xK_bracketleft  ), windows XMonad.StackSet.focusUp)
 
  -- Move
  , ((modMask .|. shiftMask, xK_bracketright ), windows XMonad.StackSet.swapDown)
  , ((modMask .|. shiftMask, xK_bracketleft  ), windows XMonad.StackSet.swapUp)

  -- Resize
  , ((modMask .|. shiftMask, xK_comma ), sendMessage Shrink)
  , ((modMask .|. shiftMask, xK_period ), sendMessage Expand)

  -- Workspaces
  , ((modMask, xK_p ),    prevWS)
  , ((modMask, xK_n  ),   nextWS)
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(XMonad.StackSet.greedyView, 0), (XMonad.StackSet.shift, shiftMask)]]

-- ManageHook -----------------------------------------------------------------
myManageHook = composeAll
    [ className =? "Steam"    --> doFloat
    , className =? "rdesktop" --> doFloat
    , className =? "Xmessage" --> doCenterFloat
    , isDialog                --> doCenterFloat
    , title =? "Emacs Everywhere"  --> doCenterFloat
    ]

-- LayoutHook -----------------------------------------------------------------
myLayoutHook = gaps [(U,5), (R,5), (L,5), (D,5)] $ minimize . boringWindows 
  $ avoidStruts 
  $ spacing 5
  $ smartBorders -- This is needed in order to play fullscreen video without borders
  $ (tiled ||| Mirror tiled ||| Full)
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio
      -- The default number of windows in the master pane
      nmaster = 1
      -- Default proportion of screen occupied by master pane
      ratio   = 1/2
      -- Percent of screen to increment by when resizing panes
      delta   = 3/100

-- xmobar ---------------------------------------------------------------------
mySB = statusBarProp "xmobar" (pure xmobarPP)
myPP = xmobarPP { 
  ppCurrent = xmcWhiteOnBlue . (xmobarBorder"Full" (blue myColor) 0) . wrap " <fn=1>" "</fn>"
  , ppSep             = ""
  , ppTitleSanitize   = xmobarStrip
  , ppHidden          = xmcBlue . wrap " <fn=1>" "</fn>"
  , ppHiddenNoWindows = xmcBase1 . wrap " <fn=1>" "</fn>"
  , ppUrgent          = xmcWhiteOnRed . (xmobarBorder"Full" (red myColor) 0) . wrap " <fn=1>" "</fn>"
  , ppWsSep           = ""
  , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
  -- NOTE: If we dont like the window display, here's where that's controlled
  , ppExtras          = [XMonad.Util.Loggers.logTitles formatFocused formatUnfocused]
  , ppLayout  = xmcWhiteOnOrange . (xmobarBorder"Full" (orange myColor) 0) . (wrap "<action=xdotool key Super+space> " " </action>") .
    ( \x -> case x of
    -- Vertical Split:
    "Minimize Spacing Tall"        -> "\xfb87" -- "<icon="++myBitmapsDir++"/tall.xbm/>"
    -- Horizontal Split:
    "Minimize Spacing Mirror Tall" -> "\xfb86" -- "<icon="++myBitmapsDir++"/mtall.xbm/>"
    "Minimize Spacing Full"        -> "\xf630" -- "<icon="++myBitmapsDir++"/full.xbm/>"
    )
  }
  where
  formatFocused   = (xmobarBorder "Full" (blue myColor) 0) . wrap (xmcWhiteOnBlue  "\xf6dc ") (xmcWhiteOnBlue  "  ") . xmcWhiteOnBlue  . ppWindow
  formatUnfocused = wrap (xmcBase01 "\xf6dc ") (xmcBase01 " ") . xmcBase01 . ppWindow
  half_space = "\x0020" -- TODO: U+0020 is a regular space. We may want U+2009 in some fonts

  -- | Windows should have *some* title, which should not not exceed a
  -- sane length.
  ppWindow :: String -> String
  ppWindow = xmobarRaw . (\w -> if Prelude.null w then "untitled" else w)  . printf "%-20s" . shorten 20

  xmcBlue, xmcBase01, xmcBase02, xmcBase1, xmcBase3, xmcMagenta, xmcRed, xmcWhite, xmcYellow, xmcWhiteOnBlue, xmcWhiteOnRed, xmcWhiteOnOrange, xmcWhiteOnBase01 :: String -> String
  xmcMagenta     = xmobarColor (magenta myColor) ""
  xmcBlue        = xmobarColor (blue myColor) ""
  xmcWhite       = xmobarColor (white myColor) ""
  xmcYellow      = xmobarColor (yellow myColor) ""
  xmcRed         = xmobarColor (red myColor) ""
  xmcBase01      = xmobarColor (base01 myColor) ""
  xmcBase02      = xmobarColor (base02 myColor) ""
  xmcBase1       = xmobarColor (base1 myColor) ""
  xmcBase3       = xmobarColor (base3 myColor) ""
  xmcWhiteOnBlue = xmobarColor (white myColor) (blue myColor)
  xmcWhiteOnRed  = xmobarColor (white myColor) (red myColor)
  xmcWhiteOnOrange  = xmobarColor (white myColor) (orange myColor)
  xmcWhiteOnBase01 = xmobarColor (white myColor) (base01 myColor)

-- Startup Hook ---------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  -- spawn "bash -c 'killall stalonetray; sleep 1; stalonetray &'"
  spawn "~/.config/xmobar/systray.sh &"
  -- Seems like this stops working for random reasons on occasion, trying it
  -- out here, instead of the xsession
  spawn "bash -c 'killall flashfocus; /usr/bin/flashfocus &'"
  -- Sometimes xcape stops working for no reason as well. 
  spawn "bash -c 'killall xcape; /usr/bin/xcape -e Control_L=Escape'"

-- Main -----------------------------------------------------------------------
main :: IO ()
main = do
  xmonad 
    . XMonad.Hooks.EwmhDesktops.ewmhFullscreen 
    . ewmh 
    . withEasySB (statusBarProp "~/.config/xmobar/xmobar-start.sh" (clickablePP myPP)) defToggleStrutsKey
    . withUrgencyHook dzenUrgencyHook { 
      args = ["-bg", (red myColor), "-fg", (base3 myColor), "-xs", "1"] 
    } $ desktopConfig
    { XMonad.terminal           = myTerminal
    , XMonad.modMask            = myModMask
    , XMonad.keys               = myKeys
    , XMonad.focusFollowsMouse  = myFocusFollowsMouse
    , XMonad.borderWidth        = myBorderWidth
    , XMonad.focusedBorderColor = myFocusedBorderColor
    , XMonad.normalBorderColor  = myUnFocusedBorderColor
    , XMonad.manageHook         = manageDocks <+> myManageHook <+> manageHook desktopConfig
    , XMonad.layoutHook         = myLayoutHook
    , startupHook               = myStartupHook
    }

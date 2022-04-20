import Data.Map
import Data.Maybe (fromJust)

import System.IO
import System.Exit
import System.Environment

import XMonad
import XMonad.StackSet
import XMonad.Config.Desktop
import XMonad.Prompt.ConfirmPrompt
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed (renamed, Rename(Replace, CutWordsLeft))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks(avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.CopyWindow
import XMonad.Actions.PerWindowKeys


-- Colors ---------------------------------------------------------------------
data ColorSchemes = ColorSchemes{black ,white ,gray ,yellow ,orange ,red ,purple ,blue ,cyan ,green :: String}

mySolarized :: ColorSchemes
mySolarized = ColorSchemes {
  black   = "#073642",
  white   = "#fdf6e3",
  gray    = "#93a1a1",
  yellow  = "#b58900",
  orange  = "#cb4b16",
  red     = "#dc322f",
  purple  = "#6c71c4",
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
myFocusedBorderColor   = black myColor :: String
myUnFocusedBorderColor = white myColor :: String
myTerminal             = "alacritty"  :: String
myFilemanager          = "pcmanfm"    :: String
myBitmapsDir           = home++"/.xmonad/icons"

-- Keys -----------------------------------------------------------------------
dmenuTransArgs = ["-l", "50", "-nb", "#414F59", "-sb", "#636D7B", "-sf",
  "#CEE7EF", "-fn", "-o", "0.85"]

dmenuSwitchMenuArgs = dmenuTransArgs ++ ["-p", "Switch to Window:"]

toggleFloat w = windows (\s -> if Data.Map.member w (XMonad.StackSet.floating s)
  then XMonad.StackSet.sink w s
  else (XMonad.StackSet.float w (XMonad.StackSet.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

myKeys conf@(XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $
  -- xmonad commands:
  [ ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
  , ((modMask .|. shiftMask, xK_c ), kill)
  , ((modMask,               xK_a ), windows copyToAll)
  , ((modMask .|. shiftMask, xK_a ), killAllOtherCopies)
  , ((modMask,               xK_m ), withFocused minimizeWindow)
  , ((modMask,               xK_o ), withFocused toggleFloat)
  , ((modMask .|. shiftMask, xK_m ), withLastMinimized maximizeWindowAndFocus)
  , ((modMask,               xK_g ), gotoMenu)
  , ((modMask,               xK_b ), bringMenu)

  -- Launching Programs
  , ((modMask, xK_Return ), spawn $ terminal conf)
  , ((modMask, xK_f      ), spawn "firefox -P default-release")
  , ((modMask, xK_i      ), spawn "firefox -P Fap")
  , ((modMask, xK_c      ), spawn "/usr/bin/google-chrome-stable")
  , ((modMask, xK_r      ), spawn "/home/cderose/bin/dmenu_run_history")
  , ((modMask, xK_Escape ), spawn "xscreensaver-command -lock")

  -- Switchto/Open Telegram
  , ((modMask, xK_t      ), runOrRaiseAndDo "/usr/bin/telegram-desktop" 
      (className =? "TelegramDesktop") (maximizeWindowAndFocus))

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

  -- Control-Right-Bracket to Control Tab:
  , ((controlMask, xK_bracketright ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome" <||> 
      className =? "TelegramDesktop", 
      XMonad.Util.Paste.sendKey controlMask xK_Tab), 
      (pure True, XMonad.Util.Paste.sendKey controlMask xK_bracketright) ])

  -- Control-Left-Bracket to Control Shift-Tab:
  , ((controlMask, xK_bracketleft ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome" <||> 
      className =? "TelegramDesktop", 
      XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Tab), 
      (pure True, XMonad.Util.Paste.sendKey controlMask xK_bracketleft) ])

  -- Control-Shift-Right-Bracket to Control-Shift-Pagedown:
  , ((controlMask .|. shiftMask, xK_bracketright ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome", 
     XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Page_Down), 
      (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_bracketright) ])

  -- Control-Shift-Left-Bracket to Control-Shift-Pageup:
  , ((controlMask .|. shiftMask, xK_bracketleft ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome", 
     XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_Page_Up), 
      (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_bracketleft) ])

  -- Ctrl-Shift-l in browsers, to F6:
	-- (This toggles location-bar/body focus)
  , ((controlMask .|. shiftMask, xK_l ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome", 
      XMonad.Util.Paste.sendKey noModMask xK_F6), 
      (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_l) ])

  -- Ctrl-d in browsers, to Ctrl-F4:
	-- (This closes the browser tab)
  , ((controlMask , xK_d ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome", 
      XMonad.Util.Paste.sendKey controlMask xK_F4), 
      (pure True, XMonad.Util.Paste.sendKey controlMask xK_d) ])

  -- Ctrl-shift-d in browsers, to Ctrl-shift-t:
	-- (This undo's the close-tab)
  , ((controlMask .|. shiftMask, xK_d ), bindFirst [ (
      className =? "firefox" <||> className =? "Google-chrome", 
      XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_t), 
      (pure True, XMonad.Util.Paste.sendKey (controlMask .|. shiftMask) xK_d) ])

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
  , ((modMask, xK_Right), nextWS)
  , ((modMask, xK_Left),  prevWS)
  , ((modMask, xK_p ),    prevWS)
  , ((modMask, xK_n  ),   nextWS)
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(XMonad.StackSet.greedyView, 0), (XMonad.StackSet.shift, shiftMask)]]

--managehook
myManageHook = composeAll
    [ className =? "Steam"    --> doFloat
    , className =? "rdesktop" --> doFloat
    ]

-- Layouts --------------------------------------------------------------------
mySpacing = spacingRaw True             -- Only for >1 window
  -- TODO: This line seems to be completely ignored:
  (Border 0 0 0 0) -- Size of screen edge gaps
  True             -- Enable screen edge gaps
  (Border 10 10 10 10) -- Size of window gaps
  True             -- Enable window gaps


-- LayoutHook ----------------------------------------------------------------
myLayoutHook = minimize . boringWindows $ avoidStruts $ mySpacing $ smartBorders $ (layoutHook desktopConfig)

-- xmobar --------------------------------------------------------------------
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]        
myWorkspaces = clickable . (Prelude.map xmobarEscape) $ ["1","2","3","4","5", "6", "7", "8", "9"]
  where                                                                       
    clickable l = [ "<action=xdotool key Super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
      (i,ws) <- zip [1..9] l,                                        
      let n = i ]

-- Main ----------------------------------------------------------------------
main :: IO ()
main = do
  -- This is the only way I could get stalone's stack order on top of xmobar
  spawn "bash -c 'killall stalonetray; sleep 1; stalonetray &'"
  xmproc <- spawnPipe ("xmobar -x 0 ~/.xmonad/xmobar.config")
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "#dc322f", "-xs", "1"] } $ desktopConfig
    { XMonad.terminal = myTerminal
    , XMonad.modMask = myModMask
    , XMonad.keys = myKeys
    , XMonad.focusFollowsMouse = myFocusFollowsMouse
    , XMonad.borderWidth = myBorderWidth
    , XMonad.focusedBorderColor = myFocusedBorderColor
    , XMonad.normalBorderColor = myUnFocusedBorderColor
    , XMonad.manageHook = manageDocks <+> myManageHook 
                        <+> manageHook desktopConfig
    , XMonad.layoutHook = myLayoutHook
    , XMonad.workspaces = myWorkspaces
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor (blue myColor) "" . wrap "[" "]"
      , ppHiddenNoWindows = xmobarColor (gray myColor) ""
      , ppTitle   = xmobarColor (blue myColor)  "" . shorten 70
      , ppVisible = wrap "(" ")"
      , ppUrgent  = xmobarColor (red myColor) (yellow myColor)
      -- TODO: The action isnt working here...
      , ppLayout  = (wrap "<action=xdotool key Super+space>" "</action>") .
        ( \x -> case x of
        "Minimize Spacing Tall"        -> "<icon="++myBitmapsDir++"/tall.xbm/>"
        "Minimize Spacing Mirror Tall" -> "<icon="++myBitmapsDir++"/mtall.xbm/>"
        "Minimize Spacing Full"        -> "<icon="++myBitmapsDir++"/full.xbm/>"
        )
      }
    }

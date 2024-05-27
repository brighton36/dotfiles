{-# LANGUAGE MultiWayIf #-}

import Data.Map
import System.Exit
import Text.Printf
import Text.Read
import Graphics.X11.ExtraTypes

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
import XMonad.Hooks.InsertPosition
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
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Prelude
import XMonad.StackSet
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.WorkspaceCompare

import MyTheme

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
  else (XMonad.StackSet.float w (XMonad.StackSet.RationalRect (1/10) (1/8) (3/4) (4/5)) s)) --x y w h

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
  -- I just don't think we need/use this..
  -- , ((modMask,               xK_b ), bringMenu)

  -- Launching Programs
  , ((modMask, xK_Return ), spawn $ terminal conf)
  , ((modMask, xK_f      ), spawn "firefox -P default-release")
  , ((modMask, xK_i      ), spawn "firefox -P Fap")
  -- This was deprecated for 'click' , by way of keynav
  -- , ((modMask, xK_c      ), spawn "/usr/bin/google-chrome-stable")
  , ((modMask, xK_r      ), spawn "~/bin/dmenu_run_history")
  , ((modMask, xK_t      ), spawn "~/.config/xmobar/systray.sh")
  , ((modMask, xK_z      ), spawn "/usr/bin/rofimoji -a type --typer xdotool --keybinding-copy \"Control+y\"")
  , ((modMask, xK_Escape ), spawn "/usr/bin/systemctl suspend") -- Alternatively: "xscreensaver-command -lock"

  -- Emacs bindings:
  , ((modMask, xK_minus      ), spawn "~/.guix-profile/bin/emacsclient --eval \"(emacs-everywhere)\"")
  , ((modMask, xK_e), spawn "~/.guix-profile/bin/emacsclient -c -a emacs")
  , ((modMask, xK_b), spawn "~/.guix-profile/bin/emacsclient -n -e '(+org-capture/open-frame)'")

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
  -- F1:
  , ((noModMask, xF86XK_AudioPrev ), spawn "pcmanfm") -- FileManager
  -- F2:
  , ((noModMask, xF86XK_AudioNext ), spawn "/home/cderose/bin/cycle_display.rb") 
  -- F3:
  , ((noModMask, xF86XK_AudioMute ), spawn "/home/cderose/bin/volume_change.sh mute")
  -- F4:
  , ((noModMask, xF86XK_AudioPlay ), spawn "/home/cderose/bin/system76_kbd_backlight_toggle.sh" )
  -- F5:
  , ((noModMask, xF86XK_AudioLowerVolume ), spawn "/home/cderose/bin/volume_change.sh down")
  -- F6:
  , ((noModMask, xF86XK_AudioRaiseVolume ), spawn "/home/cderose/bin/volume_change.sh up")
  -- F7: The built-in keyboard sends 'TouchpadToggle'
  , ((noModMask, xF86XK_TouchpadToggle ), spawn "dunstify -u normal 'TODO: Bind xF86XK_TouchpadToggle'" )
  -- F7: The moonlander sends 'Stop'
  , ((noModMask, xF86XK_AudioStop ), spawn "dunstify -u normal 'TODO: Bind xF86XK_AudioStop'" )
  -- F8: This is for the moonlander, the BIOS handles this on the builtin keyboard
  , ((noModMask, xF86XK_MonBrightnessDown ), spawn "/usr/bin/xbacklight -dec 10") -- Bright-
  -- F9: This is for the moonlander, the BIOS handles this on the builtin keyboard
  , ((noModMask, xF86XK_MonBrightnessUp ), spawn "/usr/bin/xbacklight -inc 10") -- Bright+
  -- F10:
  , ((noModMask, xK_Pause ), spawn "/home/cderose/bin/screenshot.sh")
  -- F11: 
  , ((noModMask, xK_Scroll_Lock ), spawn "dunstify -u normal 'TODO: Bind ScrollLock'" )
  -- F12: 
  , ((noModMask, xK_Insert ), spawn "dunstify -u normal 'TODO: Bind Insert'" )

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
  -- TODO: I'm really not crazy about these... this was added after we realized
  -- that the multi-head keys stepped on these
  , ((modMask .|. shiftMask, xK_h ), sendMessage Shrink)
  , ((modMask .|. shiftMask, xK_l), sendMessage Expand)

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
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_comma, xK_apostrophe, xK_period] [0..]
      , (f, m) <- [(XMonad.StackSet.view, 0), (XMonad.StackSet.shift, shiftMask)]]

-- ManageHook -----------------------------------------------------------------
myManageHook = composeAll
    [ className =? "Steam"         --> doFloat
    , className =? "rdesktop"      --> doFloat
    , className =? "Vncviewer"     --> doFloat
    , className =? "Xmessage"      --> doCenterFloat
    , isDialog                     --> doCenterFloat
    , title =? "Emacs Everywhere"  --> doFloat
    , title =? "doom-capture"      --> doRectFloat(RationalRect 0.55 0.45 0.425 0.5) --x y w h
    , title =? "Emacs top"         --> doRectFloat(RationalRect 0.55 0.05 0.425 0.75) --x y w h
    , title =? "zoom"              --> doFloat -- This will match the notifications only, not the room window
    ]

-- LayoutHook -----------------------------------------------------------------
myLayoutHook = gaps [(U,5), (R,5), (L,5), (D,5)] $ minimize . boringWindows
  $ avoidStruts
  $ spacing 5
  $ smartBorders -- This is needed in order to play fullscreen video without borders
  (
  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
    ThreeColMid 1 (3/100) (3/7)

  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
   ||| ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid)

-- XMonad.Util.Loggers ---------------------------------------------------------
-- These functions were pulled from the XMonad.Util.Loggers and and adjusted
-- (really only the change of word to 'getNameWMClass') to log the window class,
-- instead of the window title.
-- TODO: this entire section appears to be implemented now, in the latest xmonad
--       and at some point, we should be able to call XMonad.Util.Loggers#logClassnames
--       directly, instead of the Main.logClasses that we implemented here

-- | Internal function to get a wrapped title string from a window
fetchWindowClass :: Window -> X String
fetchWindowClass = fmap show . getNameWMClass

-- | A helper function to create screen-specific loggers.
withScreen :: (WindowScreen -> Logger) -> ScreenId -> Logger
withScreen f n = do
  ss <- withWindowSet $ return . XMonad.StackSet.screens
  case find ((== n) . XMonad.StackSet.screen) ss of
    Just s  -> f s
    Nothing -> pure Nothing

-- | Internal helper function for 'logWindowInfoOnScreen'.
logWindowInfoOnScreenWorker
  :: (Window -> X String)
  -> WindowScreen
  -> (Window -> String -> String)
  -> Logger
logWindowInfoOnScreenWorker getWindowInfo screen logger = do
  let wins = maybe [] XMonad.StackSet.integrate . XMonad.StackSet.stack . XMonad.StackSet.workspace $ screen
  winNames <- traverse getWindowInfo wins
  pure . Just . unwords $ zipWith logger wins winNames

-- | Internal function to get the specified window information for all windows on
-- the visible workspace of the given screen and format them according to the
-- given functions.
logWindowInfoOnScreen
  :: (Window -> X String)
  -> ScreenId
  -> (String -> String)
  -> (String -> String)
  -> (String -> String)
  -> Logger
logWindowInfoOnScreen getWindowInfo sid formatFoc formatUnfoc formatUrg =
  (`withScreen` sid) $ \screen -> do
    let focWin = fmap XMonad.StackSet.focus . XMonad.StackSet.stack . XMonad.StackSet.workspace $ screen
    urgWins <- readUrgents
    logWindowInfoOnScreenWorker getWindowInfo screen $ \win name ->
      if | Just win == focWin -> formatFoc   name
         | win `elem` urgWins -> formatUrg   name
         | otherwise          -> formatUnfoc name

-- | Internal. Like 'logWindowInfoOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logWindowInfoFocusedScreen
  :: (Window -> X String)
  -> (String -> String)
  -> (String -> String)
  -> (String -> String)
  -> Logger
logWindowInfoFocusedScreen getWindowInfo formatFoc formatUnfoc formatUrg = do
  sid <- gets $ XMonad.StackSet.screen . XMonad.StackSet.current . windowset
  logWindowInfoOnScreen getWindowInfo sid formatFoc formatUnfoc formatUrg

-- | Like 'logClassesOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logClasses :: (String -> String) -> (String -> String) -> Logger
logClasses formatFoc formatUnfoc =
  logWindowInfoFocusedScreen fetchWindowClass formatFoc formatUnfoc formatUnfoc

-- xmobar ----------------------------------------------------------------------

compareNumbers :: String -> String -> Ordering
compareNumbers a b =
  case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int) of
    -- if they're both valid numbers then compare them
    (Just x, Just y) -> compare y x
    -- push numbers to the front of strings
    (Just _, Nothing) -> LT
    (Nothing, Just _) -> GT
    -- strings get normal string comparison
    (Nothing, Nothing) -> compare a b

mySB = statusBarProp "xmobar" (pure xmobarPP)
myPP = xmobarPP { 
  ppCurrent = xmcBase3 . wrap " <fn=1>" "</fn>"
  , ppSep             = ""
  , ppTitleSanitize   = xmobarStrip
  , ppHidden          = xmcBlue . wrap " <fn=1>" "</fn>"
  , ppHiddenNoWindows = xmcBase1 . wrap " <fn=1>" "</fn>"
  , ppUrgent          = xmcWhiteOnRed . (xmobarBorder"Full" (red myColor) 0) . wrap " <fn=1>" "</fn>"
  , ppWsSep           = ""
  -- ws is the workspace, l is the layout , wins is the window title
  , ppOrder           = \[ws, l, _, wins] -> [wins, l, ws]
  -- This reversese the order of the workspaces:
  --, ppSort = mkWsSort $ return compareNumbers
  -- NOTE: If we dont like the window display, here's where that's controlled
  , ppExtras          = [Main.logClasses formatFocused formatUnfocused]
  , ppLayout          = xmcOrange . (wrap "<action=xdotool key Super+space> " " </action>") .
    ( \x -> case x of
    -- Vertical Split:
    "Minimize Spacing Grid"                 -> (spacingGrid myIcons)
    "Minimize Spacing ThreeCol"             -> (spacingThreeCol myIcons)
    "Minimize Spacing ResizableTall"        -> (spacingVertical myIcons)
    "Minimize Spacing Mirror ResizableTall" -> (spacingHorizontal myIcons)
    "Minimize Spacing Full"                 -> (spacingFull myIcons)
    )
  }
  where
  formatFocused   = wrap (xmcBase3  " ") (xmcBase3  "\xf6dc ") . xmcBase3  . ppWindow
  formatUnfocused = wrap (xmcBase1 " ") (xmcBase1 "\xf6dc ") . xmcBase1 . ppWindow
  half_space = "\x0020" -- TODO: U+0020 is a regular space. We may want U+2009 in some fonts

  -- | Windows should have *some* title, which should not not exceed a
  -- sane length.
  -- ppWindow :: String -> String
  ppWindow = xmobarRaw . (\w -> if Prelude.null w then "untitled" else w)  . printf "%-12s" . shorten 12

  xmcBlue, xmcBase01, xmcBase02, xmcBase1, xmcBase3, xmcMagenta, xmcRed, xmcWhite, xmcYellow, xmcWhiteOnBlue, xmcWhiteOnRed, xmcOrangeOnWhite, xmcWhiteOnBase01 :: String -> String
  xmcMagenta     = xmobarColor (magenta myColor) ""
  xmcBlue        = xmobarColor (blue myColor) ""
  xmcWhite       = xmobarColor (white myColor) ""
  xmcYellow      = xmobarColor (yellow myColor) ""
  xmcOrange      = xmobarColor (orange myColor) ""
  xmcRed         = xmobarColor (red myColor) ""
  xmcBase01      = xmobarColor (base01 myColor) ""
  xmcBase02      = xmobarColor (base02 myColor) ""
  xmcBase1       = xmobarColor (base1 myColor) ""
  xmcBase3       = xmobarColor (base3 myColor) ""
  xmcWhiteOnBlue = xmobarColor (white myColor) (blue myColor)
  xmcWhiteOnRed  = xmobarColor (white myColor) (red myColor)
  xmcOrangeOnWhite  = xmobarColor (orange myColor) (white myColor)
  xmcWhiteOnBase01 = xmobarColor (white myColor) (base01 myColor)

-- Startup Hook ---------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  -- spawn "bash -c 'killall stalonetray; sleep 1; stalonetray &'"
  -- spawn "~/.config/xmobar/systray.sh &"
  -- Seems like this stops working for random reasons on occasion, trying it
  -- out here, instead of the xsession
  spawn "bash -c 'killall flashfocus; /usr/bin/flashfocus &'"
  -- Sometimes xcape stops working for no reason as well. 
  -- spawn "bash -c 'killall xcape; /usr/bin/xcape -e Control_L=Escape'"

-- Main -----------------------------------------------------------------------
main :: IO ()
main = do
  xmonad
    . setupInsertPosition Below Newer
    . XMonad.Hooks.EwmhDesktops.ewmhFullscreen 
    . ewmh 
    . withSB (statusBarProp "~/.config/xmobar/xmobar-start.sh" (clickablePP myPP))
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

import Data.Map
import Data.Maybe (fromJust)

import System.IO
import System.Exit

import XMonad
import XMonad.StackSet
import XMonad.Config.Desktop
import XMonad.Prompt.ConfirmPrompt
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks(avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))


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
myModMask              = mod4Mask     :: KeyMask
myFocusFollowsMouse    = False        :: Bool
myBorderWidth          = 3            :: Dimension
myWindowGap            = 12            :: Integer
myColor                = mySolarized  :: ColorSchemes
myFocusedBorderColor   = blue myColor :: String
myUnFocusedBorderColor = gray myColor :: String
myTerminal             = "alacritty"  :: String
myFilemanager          = "pcmanfm"    :: String
-- myFont               
--   = "xft:Hack Nerd Font:regular:size=12:antialias=true:hinting=true"         :: String

mySpacing = spacingRaw True             -- Only for >1 window
  -- The bottom edge seems to look narrower than it is
  (Border 0 15 10 10) -- Size of screen edge gaps
  True             -- Enable screen edge gaps
  (Border 5 5 5 5) -- Size of window gaps
  True             -- Enable window gaps

-- Keys -----------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $
  -- xmonad commands:
  [ ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
  , ((modMask .|. shiftMask, xK_c ), kill)

  -- Launching Programs
  , ((modMask, xK_Return ), spawn $ terminal conf)
  , ((modMask, xK_f      ), spawn "firefox")
  , ((modMask, xK_r      ), spawn "dmenu_run")

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
  , ((modMask, xK_h ), sendMessage Shrink)
  , ((modMask, xK_l ), sendMessage Expand)
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(XMonad.StackSet.greedyView, 0), (XMonad.StackSet.shift, shiftMask)]]

--managehook
myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    , className =? "Steam"    --> doFloat
    , className =? "rdesktop" --> doFloat
    ]

-- xmobar --------------------------------------------------------------------
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]        
myWorkspaces = clickable . (Prelude.map xmobarEscape) $ ["1","2","3","4","5", "6", "7", "8", "9"]
  where                                                                       
    clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
      (i,ws) <- zip [1..5] l,                                        
      let n = i ]

-- Main ----------------------------------------------------------------------
main :: IO ()
main = do
  xmproc <- spawnPipe ("xmobar -x 0 ~/.xmonad/xmobar.config")
  xmonad $ desktopConfig
    { XMonad.terminal = myTerminal
    , XMonad.modMask = myModMask
    , XMonad.keys = myKeys
    , XMonad.focusFollowsMouse = myFocusFollowsMouse
    , XMonad.borderWidth = myBorderWidth
    , XMonad.focusedBorderColor = myFocusedBorderColor
    , XMonad.normalBorderColor = myUnFocusedBorderColor
    , XMonad.manageHook = manageDocks <+> myManageHook 
                        <+> manageHook desktopConfig
    , XMonad.layoutHook = avoidStruts $ mySpacing $ smartBorders (layoutHook desktopConfig)
    , XMonad.workspaces = myWorkspaces
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor (blue myColor) "" . wrap "[" "]"
      , ppHiddenNoWindows = xmobarColor (gray myColor) ""
      , ppTitle   = xmobarColor (green myColor)  "" . shorten 40
      , ppVisible = wrap "(" ")"
      , ppUrgent  = xmobarColor (red myColor) (yellow myColor)
      }
    }

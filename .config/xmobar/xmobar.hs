import Data.List
import Xmobar

-- Colors ---------------------------------------------------------------------
-- TODO: This should probably be stored somewhere in the .xmonad dir DRY this
--       out
data ColorSchemes = ColorSchemes{base03, base02, base01, base00, base0, base1, 
  base2, base3, black, white, yellow, orange, red, magenta, violet, blue, cyan, 
  green, darkgrey :: String}

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
  black   = "#000000", -- TODO: This differ's from xmonad's black
  white   = "#ffffff", -- TODO: This differ's from xmonad's white
  yellow  = "#b58900",
  orange  = "#cb4b16",
  red     = "#dc322f",
  magenta = "#d33682",
  violet  = "#6c71c4",
  blue    = "#268bd2",
  cyan    = "#2aa198",
  green   = "#859900",
  darkgrey = "#353535" -- TODO: This differ's from xmonad's darkgrey
}

myColor = mySolarized  :: ColorSchemes

-- Icons ----------------------------------------------------------------------
data Icons = Icons{cornerstone, system, memory, swap, mute, 
  vol_high, vol_med, vol_low, bat_low, bat_med, bat_high, charged, discharging, 
  ac_on :: String}
myIcons :: Icons
myIcons = Icons {
    cornerstone = "\61603"      -- ,,
  , system      = "\61573" -- 
  , memory      = "\57958" -- 
  , swap        = "\59142" -- 
  , mute        = "婢"     -- 婢
  , vol_high    = "\61480" -- 
  , vol_med     = "墳"     -- 墳
  , vol_low     = "\61479" -- 
  , bat_low     = "\62841" -- 
  , bat_med     = "\62845" -- 
  , bat_high    = "\62840" -- 
  , charged     = "⏼"      -- ⏼
  , discharging = "ﮤ"      -- ﮤ
  , ac_on       = "ﮣ"      -- ﮣ 
}

-- Helpers -------------------------------------------------------------------
fc :: [Char] -> [Char] -> [Char] -> [Char]
fc foreground background contents = (concat [ 
  "<fc=",foreground,",",background,">", contents, "</fc>"])

box :: [Char] -> [Char]-> [Char] -> [Char]
box foreground background contents = (concat [ 
  "<box type=full color=",background,">", 
  (fc foreground background contents), "</box>"])

action :: [Char] -> [Char] -> [Char]
action command contents = (concat [ 
   "<action=",command,">", contents, "</action>"])

-------------------------------------------------------------------------------
config :: Config
config = Xmobar.defaultConfig {
    font             =  "xft:Ubuntu Nerd Font:size=15:antialias=true"
  , additionalFonts  =  [
    "xft:Ubuntu Nerd Font Mono:size=15:antialias=true",
    "xft:Source Code Pro Bold:size=15:antialias=true"
    ]
  , bgColor          =  (white myColor)
  , fgColor          =  (base00 myColor)
  , alpha            =  255
  , position         =  Top --Static { xpos = 0 , ypos = 0, width = 1920, height = 24}
  , border           =  NoBorder
  , lowerOnStart     =  True
  , hideOnStart      =  False
  , allDesktops      =  True
  , overrideRedirect =  False
  , pickBroadest     =  False
  , persistent       =  False
  , iconRoot         = "/home/cderose/.config/xmobar/icons/"
  , sepChar          =  "%"
  , alignSep         =  "}{"
  , template = concat [
    (box (white myColor) (base01 myColor) (" "++(cornerstone myIcons)++" ")),
    "%UnsafeXMonadLog% }{ ",
    (box (white myColor) (blue myColor) "\xf6dc %cpu% %memory% %swap% "),
    "%battery%\xf6dc%default:Master%",
    -- NOTE: the background color on the telegram icon appears to come from qt6ct
    (box (white myColor) (white myColor) "%traypad%"),
    (box (white myColor) (violet myColor) " <fn=2>%date%</fn> ")
  ]

  -- plugins
  , commands = [
    Run $ UnsafeXMonadLog
    , Run $ Cpu 
      [ "--template"
      , (action "alacritty --command btop" (system myIcons)++ " <total><icon=percent.xbm/>")
      , "-L",      "10"
      , "-H",      "70"
      -- ,"--normal", (base00 myColor)
      ,"--high",   (white myColor)++","++(red myColor) 
      ] 5
    -- , Run $ MultiCpu 
    --   [ "--template", "<ipat>"
    --   , "--"
    --   , "--load-icon-pattern", "<icon=hbar_%%.xbm/>"
    --   ] 2
    , Run $ Memory 
      [ "--template"
      , (action "alacritty --command btop" (memory myIcons)++ " <usedratio><icon=percent.xbm/>")
      , "--Low"      , "20"             -- units: %
      , "--High"     , "80"             -- units: %
      --, "--low"      , (base00 myColor)
      --, "--normal"   , (base00 myColor)
      , "--high"     , (white myColor)++","++(red myColor)
      ] 5
    , Run $ Swap 
      [ "--template"
      , (action "alacritty --command btop" (swap myIcons)++ " <usedratio><icon=percent.xbm/>")
      ] 5
    -- , Run $ Brightness 
    --   [ "--template", " <percent>"
    --   , "--"
    --   , "-D", "/sys/class/backlight/intel_backlight"
    --   ] 5
    , Run $ Volume "default" "Master" 
      [ "--template", (action "/usr/bin/pavucontrol" "<status>")
      , "--"
      , "-O", ""                  -- Status On
      , "-o", (box (white myColor) (red myColor) (" "++(mute myIcons)++"  ")) -- Status Off
      , "-C", (base00 myColor)    -- Status On Color
      , "-c", (red myColor)       -- Status Off Color
      , "-H", "130"               -- High threshold
      , "-L", "80"                -- Low threshold
      , "-h", (box (darkgrey myColor) (white myColor) ((vol_high myIcons)++"  "))
      , "-m", (box (darkgrey myColor) (white myColor) ((vol_med myIcons)++"  "))
      , "-l", (box (darkgrey myColor) (white myColor) ((vol_low myIcons)++"  "))
      ] 5
    , Run $ BatteryP ["BAT0"]
      [ "--template"
      , (action "alacritty --command /usr/bin/sudo /usr/bin/powertop" "<acstatus>")
      , "--"
      , "-L",        "15"              -- Low threshold (percent)
      , "-H",        "85"              -- High threshold (percent)
      , "-h",        (base00 myColor)  -- High color
      , "-m",        (base01 myColor)  -- Medium color
      , "-l",        (red myColor)     -- Low color
      , "--lows",    (box (white myColor) (red myColor) (" "++(bat_low myIcons)++"<left><icon=percent.xbm/>  "))
      , "--mediums", (box (white myColor) (blue myColor) ((bat_med myIcons)++"<left><icon=percent.xbm/>  "))
      , "--highs",   (box (white myColor) (cyan myColor) (" "++(bat_high myIcons++"<left><icon=percent.xbm/>  ")))
      , "-i",        (box (white myColor) (green myColor) 
                       (" "++(charged myIcons)++" <left><icon=percent.xbm/>  "))
      , "-O",        (box (white myColor) (green myColor) 
                       (" "++(ac_on myIcons)++" <left><icon=percent.xbm/>  "))
      , "-o",        "" -- This is taken care of in "--mediums/lows"
      ] 5
    , Run $ Com "/home/cderose/.config/xmobar/systraypad.sh" [] "traypad" 10
    , Run $ Date "%a %b %_d %H:%M" "date" 10
  ]
}

main :: IO ()
main = xmobar Main.config

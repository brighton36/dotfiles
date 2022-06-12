import Data.List

import Xmobar

-- Colors ---------------------------------------------------------------------
-- TODO: This should probably be stored somewhere in the .xmonad dir DRY this
--       out
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
  black   = "#000000", -- TODO: This differ's from xmonad's black
  white   = "#ffffff", -- TODO: This differ's from xmonad's white
  yellow  = "#b58900",
  orange  = "#cb4b16",
  red     = "#dc322f",
  magenta = "#d33682",
  violet  = "#6c71c4",
  blue    = "#268bd2",
  cyan    = "#2aa198",
  green   = "#859900"
}

myColor = mySolarized  :: ColorSchemes

-- Icons ----------------------------------------------------------------------
data Icons = Icons{cornerstone, command_sep, system, memory, swap, mute, 
  vol_high, vol_med, vol_low, bat_low, bat_med, bat_high, charged, discharging, 
  ac_on :: String}
myIcons :: Icons
myIcons = Icons {
    cornerstone = "\62211" -- 
  , command_sep = "\63192" --  
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

-------------------------------------------------------------------------------
config :: Config
config = Xmobar.defaultConfig {
    font             =  "xft:Ubuntu Nerd Font:size=15:antialias=true"
  , additionalFonts  =  ["xft:Ubuntu Nerd Font Mono:size=15:antialias=true"]
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
    "<box type=full color=",(base01 myColor),">", 
      "<fc=",(white myColor),",",(base01 myColor),"> ",(cornerstone myIcons)," </fc>",
    "</box>",
    "%UnsafeStdinReader% }{ ",
    (command_sep myIcons),
    intercalate ((command_sep myIcons)) [
      "%cpu% %memory% %swap%",
      "%battery%",
      "%default:Master%"
    ],
    " %traypad%",
    "<box type=full color=",(violet myColor),">",
      "<fc=",(white myColor),",",(violet myColor),"> %date% </fc>",
    "</box>"
    ]

  -- plugins
  , commands = [
    Run $ UnsafeStdinReader
    , Run $ Cpu 
      [ "--template", concat [
        "<action=alacritty --command btop>", (system myIcons), " <total></action>"]
      , "-L",      "10"
      , "-H",      "70"
      ,"--normal", (base00 myColor)
      ,"--high",   (red myColor) 
      ] 5
    -- , Run $ MultiCpu 
    --   [ "--template", "<ipat>"
    --   , "--"
    --   , "--load-icon-pattern", "<icon=hbar_%%.xbm/>"
    --   ] 2
    , Run $ Memory 
      [ "--template" , concat [ 
        "<action=alacritty --command btop>", (memory myIcons), " <usedratio></action>"]
      , "--Low"      , "20"             -- units: %
      , "--High"     , "60"             -- units: %
      , "--low"      , (base00 myColor)
      , "--normal"   , (base00 myColor)
      , "--high"     , (red myColor)
      ] 5
    , Run $ Swap 
      ["--template", concat [
        "<action=alacritty --command btop>", (swap myIcons), " <usedratio></action>" ] 
      ] 5
    -- , Run $ Brightness 
    --   [ "--template", " <percent>"
    --   , "--"
    --   , "-D", "/sys/class/backlight/intel_backlight"
    --   ] 5
    , Run $ Volume "default"  "Master" 
      [ "--template", "<action=/usr/bin/pavucontrol><status></action>"
      , "--"
      , "-O", ""                  -- Status On
      , "-o", " "++(mute myIcons) -- Status Off
      , "-C", (base00 myColor)    -- Status On Color
      , "-c", (red myColor)       -- Status Off Color
      , "-H", "130"               -- High threshold
      , "-L", "80"                -- Low threshold
      , "-h", (vol_high myIcons)
      , "-m", (vol_med myIcons)
      , "-l", (vol_low myIcons)
      ] 5
    , Run $ BatteryP ["BAT0"]
      [ "--template"
      , "<action=alacritty --command /usr/bin/sudo /usr/bin/powertop><acstatus> <left></action>"
      , "--"
      , "-L",        "15"              -- Low threshold (percent)
      , "-H",        "66"              -- High threshold (percent)
      , "-h",        (base00 myColor)  -- High color
      , "-m",        (base00 myColor)  -- Medium color
      , "-l",        (red myColor)     -- Low color
      , "--lows",    concat ["<fc=",(red myColor),">",(bat_low myIcons),"</fc>"]
      , "--mediums", (bat_med myIcons)
      , "--highs",   (bat_high myIcons)
      , "-i",        concat ["<fc=",(green myColor),">",(charged myIcons),"</fc>"]
      , "-O",        concat ["<fc=",(green myColor),">",(ac_on myIcons),"</fc>"]
      , "-o",        (discharging myIcons)
      ] 5
    , Run $ Com "/home/cderose/.config/xmobar/systraypad.sh" [] "traypad" 10
    , Run $ Date "%a %b %_d %H:%M" "date" 10
	]
}

main :: IO ()
main = xmobar Main.config

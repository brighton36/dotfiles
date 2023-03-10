import Xmobar

import MyTheme

-------------------------------------------------------------------------------
myColor = mySolarized  :: ColorSchemes

config :: Config
config = Xmobar.defaultConfig {
    font             =  "xft:Ubuntu Nerd Font:size=14:antialias=true"
  , additionalFonts  =  [
    "xft:Ubuntu Nerd Font Mono:size=14:antialias=true",
    "xft:Source Code Pro Bold:size=14:antialias=true",
    "xft:Linux Libertine:size=24:antialias=true"
    ]
  , bgColor          =  (blue myColor)
  , fgColor          =  (base1 myColor)
  , alpha            =  120
  , position         =  TopH 36
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
    (fc2 (white myColor) " %time% "),
    (fc2 (cyan myColor) "<fn=2> %hdate% </fn>"),
    (fc2 (white myColor) " <fn=2> %date%</fn>"),
    "%battery% %default:Master%\xf6dc",
    "  %swap% %memory% %cpu% \xf6dc",
    " }{ %UnsafeXMonadLog%",
    ("  <fc="++(yellow myColor)++">"++(cornerstone myIcons)++"</fc> ")
  ]

  -- plugins
  , commands = [
    Run $ UnsafeXMonadLog
    , Run $ Cpu 
      [ "--template"
      , (action "alacritty --command btop" (system myIcons)++"<total>% ")
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
      , (action "alacritty --command btop" (memory myIcons)++"<usedratio>% ")
      , "--Low"      , "20"             -- units: %
      , "--High"     , "80"             -- units: %
      --, "--low"      , (base00 myColor)
      --, "--normal"   , (base00 myColor)
      , "--high"     , (white myColor)++","++(red myColor)
      ] 5
    , Run $ Swap 
      [ "--template"
      , (action "alacritty --command btop" (swap myIcons)++"<usedratio>% ")
      ] 5
    -- , Run $ Brightness 
    --   [ "--template", " <percent>"
    --   , "--"
    --   , "-D", "/sys/class/backlight/intel_backlight"
    --   ] 5
    , Run $ Volume "default" "Master" 
      [ "--template", (action "/usr/bin/pavucontrol" "<volume>% <status>")
      , "--"
      , "-O", ""                  -- Status On
      , "-o", (fc2 (red myColor) (" "++(mute myIcons)++"  ")) -- Status Off
      , "-C", (base00 myColor)    -- Status On Color
      , "-c", (red myColor)       -- Status Off Color
      , "-H", "130"               -- High threshold
      , "-L", "80"                -- Low threshold
      , "-h", (fc2 (orange myColor) ((vol_high myIcons)++"  "))
      , "-m", (fc2 (cyan myColor) ((vol_med myIcons)++"  "))
      , "-l", (fc2 (yellow myColor) ((vol_low myIcons)++"  "))
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
      , "--lows",    (fc2 (red myColor) (" "++(bat_low myIcons)++"<left><icon=percent.xbm/>  "))
      , "--mediums", (fc2 (blue myColor) ((bat_med myIcons)++"<left><icon=percent.xbm/>  "))
      , "--highs",   (fc2 (cyan myColor) (" "++(bat_high myIcons++"<left><icon=percent.xbm/>  ")))
      , "-i",        (fc2 (green myColor) ("  <left>% "++(charged myIcons)++" "))
      , "-O",        (fc2 (green myColor)
                       (" "++(ac_on myIcons)++" <left><icon=percent.xbm/>  "))
      , "-o",        "" -- This is taken care of in "--mediums/lows"
      ] 5
--    , Run $ Com "/home/cderose/.config/xmobar/systraypad.sh" [] "traypad" 10
    , Run $ Com "/usr/bin/hebcal" ["-T"] "hdate" 10
    , Run $ Date "%a %b %_d" "date" 10
    , Run $ Date "%H:%M" "time" 10
  ]
}

main :: IO ()
main = xmobar Main.config

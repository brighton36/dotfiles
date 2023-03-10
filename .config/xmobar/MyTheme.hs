module MyTheme(
  fc2, fc3, box, fn, action,
  ColorSchemes, mySolarized,
  base03, base02, base01, base00, base0, base1, base2, base3, 
  black, white, yellow, orange, red, magenta, violet, blue, cyan, green, darkgrey,
  Icons, myIcons,
  cornerstone, system, memory, swap, mute, vol_high, vol_med, vol_low, bat_low, 
  bat_med, bat_high, charged, discharging, ac_on
  )
  where
  import Data.Char

  -- Helpers -------------------------------------------------------------------
  fc2 :: [Char] -> [Char] -> [Char]
  fc2 foreground contents = (concat ["<fc=",foreground,">", contents, "</fc>"])

  fc3 :: [Char] -> [Char] -> [Char] -> [Char]
  fc3 foreground background contents = (concat [
    "<fc=",foreground,",",background,">", contents, "</fc>"])

  box :: [Char] -> [Char]-> [Char] -> [Char]
  box foreground background contents = (concat [ 
    "<box type=full color=",background,">", 
    (fc3 foreground background contents), "</box>"])

  fn :: Int -> [Char] -> [Char]
  fn n contents = (concat [ 
    "<fn=",[(intToDigit n)],">", contents, "</fn>"]) 

  action :: [Char] -> [Char] -> [Char]
  action command contents = (concat [ 
     "<action=",command,">", contents, "</action>"])

  -- Colors ---------------------------------------------------------------------
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
    black   = "#000000",
    white   = "#ffffff",
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

  -- Icons ----------------------------------------------------------------------
  data Icons = Icons{cornerstone, system, memory, swap, mute, 
    vol_high, vol_med, vol_low, bat_low, bat_med, bat_high, charged, discharging, 
    ac_on :: String}
  myIcons :: Icons
  myIcons = Icons {
      cornerstone = (fn 3 "בשֵׁחמַ ")  -- ,,
    , system      = "C:"  -- (fn 3 "דבעמ")   -- "\61573" -- 
    , memory      = "M:"  -- (fn 3 "ןורכיז") -- "\57958" -- 
    , swap        = "S:" -- (fn 3 "ףילִחהַלְ") -- "\59142" -- 
    , mute        = (fn 3 "קיתִשְׁהַלְ") -- 婢
    , vol_high    = (fn 3 "הַוֹבגָ")   -- "\61480" -- 
    , vol_med     = (fn 3 "ינוניב") -- "墳"     -- 墳
    , vol_low     = (fn 3 "ךוּמנָ")   -- "\61479" -- 
    , bat_low     = (fn 3 "ךוּמנָ")   -- "\62841" -- 
    , bat_med     = (fn 3 "ינוניב") -- "\62845" -- 
    , bat_high    = (fn 3 "הַוֹבגָ")   -- "\62840" -- 
    , charged     = (fn 3 "ןועט")   -- "⏼"      -- ⏼
    , discharging = (fn 3 "הקירפ")  -- "ﮤ"      -- ﮤ
    , ac_on       = (fn 3 "ענָוּממְ")  -- "ﮣ"      -- ﮣ 
  }


---------------------------------
--  "Solarized" awesome theme  --
--     By Gwenhael Le Moine    --
---------------------------------

-- Alternative icon sets and widget icons:
--  * http://awesome.naquadah.org/wiki/Nice_Icons

local lfs = require("lfs")
local xresources = require("beautiful.xresources")

local dpi = xresources.apply_dpi

function images_in_dir (path)
  ret = {}
  for file in lfs.dir(path) do
    local ext = file:match("[^.]+$")
    if ext == "jpg" or ext == "png" then
      table.insert(ret, path..'/'..file)
    end
  end
  return ret
end

--
-- {{{ Main
local theme = {}

theme.default_themes_path = "/usr/share/awesome/themes"

theme.wallpapers = images_in_dir(os.getenv("HOME") .. "/Pictures/Wallpapers")
math.randomseed(os.time())

theme.wallpaper = function (s)
    return theme.wallpapers[math.random(#theme.wallpapers)]
end

theme.colors = {}
theme.colors.base3   = "#002b36ff"
theme.colors.base2   = "#073642ff"
theme.colors.base1   = "#586e75ff"
theme.colors.base0   = "#657b83ff"
theme.colors.base00  = "#839496ff"
theme.colors.base01  = "#93a1a1ff"
theme.colors.base02  = "#eee8d5ff"
theme.colors.base03  = "#fdf6e3ff"
theme.colors.yellow  = "#b58900ff"
theme.colors.orange  = "#cb4b16ff"
theme.colors.red     = "#dc322fff"
theme.colors.magenta = "#d33682ff"
theme.colors.violet  = "#6c71c4ff"
theme.colors.blue    = "#268bd2ff"
theme.colors.cyan    = "#2aa198ff"
theme.colors.green   = "#859900ff"
-- }}}

-- {{{ Styles
theme.font      = "Ubuntu Mono Bold 9"

-- {{{ Colors
theme.fg_normal  = theme.colors.base02
theme.fg_focus   = theme.colors.base03
theme.fg_urgent  = theme.colors.base3

theme.bg_normal  = theme.colors.base3

theme.bg_focus   = theme.colors.base1
-- This is notification color in the taglist and the tasklist
theme.bg_urgent  = theme.colors.blue
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width  = dpi(1)
theme.border_normal = theme.bg_normal
theme.border_focus  = theme.bg_focus
theme.border_marked = theme.bg_urgent
theme.useless_gap = dpi(4)
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.bg_focus
-- The 00 is the alpha
theme.titlebar_bg_normal = theme.bg_normal
theme.titlebar_height  = dpi(20)
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = theme.colors.green
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_resize = "false"
theme.taglist_squares_sel   = "/home/cderose/.config/awesome/bar-sel.png"
theme.taglist_squares_unsel = "/home/cderose/.config/awesome/bar-unsel.png"
--theme.taglist_squares_sel   = theme.default_themes_path.."/zenburn/taglist/squarefz.png"
--theme.taglist_squares_unsel = theme.default_themes_path.."/zenburn/taglist/squarez.png"
-- }}}

-- {{{ Misc
theme.awesome_icon           = theme.default_themes_path.."/zenburn/awesome-icon.png"
theme.menu_submenu_icon      = theme.default_themes_path.."/default/submenu.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = theme.default_themes_path.."/zenburn/layouts/tile.png"
theme.layout_tileleft   = theme.default_themes_path.."/zenburn/layouts/tileleft.png"
theme.layout_tilebottom = theme.default_themes_path.."/zenburn/layouts/tilebottom.png"
theme.layout_tiletop    = theme.default_themes_path.."/zenburn/layouts/tiletop.png"
theme.layout_fairv      = theme.default_themes_path.."/zenburn/layouts/fairv.png"
theme.layout_fairh      = theme.default_themes_path.."/zenburn/layouts/fairh.png"
theme.layout_spiral     = theme.default_themes_path.."/zenburn/layouts/spiral.png"
theme.layout_dwindle    = theme.default_themes_path.."/zenburn/layouts/dwindle.png"
theme.layout_max        = theme.default_themes_path.."/zenburn/layouts/max.png"
theme.layout_fullscreen = theme.default_themes_path.."/zenburn/layouts/fullscreen.png"
theme.layout_magnifier  = theme.default_themes_path.."/zenburn/layouts/magnifier.png"
theme.layout_floating   = theme.default_themes_path.."/zenburn/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = theme.default_themes_path.."/zenburn/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.default_themes_path.."/zenburn/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = theme.default_themes_path.."/zenburn/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = theme.default_themes_path.."/zenburn/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme.default_themes_path.."/zenburn/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.default_themes_path.."/zenburn/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = theme.default_themes_path.."/zenburn/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = theme.default_themes_path.."/zenburn/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme.default_themes_path.."/zenburn/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.default_themes_path.."/zenburn/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = theme.default_themes_path.."/zenburn/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = theme.default_themes_path.."/zenburn/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme.default_themes_path.."/zenburn/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.default_themes_path.."/zenburn/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = theme.default_themes_path.."/zenburn/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = theme.default_themes_path.."/zenburn/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.default_themes_path.."/zenburn/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.default_themes_path.."/zenburn/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80

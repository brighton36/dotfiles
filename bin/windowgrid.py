#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os
import wnck
from Xlib import display

PACKAGE = "window_grid"

def _help_():
    print(u'Help:')
    print(u'Lorem ipsum dolores…')
    return(0)

def active_window():
    # returns the default screen, but it fails when not called from X
    # TODO: connect to the dbus-session-bus to get the right environment
    # (like $DISPLAY)
    screen = wnck.screen_get_default()
    screen.force_update()
    return(screen.get_active_window())

def get_resolution(orientation):
    # Accept only 'width' and 'height' for the specific resolution
    return display.Display.screen(display.Display())[orientation +
       '_in_pixels']

def get_geometry():
    # just a shorting for wnck.Window.get_geometry(active_window())
    return(active_window().get_geometry())

def window_state(head, sub = int(1)):
    # The HeadMap showing the screen; 4, 5 and 6 are vertical full,
    #                                 2, 5 and 8 are horizontal full.
    # ┌─────────────────┬─────────────────┐
    # │                 ┆                 │
    # │        7        8        9        │
    # │                 ┆                 │
    # ├┄┄┄┄┄┄┄╴4╶┄┄┄┄┄┄╴5╶┄┄┄┄┄┄╴6╶┄┄┄┄┄┄┄┤
    # │                 ┆                 │
    # │        1        2        3        │
    # │                 ┆                 │
    # └─────────────────┴─────────────────┘
    # The SubMap showing the widths
    # ┌─────────────────┐
    # │                 │
    # │        1        │
    # │                 │
    # └─────────────────┘
    # ┌───────────────────────┐
    # │                       │
    # │           2           │
    # │                       │
    # └───────────────────────┘
    # ┌───────────┐
    # │           │
    # │     3     │
    # │           │
    # └───────────┘
    head = int(head)
    x_res = get_resolution(u'width')
    y_res = get_resolution(u'height')
    x_sub_map = {1: x_res / 2, 2: x_res / 3 * 2, 3: x_res / 3}
    if (head % 3) == 1:
        x = 0
        width = x_sub_map[sub]
    elif (head % 3) == 2:
        if sub == 1:
            x = 0
            width = x_res
        else:
            x = x_res / 3
            width = x_res / 3
    else:
        x = x_res - x_sub_map[sub]
        width = x_sub_map[sub]
    if (7 <= head <= 9):
        y = 0
        height = y_res / 2
    elif (4 <= head <= 6):
        y = 0
        height = y_res
    else:
        y = y_res / 2
        height = y_res / 2
    return(int(x), int(y), int(width), int(height))

def set_geometry(geo_list):
    # DOC: http://library.gnome.org/devel/libwnck/stable/WnckWindow.html
    #      #wnck-window-set-geometry
    # gravity:  0   - current gravity
    #           1   - top left
    #           2   - top center            1     2     3
    #           3   - top right               ┏━╾─┴─╼━┓
    #           4   - center left             ╿       ╿
    #           5   - center center         4 ┤   5   ├ 6
    #           6   - center right            ╽       ╽
    #           7   - bottom left             ┗━╾─┬─╼━┛
    #           8   - bottom center         7     8     9
    #           9   - bottom right
    #           10  - static (top left)
    # only 10 does what I want… all the other gravities (even 1) don't set
    # the windows right
    active_window().unmaximize_horizontally()
    active_window().unmaximize_vertically()
    active_window().set_geometry(10, 15, geo_list[0], geo_list[1], geo_list[2], geo_list[3])
    return 0
    
def main():
    if len(sys.argv) > 2:
        print('Too many arguments! Only one allowed!')
        _help_()
        sys.exit(1)
    if len(sys.argv) < 2:
        print('Too few arguments! At least one required!')
        _help_()
        sys.exit(1)
    try:
        cmd_arg = int(str(sys.argv[1]))
    except ValueError:
        print('Wrong argument!')
        _help_()
        sys.exit(1)
    if cmd_arg == 0:
        print get_geometry()
        sys.exit(0)
    print('%s' % active_window().get_data('submap'))
    if active_window().get_data('submap') == '1':
        print('Known state! Switch to SubMap 2!')
        set_geometry(window_state(cmd_arg, 2))
        active_window().set_data('submap', 2)
    elif active_window().get_data('submap') == '2':
        if (cmd_arg % 3) == 2:
            print('Known state! Switch to SubMap 1!')
            set_geometry(window_state(cmd_arg, 1))
            active_window().set_data('submap', 1)
        else:
            print('Known state! Switch to SubMap 3!')
            set_geometry(window_state(cmd_arg, 3))
            active_window().set_data('submap', 3)
    elif active_window().get_data('submap') == '3':
        print('Known state! Switch to SubMap 1!')
        set_geometry(window_state(cmd_arg, 1))
        active_window().set_data('submap', 1)
    else:
        set_geometry(window_state(cmd_arg, 1))
        active_window().set_data('submap', 1)
    print('%s' % active_window().get_data('submap'))
    sys.exit(0)

if __name__ == '__main__':
    main()

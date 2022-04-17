# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(1)

# [Global modemap] Change modifier keys as in xmodmap
# define_modmap({
#     Key.CAPSLOCK: Key.LEFT_CTRL
# })

# [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# released, and a modifier key when held down with another key. See Xcape,
# Carabiner and caps2esc for ideas and concept.
# define_multipurpose_modmap(
#     # Enter is enter when pressed and released. Control when held down.
#     # {Key.ENTER: [Key.ENTER, Key.RIGHT_CTRL]}
#
#     # Capslock is escape when pressed and released. Control when held down.
#     {Key.CAPSLOCK: [Key.ESC, Key.LEFT_CTRL]}
#     # To use this example, you can't remap capslock with define_modmap.
# )

# Keybindings for Firefox/Chrome
define_keymap(re.compile("Firefox|Google-chrome"), {
    # Ctrl+[ and Ctrl+] to switch next/previous tab
    K("C-EQUAL"): K("C-TAB"),
    K("C-MINUS"): K("C-Shift-TAB"),
    # TODO: Ctrl-Shift-Page Up and Ctrl-Shift-Page Down
    # Type C-j to focus to the content
    K("C-j"): K("C-f6"),
    # very naive "Edit in editor" feature (just an example)
    # K("C-o"): [K("C-a"), K("C-c"), launch(["gedit"]), sleep(0.5), K("C-v")]
}, "Firefox and Chrome")


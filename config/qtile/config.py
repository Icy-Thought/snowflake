from typing import List  # noqa: F401

from modules.aesthetics import screens
from modules.keymaps import keys, mouse
from modules.layouts import floating_layout, groups, layouts

# from modules.statusbar import screens

# assert keys
# assert mouse
# assert groups
# assert layouts
# assert floating_layout

auto_fullscreen = True
auto_minimize = False
bring_front_click = False
cursor_warp = False

dgroups_app_rules = []  # type: List
dgroups_key_binder = None
dpi_scale = 1.0

focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True

wmname = "Qtile"

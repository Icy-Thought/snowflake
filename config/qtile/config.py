from typing import List  # noqa: F401

from lib.groups import floating_layout, groups, layouts
from lib.keymaps import keys, mouse
from lib.statusbar import extension_defaults, screens, widget_defaults

auto_fullscreen = True
auto_minimize = False
bring_front_click = False
cursor_warp = True

dgroups_app_rules = []  # type: List
dgroups_key_binder = None
dpi_scale = 1.0

focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True

wmname = "Qtile"

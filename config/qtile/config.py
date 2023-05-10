from typing import List  # noqa: F401

from lib.groups import floating_layout, groups, layouts
from lib.keymaps import keys, mouse
from lib.statusbar import extension_defaults, screens, widget_defaults

# Qtile imports:
from libqtile import qtile

if qtile.core.name == "x11":
    from lib.x11 import wmname
elif qtile.core.name == "wayland":
    from lib.wayland import wl_input_rules

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

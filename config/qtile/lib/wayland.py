from libqtile.backend.wayland import InputConfig

wl_input_rules = {
    "type:touchpad": InputConfig(
        drag=True,
        dwt=False,
        natural_scroll=True,
        pointer_accel=0.3,
        tap=True,
    )
}

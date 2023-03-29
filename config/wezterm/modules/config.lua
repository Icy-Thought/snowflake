local M = {}

M.term = "wezterm"
M.audible_bell = "Disabled"
M.automatically_reload_config = true
M.hide_tab_bar_if_only_one_tab = false

M.bold_brightens_ansi_colors = true
M.use_fancy_tab_bar = false
M.line_height = 1.0

M.window_padding = {
    left = 15,
    right = 15,
    top = 10,
    bottom = 0,
}

M.text_background_opacity = 1.0
M.warn_about_missing_glyphs = false
M.window_background_opacity = 0.80
M.enable_scroll_bar = false
M.scrollback_lines = 5000

M.cursor_blink_ease_in = "Constant"
M.cursor_blink_ease_out = "Constant"
-- M.default_cursor_style = "BlinkingBlock" <- high cpu-usage..

return M

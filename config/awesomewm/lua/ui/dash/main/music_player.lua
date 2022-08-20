-- █▀▄▀█ █░█ █▀ █ █▀▀   █▀█ █░░ ▄▀█ █▄█ █▀▀ █▀█ --
-- █░▀░█ █▄█ ▄█ █ █▄▄   █▀▀ █▄▄ █▀█ ░█░ ██▄ █▀▄ --
---------------- Credit: @rxyhn ------------------

local gears = require("gears")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local helpers = require("helpers")
local widgets = require("ui.widgets")
local playerctl_daemon = require("signal.playerctl")

local music_text = wibox.widget({
    font = beautiful.header_font_name .. "Medium 10",
    valign = "center",
    widget = wibox.widget.textbox,
})

local music_art = wibox.widget({
    image = beautiful.music,
    resize = false,
    valign = "center",
    halign = "center",
    widget = wibox.widget.imagebox,
})

local music_art_container = wibox.widget({
    music_art,
    forced_height = dpi(120),
    forced_width = dpi(180),
    widget = wibox.container.background,
})

local filter_color = {
    type = "linear",
    from = { 0, 0 },
    to = { 0, 200 },
    stops = { { 0, beautiful.nord3 }, { 1, beautiful.nord3 .. "cc" } },
}

local music_art_filter = wibox.widget({
    {
        bg = filter_color,
        forced_height = dpi(120),
        forced_width = dpi(200),
        widget = wibox.container.background,
    },
    direction = "east",
    widget = wibox.container.rotate,
})

local music_title = wibox.widget({
    font = beautiful.header_font_name .. "Regular 13",
    valign = "center",
    widget = wibox.widget.textbox,
})

local music_artist = wibox.widget({
    font = beautiful.header_font_name .. "Bold 16",
    valign = "center",
    widget = wibox.widget.textbox,
})

local music = wibox.widget({
    {
        {
            {
                music_art_container,
                music_art_filter,
                layout = wibox.layout.stack,
            },
            {
                {
                    {
                        music_text,
                        helpers.ui.vertical_pad(dpi(15)),
                        {

                            {
                                widget = wibox.container.scroll.horizontal,
                                step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                                fps = 60,
                                speed = 75,
                                music_artist,
                            },
                            {
                                widget = wibox.container.scroll.horizontal,
                                step_function = wibox.container.scroll.step_functions.waiting_nonlinear_back_and_forth,
                                fps = 60,
                                speed = 75,
                                music_title,
                            },
                            forced_width = dpi(170),
                            layout = wibox.layout.fixed.vertical,
                        },
                        layout = wibox.layout.fixed.vertical,
                    },
                    nil,
                    {
                        {
                            widgets.playerctl.previous(
                                20,
                                beautiful.xforeground,
                                beautiful.music_button_transparent
                            ),
                            widgets.playerctl.play(
                                beautiful.xforeground,
                                beautiful.music_button_transparent
                            ),
                            widgets.playerctl.next(
                                20,
                                beautiful.xforeground,
                                beautiful.music_button_transparent
                            ),
                            layout = wibox.layout.flex.horizontal,
                        },
                        forced_height = dpi(70),
                        margins = dpi(10),
                        widget = wibox.container.margin,
                    },
                    expand = "none",
                    layout = wibox.layout.align.vertical,
                },
                top = dpi(9),
                bottom = dpi(9),
                left = dpi(10),
                right = dpi(10),
                widget = wibox.container.margin,
            },
            layout = wibox.layout.stack,
        },
        bg = beautiful.nord3,
        shape = helpers.ui.rrect(beautiful.border_radius),
        forced_width = dpi(200),
        forced_height = dpi(200),
        widget = wibox.container.background,
    },
    margins = dpi(10),
    color = "#FF000000",
    widget = wibox.container.margin,
})

--- playerctl
--- -------------
playerctl_daemon:connect_signal(
    "metadata",
    function(_, title, artist, album_path, __, ___, ____)
        if title == "" then
            title = "Nothing Playing"
        end
        if artist == "" then
            artist = "Nothing Playing"
        end
        if album_path == "" then
            album_path = gears.filesystem.get_configuration_dir()
                .. "theme/assets/no_music.png"
        end

        music_art:set_image(gears.surface.load_uncached(album_path))
        music_title:set_markup_silently(
            helpers.ui.colorize_text(title, beautiful.xforeground)
        )
        music_artist:set_markup_silently(
            helpers.ui.colorize_text(artist, beautiful.xforeground)
        )
    end
)

playerctl_daemon:connect_signal("playback_status", function(_, playing, __)
    if playing then
        music_text:set_markup_silently(
            helpers.ui.colorize_text(
                "Now Playing",
                beautiful.music_now_playing_fg
            )
        )
    else
        music_text:set_markup_silently(
            helpers.ui.colorize_text("Music", beautiful.music_now_playing_fg)
        )
    end
end)

return music

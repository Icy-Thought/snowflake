-- █▄▄ ▄▀█ █▀█
-- █▄█ █▀█ █▀▄

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local animation = require("modules.animation")

return function(s)
    -- IMPORTS --
    s.clock = require("ui.bar.clock")(s)
    s.battery = require("ui.bar.battery")(s)
    s.volume = require("ui.bar.volume")()
    s.brightness = require("ui.bar.brightness")()
    app_launcher = require("ui.bar.app_launcher")

    -- TAGLIST --
    local modkey = "Mod4"
    local taglist_buttons = gears.table.join(
        awful.button({}, 1, function(t)
            t:view_only()
        end),
        awful.button({ modkey }, 1, function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end),
        awful.button({}, 3, awful.tag.viewtoggle),
        awful.button({ modkey }, 3, function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end),
        awful.button({}, 4, function(t)
            awful.tag.viewnext(t.screen)
        end),
        awful.button({}, 5, function(t)
            awful.tag.viewprev(t.screen)
        end)
    )

    local function tag_list(s)
        local taglist = awful.widget.taglist({
            screen = s,
            filter = awful.widget.taglist.filter.all,
            layout = { layout = wibox.layout.fixed.vertical },
            widget_template = {
                widget = wibox.container.margin,
                forced_width = dpi(15),
                forced_height = dpi(40),
                create_callback = function(self, c3, _)
                    local indicator = wibox.widget({
                        widget = wibox.container.place,
                        valign = "center",
                        {
                            widget = wibox.container.background,
                            forced_width = dpi(5),
                            shape = gears.shape.rounded_bar,
                        },
                    })

                    self.indicator_animation = animation:new({
                        duration = 0.125,
                        easing = animation.easing.linear,
                        update = function(self, pos)
                            indicator.children[1].forced_height = pos
                        end,
                    })

                    self:set_widget(indicator)

                    if c3.selected then
                        self.widget.children[1].bg =
                            beautiful.wibar_focused,
                            self.indicator_animation:set(dpi(20))
                    elseif #c3:clients() == 0 then
                        self.widget.children[1].bg =
                            beautiful.wibar_empty,
                            self.indicator_animation:set(dpi(10))
                    else
                        self.widget.children[1].bg =
                            beautiful.wibar_occupied,
                            self.indicator_animation:set(dpi(10))
                    end

                    -- Tag preview
                    self:connect_signal("mouse::enter", function()
                        if #c3:clients() > 0 then
                            awesome.emit_signal(
                                "bling::tag_preview::update",
                                c3
                            )
                            awesome.emit_signal(
                                "bling::tag_preview::visibility",
                                s,
                                true
                            )
                        end
                    end)

                    self:connect_signal("mouse::leave", function()
                        awesome.emit_signal(
                            "bling::tag_preview::visibility",
                            s,
                            false
                        )
                    end)
                end,
                update_callback = function(self, c3, _)
                    if c3.selected then
                        self.widget.children[1].bg =
                            beautiful.wibar_focused,
                            self.indicator_animation:set(dpi(20))
                    elseif #c3:clients() == 0 then
                        self.widget.children[1].bg =
                            beautiful.wibar_empty,
                            self.indicator_animation:set(dpi(10))
                    else
                        self.widget.children[1].bg =
                            beautiful.wibar_occupied,
                            self.indicator_animation:set(dpi(10))
                    end
                end,
            },
            buttons = taglist_buttons,
        })

        return wibox.widget({
            taglist,
            margins = dpi(8),
            widget = wibox.container.margin,
        })
    end

    -- SYSTRAY --

    -- BAR --
    -- assembling the bar
    s.bar = awful.popup({
        screen = s,
        type = "dock",
        minimum_height = s.geometry.height,
        maximum_height = s.geometry.height,
        minimum_width = dpi(40),
        maximum_width = dpi(40),
        placement = function(c)
            awful.placement.left(c)
        end,
        widget = {
            {
                {
                    layout = wibox.layout.align.vertical,
                    expand = "none",
                    {
                        app_launcher,
                        layout = wibox.layout.fixed.vertical,
                    },
                    tag_list(s),
                    {
                        s.brightness,
                        s.volume,
                        s.battery,
                        -- system tray
                        -- notif panel
                        s.clock,
                        spacing = dpi(8),
                        layout = wibox.layout.fixed.vertical,
                    },
                },
                left = dpi(3),
                right = dpi(3),
                widget = wibox.container.margin,
            },
            bg = beautiful.wibar_bg,
            widget = wibox.container.background,
        },
    })

    -- reserve screen space
    s.bar:struts({
        left = s.bar.maximum_width,
    })

    -- SETTINGS --
    -- Bar visibility
    local function remove_bar(c)
        if c.fullscreen or c.maximized then
            c.screen.bar.visible = false
        else
            c.screen.bar.visible = true
        end
    end

    -- i dont really understand this one
    local function add_bar(c)
        if c.fullscreen or c.maximized then
            c.screen.bar.visible = true
        end
    end

    --client.connect_signal("property::fullscreen", remove_bar)
    --client.connect_signal("request::unmanage", add_bar)
end

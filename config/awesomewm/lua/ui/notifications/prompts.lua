--  █▀█ █▀█ █▀█ █▀▄▀█ █▀█ ▀█▀ █▀
--  █▀▀ █▀▄ █▄█ █░▀░█ █▀▀ ░█░ ▄█

-- Prompts that are triggered periodically.
-- The main use case is notifs that ask if I've completed my habits.
-- These are triggered with a cron job:
--    echo "awesome.emit_signal('habit::check_journal')" | awesome-client

local awful = require("awful")
local beautiful = require("beautiful")
local naughty = require("naughty")
local helpers = require("helpers")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local user_vars = require("user_variables")

-- ░░█ █▀█ █▄░█ █░░
-- █▄█ █▀▄ █░▀█ █▄▄
-- Make sure I've written my daily jrnl entry
-- Triggers 2x per hour between 4-11pm if I haven't written
local journal_yes = naughty.action({
    name = helpers.ui.colorize_text("Yes", beautiful.xforeground),
})
journal_yes:connect_signal("invoked", function()
    awful.spawn.easy_async_with_shell("alacritty -e jrnl", function() end)
end)

local journal_no = naughty.action({
    name = helpers.ui.colorize_text("No", beautiful.xforeground),
})
journal_no:connect_signal("invoked", function()
    naughty.notification({
        app_name = "Health",
        title = ">:(",
        message = "Suit yourself.",
        timeout = 5000,
    })
end)

awesome.connect_signal("habit::journal_check", function()
    -- If I haven't journaled today, trigger prompt
    -- If I have journaled, update habit tracker (pixela)
    local cmd = "jrnl -on today"
    awful.spawn.easy_async_with_shell(cmd, function(stdout)
        if stdout == "\n" then
            naughty.notification({
                app_name = "Health",
                title = "You haven't journaled today.",
                message = "Open journal now?",
                timeout = 5000,
                actions = { journal_yes, journal_no },
            })
        else
            local cmd = "pi pixel increment -g journal"
            awful.spawn.easy_async(cmd, function() end)
        end
    end)
end)

--  █▀ █░░ █▀▀ █▀▀ █▀█
--  ▄█ █▄▄ ██▄ ██▄ █▀▀
-- Makes sure I'm going to bed on time
-- Cronjob triggers this every half hour between 12am-5am
local sleep_yes = naughty.action({
    name = helpers.ui.colorize_text("Okay.", beautiful.xforeground),
})
sleep_yes:connect_signal("invoked", function()
    naughty.notification({
        app_name = "Health",
        title = "Awesome!",
        message = "Hope you go to sleep soon.",
        timeout = 3,
    })
end)

local sleep_no = naughty.action({
    name = helpers.ui.colorize_text("Nah.", beautiful.xforeground),
})
sleep_no:connect_signal("invoked", function()
    naughty.notification({
        app_name = "Health",
        title = "You decide not to sleep.",
        message = "You will regret this in the\nmorning.",
        fg = beautiful.nord11,
        timeout = 5000,
    })
end)

awesome.connect_signal("habit::sleep_check", function()
    naughty.notification({
        app_name = "Health",
        title = "You're still awake?",
        message = "Maybe consider a melatonin?\nThey're in your desk drawer.",
        timeout = 5000,
        actions = { sleep_yes, sleep_no },
    })
end)

-- █░░ █▀▀ █▀▄ █▀▀ █▀▀ █▀█
-- █▄▄ ██▄ █▄▀ █▄█ ██▄ █▀▄
-- Checks if I've updated my ledger today
-- Every 3 hours
local ledger_yes = naughty.action({
    name = helpers.ui.colorize_text("Yes", beautiful.xforeground),
})

ledger_yes:connect_signal("invoked", function()
    local ledger_dir = user_vars.dash.ledger_dir
    awful.spawn.with_shell([[
      tmux new-window -c ]] .. ledger_dir .. [[\; \
      send-keys "vp *" Enter \; \
      splitw -h -c "#{pane_current_path}"\; \
      splitw -v -c "#{pane_current_path}"\; \
      rename-window ledger
    ]])
end)

local ledger_no = naughty.action({
    name = helpers.ui.colorize_text("No", beautiful.xforeground),
})

awesome.connect_signal("habit::ledger_check", function()
    naughty.notification({
        app_name = "Health",
        title = "Ledger check",
        message = "Open ledger now?",
        timeout = 5000,
        actions = { ledger_yes, ledger_no },
    })
end)

-- █▀▄▀█ █▀█ █▀█ █▀▄
-- █░▀░█ █▄█ █▄█ █▄▀
-- Simple mood tracker

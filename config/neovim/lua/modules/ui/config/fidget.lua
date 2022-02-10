local fidget = require("fidget")

fidget.setup ({
	text = {
		spinner = {
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
			" ",
		},
		done = "",
		commenced = " ",
		completed = " ",
	},
	align = {
		bottom = true,
		right = true,
	},
	timer = {
		spinner_rate = 100,
		fidget_decay = 500,
		task_decay = 300,
	},
	window = {
		relative = "editor",
		blend = 100,
		zindex = nil,
	},
	fmt = {
		leftpad = true,
		stack_upwards = true,
		fidget = function(fidget_name, spinner)
			return string.format("%s %s", spinner, fidget_name)
		end,
		task = function(task_name, message, percentage)
			return string.format("%s%s [%s]", message, percentage and string.format(" (%s%%)", percentage) or "", task_name)
		end,
	},
	debug = {
		logging = false,
	},
})

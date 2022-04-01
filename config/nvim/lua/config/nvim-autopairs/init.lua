local npairs = prequire("nvim-autopairs")

npairs.setup({
    check_ts = true,
    ts_config = {
        lua = {"string"},
        javascript = {"template_string"},
        java = false,
    },
    fast_wrap = {
      map = "<M-e>",
      chars = { "{", "[", "(", '"', "'" },
      pattern = string.gsub([[ [%"%"%)%>%]%)%}%,] ]], "%s+", ""),
      offset = -1,
      end_key = "$",
      keys = "qwertyuiopzxcvbnmasdfghjkl",
      check_comma = true,
      highlight = "Search",
      highlight_grey="Comment"
    },
})

-- If you want insert `(` after select function or method item
local cmp = prequire("cmp")
local cmp_autopairs = require("nvim-autopairs.completion.cmp")

cmp.event:on( "confirm_done", cmp_autopairs.on_confirm_done({  map_char = { tex = "" } }))

-- Add a lisp filetype (wrap my-function)
-- FYI: Hardcoded = { "clojure", "clojurescript", "fennel", "janet" }
cmp_autopairs.lisp[#cmp_autopairs.lisp+1] = "racket"

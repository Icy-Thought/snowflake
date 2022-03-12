local kanagawa = prequire('kanagawa')

kanagawa.setup({
    undercurl = true, -- enable undercurls
    commentStyle = "italic",
    functionStyle = "bold,italic",
    keywordStyle = "italic",
    statementStyle = "bold",
    typeStyle = "NONE",
    variablebuiltinStyle = "italic",
    specialReturn = true, -- special highlight for the return keyword
    specialException = true, -- special highlight for exception handling keywords
    transparent = false, -- do not set background color
    dimInactive = true, -- dim inactive window `:h hl-NormalNC`
    colors = {},
    overrides = {},
})

local lualine = prequire('lualine')

lualine.setup({
  options = {
    theme = 'kanagawa',
  },
})

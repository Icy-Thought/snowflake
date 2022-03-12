local ayu = prequire('ayu')

ayu.setup({
    mirage = true,
    overrides = {},
})

local lualine = prequire('lualine')

lualine.setup({
  options = {
    theme = 'ayu',
  },
})

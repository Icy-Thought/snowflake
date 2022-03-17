local toggleterm = prequire("toggleterm")

toggleterm.setup({
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.40
            end
        end,
    open_mapping = [[<c-\>]],
    hide_numbers = true,
    shade_filetypes = {},
    shade_terminals = false,
    shading_factor = "1",                                                   
    start_in_insert = true,
    insert_mappings = true,                                                 
    terminal_mappings = true,                                               
    persist_size = true,
    direction = "vertical",                                                 
    close_on_exit = true,                                                   
    shell = vim.o.shell,                                                    
                                                                            
    float_opts = {
        border = "curved",
        winblend = 3,
        highlights = {
            border = "Normal",
            background = "Normal",
        }
    }
})

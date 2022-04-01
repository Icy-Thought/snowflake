vim.cmd([[
call wilder#setup({"modes": [":", "/", "?"]})

call wilder#set_option("pipeline", [
      \   wilder#branch(
      \     wilder#python_file_finder_pipeline({
      \       "file_command": {_, arg -> arg[0] ==# "." ? ["rg", "--files", "--hidden"] : ["rg", "--files"]},
      \       "dir_command": {_, arg -> arg[0] ==# "." ? ["fd", "-tf", "-H"] : ["fd", "-tf"]},
      \       "filters": ["fuzzy_filter", "difflib_sorter"],
      \     }),
      \     wilder#substitute_pipeline({
      \       "pipeline": wilder#python_search_pipeline({
      \         "skip_cmdtype_check": 1,
      \         "pattern": wilder#python_fuzzy_pattern({
      \           "start_at_boundary": 0,
      \         }),
      \       }),
      \     }),
      \     wilder#cmdline_pipeline({
      \       "fuzzy": 1,
      \       "fuzzy_filter": has("nvim") ? wilder#lua_fzy_filter() : wilder#vim_fuzzy_filter(),
      \     }),
      \     [
      \       wilder#check({_, x -> empty(x)}),
      \       wilder#history(),
      \     ],
      \     wilder#python_search_pipeline({
      \       "pattern": wilder#python_fuzzy_pattern({
      \         "start_at_boundary": 0,
      \       }),
      \     }),
      \   ),
      \ ])

let s:highlighters = [
      \ wilder#pcre2_highlighter(),
      \ wilder#lua_fzy_highlighter(),
      \ ]

let s:popupmenu_renderer = wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
      \ "highlights": {
      \    "default": "Normal",
      \    "border": "NormalFloat",
      \    "accent": wilder#make_hl("WilderAccent", "Normal", "TabLineSel"),
      \ },
      \ "border": "rounded",
      \ "empty_message": wilder#popupmenu_empty_message_with_spinner(),
      \ "highlighter": s:highlighters,
      \ "left": [
      \   " ",
      \   wilder#popupmenu_devicons(),
      \   wilder#popupmenu_buffer_flags({
      \     "flags": " a + ",
      \     "icons": {"+": "", "a": "", "h": ""},
      \   }),
      \ ],
      \ "right": [
      \   " ",
      \   wilder#popupmenu_scrollbar(),
      \ ],
      \ }))

let s:wildmenu_renderer = wilder#wildmenu_renderer({
      \ "highlighter": s:highlighters,
      \ "separator": " · ",
      \ "left": [" ", wilder#wildmenu_spinner(), " "],
      \ "right": [" ", wilder#wildmenu_index()],
      \ })

call wilder#set_option("renderer", wilder#renderer_mux({
      \ ":": s:popupmenu_renderer,
      \ "/": s:wildmenu_renderer,
      \ "substitute": s:wildmenu_renderer,
      \ }))
]])

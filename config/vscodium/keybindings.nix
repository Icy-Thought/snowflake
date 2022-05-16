{...}: [
  {
    command = "markdown.showPreview";
    key = "ctrl+shift+m";
    when = "editorLangId == 'markdown'";
  }
  {
    command = "-markdown.showPreview";
    key = "ctrl+shift+v";
    when = "editorLangId == 'markdown'";
  }
  {
    command = "vim.showQuickpickCmdLine";
    key = "shift+;";
    when = "editorTextFocus && vim.mode != 'Insert'";
  }
]

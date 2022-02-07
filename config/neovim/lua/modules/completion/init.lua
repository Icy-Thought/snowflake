local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

require "modules.completion.null-ls"
require "modules.completion.installer"
require("modules.completion.handlers").setup()

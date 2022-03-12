local lsp_installer = prequire('nvim-lsp-installer')

lsp_installer.on_server_ready(function(server)
    local opts = {
        on_attach = require("user.lsp.handlers").on_attach,
        capabilities = require("user.lsp.handlers").capabilities,
    }

     if server.name == "hls" then
        local hls_opts = require("user.lsp.settings.hls")
        opts = vim.tbl_deep_extend("force", hls_opts, opts)
     end

     if server.name == "sumneko_lua" then
        local sumneko_opts = require("user.lsp.settings.sumneko_lua")
        opts = vim.tbl_deep_extend("force", sumneko_opts, opts)
     end

    server:setup(opts)
end)

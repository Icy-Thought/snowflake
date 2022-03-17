local null_ls = prequire("null-ls")

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
local code_actions = null_ls.builtins.code_actions

null_ls.setup {
    debounce = 150,
    save_after_format = false,
    sources = {
        formatting.stylua,
        formatting.prettier.with({ extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } }),
        formatting.black.with { extra_args = { "--fast" }, filetypes = { "python" } },
        formatting.isort.with { extra_args = { "--profile", "black" }, filetypes = { "python" } },
        diagnostics.ansiblelint.with {
            condition = function(utils)
                return utils.root_has_file "roles" and utils.root_has_file "inventories"
            end,
        },
        diagnostics.shellcheck,
        diagnostics.markdownlint.with {
            filetypes = { "markdown" },
        },
        diagnostics.vale.with {
            filetypes = { "markdown" },
        },
        diagnostics.revive.with {
            condition = function(utils)
                return utils.root_has_file "revive.toml"
            end,
        },
        code_actions.shellcheck,
    },
}

-- Configure LSP servers using Neovim 0.11+ native API
vim.lsp.config("hls", {})
vim.lsp.config("nixd", {})
vim.lsp.config("pyright", {})
vim.lsp.config("ts_ls", {})
vim.lsp.config("yamlls", {})
vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
            },
        },
    },
})
vim.lsp.config("terraformls", {})

-- Enable all configured servers
vim.lsp.enable("hls")
vim.lsp.enable("nixd")
vim.lsp.enable("pyright")
vim.lsp.enable("ts_ls")
vim.lsp.enable("yamlls")
vim.lsp.enable("lua_ls")
vim.lsp.enable("terraformls")

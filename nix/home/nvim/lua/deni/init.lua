-- already nvim defaults: filetype plugin indent on

-- Show line numbers
vim.opt.number = true

-- Whitespace: Show whitespace characters
vim.opt.list = true

-- Tabs and Spaces Configuration
vim.opt.tabstop = 4 -- Number of spaces that a <Tab> in the file counts for
vim.opt.shiftwidth = 4 -- Number of spaces to use for each step of (auto)indent
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.softtabstop = 4 -- Number of spaces that a <Tab> counts for while performing editing operations

-- Conceal feature (disabled by default)
vim.opt.conceallevel = 0

-- Wrapping: Disable line wrapping
vim.opt.wrap = false

-- Use system clipboard for yanks, deletes, changes, and puts
vim.opt.clipboard:append("unnamedplus")

-- Highlight trailing whitespace in Neovim Lua configuration

-- -- Define the highlight group 'ExtraWhitespace' with a red background
-- vim.api.nvim_set_hl(0, 'ExtraWhitespace', { bg = '#FF0000' })
--
-- -- Initial match to highlight trailing whitespace
-- vim.cmd([[match ExtraWhitespace /\s\+$/]])
--
-- -- Autocommands to update the match patterns based on the editing context
-- vim.api.nvim_create_autocmd('BufWinEnter', {
--     pattern = '*',
--     command = [[match ExtraWhitespace /\s\+$/]],
-- })
--
-- vim.api.nvim_create_autocmd('InsertEnter', {
--     pattern = '*',
--     command = [[match ExtraWhitespace /\s\+\%#\@<!$/]],
-- })
--
-- vim.api.nvim_create_autocmd('InsertLeave', {
--     pattern = '*',
--     command = [[match ExtraWhitespace /\s\+$/]],
-- })
--
-- vim.api.nvim_create_autocmd('BufWinLeave', {
--     pattern = '*',
--     callback = function()
--         vim.fn.clearmatches()
--     end,
-- })

-- Autocommand to remove trailing whitespace before saving the file
vim.api.nvim_create_autocmd('BufWritePre', {
    pattern = '*',
    command = [[:%s/\s\+$//e]],
})

-- vim.g.mapleader = " ";

require("deni.keymaps")
require("deni.plugins.treesitter")
require("deni.plugins.telescope")
require("deni.plugins.ui")
require("deni.plugins.utils")
require("deni.plugins.formatting")
require("deni.plugins.linting")
require("deni.plugins.lsp")
require("deni.plugins.coding")

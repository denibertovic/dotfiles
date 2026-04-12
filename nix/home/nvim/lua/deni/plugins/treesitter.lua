-- Treesitter highlight is built-in to neovim; ensure it's enabled for all filetypes
vim.api.nvim_create_autocmd("FileType", {
	callback = function(args)
		pcall(vim.treesitter.start, args.buf)
	end,
})

-- Textobjects move
local move = require("nvim-treesitter-textobjects.move")
local opts = { noremap = true, silent = true }

vim.keymap.set({ "n", "x", "o" }, "]f", function() move.goto_next_start("@function.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "]c", function() move.goto_next_start("@class.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "]a", function() move.goto_next_start("@parameter.inner") end, opts)
vim.keymap.set({ "n", "x", "o" }, "]F", function() move.goto_next_end("@function.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "]C", function() move.goto_next_end("@class.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "]A", function() move.goto_next_end("@parameter.inner") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[f", function() move.goto_previous_start("@function.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[c", function() move.goto_previous_start("@class.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[a", function() move.goto_previous_start("@parameter.inner") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[F", function() move.goto_previous_end("@function.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[C", function() move.goto_previous_end("@class.outer") end, opts)
vim.keymap.set({ "n", "x", "o" }, "[A", function() move.goto_previous_end("@parameter.inner") end, opts)

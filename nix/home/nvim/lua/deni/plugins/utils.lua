require("mini.pairs").setup({
	modes = { insert = true, command = true, terminal = false },
	-- skip autopair when next character is one of these
	skip_next = [=[[%w%%%'%[%"%.%`%$]]=],
	-- skip autopair when the cursor is inside these treesitter nodes
	skip_ts = { "string" },
	-- skip autopair when next character is closing pair
	-- and there are more closing pairs than opening pairs
	skip_unbalanced = true,
	-- better deal with markdown code blocks
	markdown = true,
})
require("mini.surround").setup()
require("mini.trailspace").setup()

-- better commenting
require("ts-comments").setup({
    lang = {
        terraform = { "# %s", "/* %s */" },
        nix = "# %s"
    }
})
-- ts_context_commentstring is not required apart for the calculate_commentstring function below when using
-- it with mini.comment
-- require("ts_context_commentstring").setup({enable_autocmd = false})
-- require("ts_context_commentstring").setup({})

-- Workaround as per https://github.com/LazyVim/LazyVim/discussions/654#discussioncomment-10978917
-- I'm not sure why mini.comment and ts-comments and vimPlugins.nvim-ts-context-comm don't have this working by default
-- It does seem to work for other filetypes though.
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "nix",
--   callback = function()
--     vim.bo.commentstring = "# %s"
--   end
-- })

-- another example for setting comment strings
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "terraform",
--   callback = function()
--     vim.bo.commentstring = "# %s"
--   end
-- })

require("mini.comment").setup({
      custom_commentstring = function()
        return require("ts_context_commentstring.internal").calculate_commentstring() or vim.bo.commentstring
      end
})

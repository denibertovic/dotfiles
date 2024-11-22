-- buffer manipulation
vim.keymap.set("n", "<C-j>", vim.cmd.bprev)
vim.keymap.set("n", "<C-k>", vim.cmd.bnext)
vim.keymap.set({ "n", "v", "o" }, "<C-d>", vim.cmd.bd)

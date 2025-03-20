-- buffer manipulation
vim.keymap.set("n", "<C-j>", vim.cmd.bprev)
vim.keymap.set("n", "<C-k>", vim.cmd.bnext)
vim.keymap.set({ "n", "v", "o" }, "D", vim.cmd.bd)
-- vim.keymap.set({ "n", "v", "o" }, "<leader>x", "<Esc>:bprevious<bar>bdelete #<Return>")
-- vim.keymap.set({ "n", "v", "o" }, "<leader>X", ":%bd!|e#<cr>")

vim.keymap.set("n", "<leader>dt", function()
   -- this will be deprecated in favor of is_enabled()
   -- so the logic will need to be switched
   if vim.diagnostic.is_disabled() then
     vim.diagnostic.enable()
   else
     vim.diagnostic.disable()
   end
end)

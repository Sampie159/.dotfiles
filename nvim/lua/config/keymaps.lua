-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "S", "cc", { noremap = true, silent = true })

vim.keymap.set("n", "<C-e>", "<cmd>ClangdSwitchSourceHeader<CR>", { noremap = true, silent = true })

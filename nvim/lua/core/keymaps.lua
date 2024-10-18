-- Kill buffer
vim.keymap.set("n", "<leader>bk", "<cmd>:bdelete!<CR>")

-- Keymaps for better default experience
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Copied from ThePrimeagen
-- vim.keymap.set("x", "<leader>p", "\"_dP")
vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("v", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>Y", "\"+Y")
vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", ";s", [[:%s/\<<C-r><C-w>\>/<C-w>/gI<Left><Left><Left>]])
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<cr>", { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })

vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- Move.nvim
local moveopts = { noremap = true, silent = true }

vim.keymap.set('n', '<A-j>', ':m .+1<CR>==', moveopts)
vim.keymap.set('n', '<A-k>', ':m .-2<CR>==', moveopts)
vim.keymap.set('v', '<A-j>', ':m \'>+1<CR>gv=gv', moveopts)
vim.keymap.set('v', '<A-k>', ':m \'<-2<CR>gv=gv', moveopts)

-- Copilot
-- vim.keymap.set("n", "<leader>cp", "<cmd> Copilot enable<CR>")

-- C-d C-u center screen
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Sort selected lines
vim.keymap.set("v", "<leader>s", ":sort<CR>")

-- Reverse sort selected lines
vim.keymap.set("v", "<leader>S", ":sort!<CR>")

vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)

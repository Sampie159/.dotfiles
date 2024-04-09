require('oil').setup()

vim.keymap.set("n", "-", require('oil').open, { desc = "Open Parent Directory" })

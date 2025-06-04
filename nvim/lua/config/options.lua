-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.g.autoformat = false

-- LSP Configs

vim.lsp.config('ols', {
    cmd = { "ols", "--stdio" },
    filetypes = { "odin" },
    root_markers = { '.git', 'odinfmt.json', 'ols.json' },
})
vim.lsp.enable('ols')

vim.lsp.config('zls', {
    cmd = { 'zls' },
    filetypes = { 'zig', 'zir' },
    root_markers = { '.git', 'build.zig', 'zls.json' },
})
vim.lsp.enable('zls')

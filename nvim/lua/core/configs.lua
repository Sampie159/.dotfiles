-- Leader as <space>confi
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Search highlight
vim.o.hlsearch = false

-- Line numbers default
-- vim.wo.number = true

-- Enable mouse
vim.o.mouse = 'a'

vim.o.cindent = true
vim.o.cino = "N-s,g0,:0,E-s,(0"

-- Sync clipboard
vim.o.clipboard = 'unnamedplus'

-- Break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Relative numbers
-- vim.o.relativenumber = true

-- Tab stop
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeout = true
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = "menu,menuone"
vim.opt.completeopt = "menu,menuone"

-- Set termguicolors
vim.o.termguicolors = true

-- Set scrolloff
vim.o.scrolloff = 8

-- [[ Highlight on yank ]]
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

vim.cmd([[autocmd FileType * set formatoptions-=ro]])

vim.o.autoread = true

vim.opt.conceallevel = 2
vim.opt.concealcursor = 'nc'

vim.opt.shell = "/bin/bash"

vim.g.zig_fmt_autosave = 0

vim.lsp.inlay_hint.enable(false)
vim.diagnostic.enable(false)

if vim.g.neovide then
    vim.o.guifont = "AnonymicePro Nerd Font:h12"
end

for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
    vim.api.nvim_set_hl(0, group, {})
end

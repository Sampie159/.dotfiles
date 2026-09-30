vim.bo.shiftwidth = 2
-- haskell-language-server relies heavily on codeLenses
vim.keymap.set('n', '<space>cl', vim.lsp.codelens.run, { noremap = true, silent = true, buffer = 0 })

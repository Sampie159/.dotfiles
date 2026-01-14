require('nvim-treesitter').setup {
    install_dir = vim.fn.stdpath('data') .. '/size',
}

vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
    callback = function(args) pcall(vim.treesitter.start(), args.buf) end,
})

require 'treesitter-context'.setup {
    enable = true,
    max_lines = 0,
    min_window_height = 0,
    line_numbers = true,
    multiline_threshold = 20,
    trim_scope = 'outer',
    mode = 'cursor',
    separator = nil,
    zindex = 20,
    event = "BufRead",
}

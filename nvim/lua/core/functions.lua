local function label(args)
    local lbl = args.args
    local buf = vim.api.nvim_get_current_buf()
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local str = "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */"
    local lbl_length = string.len(lbl)
    local padding = (75 - lbl_length) / 2
    local extra = lbl_length % 2 == 0 and 1 or 0
    local str2 = "/*" ..
        string.rep(" ", padding) ..
        string.upper(lbl) .. string.rep(" ", padding + extra) .. "*/"

    vim.api.nvim_buf_set_lines(buf, line, line, false, { str, str2, str })
end

vim.api.nvim_create_user_command('Label', label, { nargs = "*" })

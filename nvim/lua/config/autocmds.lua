-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

local function insert_header_guard()
    local filename = vim.fn.expand("%:t:r")
    local ext = "_" .. string.upper(vim.fn.expand("%:t:e"))
    local guard = "_" .. string.upper(filename) .. ext .. "_"

    local lines = {
        "#if !defined(" .. guard .. ")",
        "",
        "",
        "",
        "#define " .. guard,
        "#endif /* " .. guard .. " */",
    }

    -- Replace the whole buffer with these lines (set the lines from 0 to line count)
    vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)

    -- Move cursor to the third line (line 3, column 0)
    vim.api.nvim_win_set_cursor(0, { 3, 0 })
end

vim.api.nvim_create_autocmd("BufNewFile", {
    pattern = { "*.h", "*.hh", "*.hpp" },
    callback = insert_header_guard,
})

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

local function vcvars()
    local buf = vim.api.nvim_get_current_buf()
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local str = [[
SET "LIB="
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio 12.0
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio 11.0
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio 10.0
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio 13.0
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio 14.0
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files\Microsoft Visual Studio\2022\Community
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
SET VC_PATH=C:\Program Files\Microsoft Visual Studio\2022\Professional
IF NOT DEFINED LIB (IF EXIST "%VC_PATH%" (call "%VC_PATH%\VC\Auxiliary\Build\vcvarsall.bat" x64))
]]
    local lines = {}
    for s in str:gmatch("[^\r\n]+") do
        table.insert(lines, s)
    end

    vim.api.nvim_buf_set_text(buf, line, 0, line, 0, lines)
end

vim.api.nvim_create_user_command('Label', label, { nargs = "*" })
vim.api.nvim_create_user_command('Vcvars', vcvars, {})

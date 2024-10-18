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
set msvc22p=C:\Program Files\Microsoft Visual Studio\2022\Professional
set msvc22c=C:\Program Files\Microsoft Visual Studio\2022\Community
set msvc19p=C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional
set msvc19c=C:\Program Files (x86)\Microsoft Visual Studio\2019\Community
set msvc17p=C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional
set msvc17c=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community
set msvc14=C:\Program Files (x86)\Microsoft Visual Studio 14.0
set msvc13=C:\Program Files (x86)\Microsoft Visual Studio 13.0
set msvc12=C:\Program Files (x86)\Microsoft Visual Studio 12.0
set msvc11=C:\Program Files (x86)\Microsoft Visual Studio 11.0
set msvc10=C:\Program Files (x86)\Microsoft Visual Studio 10.0
if exist "%msvc22p%" (
    call "%msvc22p%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc22c%" (
    call "%msvc22c%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc19p%" (
    call "%msvc19p%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc19c%" (
    call "%msvc19c%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc17p%" (
    call "%msvc17p%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc17c%" (
    call "%msvc17c%\VC\Auxiliary\Build\vcvarsall.bat" x64
) else if exist "%msvc14%" (
    call "%msvc14%\VC\vcvarsall.bat" x64
) else if exist "%msvc13%" (
    call "%msvc13%\VC\vcvarsall.bat" x64
) else if exist "%msvc12%" (
    call "%msvc12%\VC\vcvarsall.bat" x64
) else if exist "%msvc11%" (
    call "%msvc11%\VC\vcvarsall.bat" x64
) else if exist "%msvc10%" (
    call "%msvc10%\VC\vcvarsall.bat" x64
)
]]
    local lines = {}
    for s in str:gmatch("[^\r\n]+") do
        table.insert(lines, s)
    end

    vim.api.nvim_buf_set_text(buf, line, 0, line, 0, lines)
end

vim.api.nvim_create_user_command('Label', label, { nargs = "*" })
vim.api.nvim_create_user_command('Vcvars', vcvars, {})

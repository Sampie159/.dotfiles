local ls = require('luasnip')
local s = ls.s
local i = ls.i
local t = ls.t

local d = ls.dynamic_node
local c = ls.choice_node
local f= ls.function_node
local sn = ls.snippet_node

local fmt = require('luasnip.extras.fmt').fmt
local rep = require('luasnip.extras').rep

local snippets, autosnippets = {}, {}

local vcvars = s('vcvars', fmt([[
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


]], {}))
table.insert(autosnippets, vcvars)

return snippets, autosnippets

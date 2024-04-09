local ls = require('luasnip')
local s = ls.s
local i = ls.i
local t = ls.t

local d = ls.dynamic_node
local c = ls.choice_node
local f = ls.function_node
local sn = ls.snippet_node

local fmt = require('luasnip.extras.fmt').fmt
local rep = require('luasnip.extras').rep

local snippets, autosnippets = {}, {}

local constd = s('constd', t('const std = @import("std");'))
table.insert(autosnippets, constd)

local constout = s('consout', t('const stdout = std.io.getStdOut().writer();'))
table.insert(autosnippets, constout)

local constin = s('consin', t('const stdin = std.io.getStdIn().reader();'))
table.insert(autosnippets, constin)

local consterr = s('conserr', t('const stderr = std.io.getStdErr().writer();'))
table.insert(autosnippets, consterr)

local import = s({ trig = "imp ([%a%d]+)%s", regTrig = true }, {
  f(function(_, snip)
    return "const " .. snip.captures[1] .. " = @import(\"" .. snip.captures[1] .. ".zig\");"
  end),
})
table.insert(autosnippets, import)

return snippets, autosnippets

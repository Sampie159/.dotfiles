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

local cfgtests = s('ctst', fmt([[
#[cfg(test)]
mod tests {{
    use super::*;

    {}
}}
]], { i(1, '// Your tests here') }))
table.insert(autosnippets, cfgtests)

local test = s('tst', fmt([[
#[test]
{}
]], { i(1) }))
table.insert(autosnippets, test)

local derive = s('#der', fmt([[
#[derive({})]
{}
]], { i(1), i(2) }))
table.insert(autosnippets, derive)

local should_panic = s('shp', fmt([[
#[test]
#[should_panic]
{}
]], { i(1) }))
table.insert(autosnippets, should_panic)

return snippets, autosnippets

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

local lpsd = s('lpsd', fmt([[
import type {{ PageServerData }} from './$types';

export const load: PageServerData = {} ({}) => {{
  {}
}};
]], {
    c(1, { t('async'), t('') }),
    c(2, { t(''), i(1, 'parameters') }),
    i(3, '// Function body'),
  }))
table.insert(autosnippets, lpsd)

local lpd = s('lpd', fmt([[
import type {{ PageData }} from './$types';

export const load: PageData = {} ({}) => {{
  {}
}};
]], {
    c(1, { t('async'), t('') }),
    c(2, { t(''), i(1, 'parameters') }),
    i(3, '// Function body'),
  }))
table.insert(autosnippets, lpd)

local formactions = s('formactions', fmt([[
import type {{ Actions }} from './$types';

export const actions: Actions = {{
  {}
}}
]], {
    i(1, '// Actions')
  }))
table.insert(autosnippets, formactions)

return snippets, autosnippets

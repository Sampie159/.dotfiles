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

local scts = s('scts', fmt([[
<script lang="ts">
    {}
</script>
]], {
    i(1, '// Your code here')
}))
table.insert(autosnippets, scts)

local sts = s('sts', fmt([[
<style lang="scss">
    {}
</style>
]], {
    i(1, '// Your code here')
}))
table.insert(autosnippets, sts)

local stpc = s('stpc', fmt([[
<style lang="postcss">
    {}
</style>
]], {
    i(1, '// Your code here')
}))
table.insert(autosnippets, stpc)

return snippets, autosnippets

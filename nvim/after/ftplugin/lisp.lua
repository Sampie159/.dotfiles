-- Common Lisp indenting: Vim's built-in lisp indenter, plus two fixes.

-- `if` branches line up with the test instead of indenting by 2.
vim.opt_local.lispwords:remove('if')

-- flet/labels/macrolet: the built-in indenter treats each local definition as a
-- function call and aligns its body with the lambda list. Indent it by 2 instead.
local local_fn_forms = { flet = true, labels = true, macrolet = true }
local literals = { str_lit = true, char_lit = true, comment = true, block_comment = true }

-- Ignore parens inside strings, comments and #\( character literals.
local function in_literal()
    local ok, node = pcall(vim.treesitter.get_node)
    return (ok and node and literals[node:type()]) and 1 or 0
end

-- Moves the cursor to the open paren enclosing it; returns {lnum, col} or nil.
local function enclosing_paren()
    local pos = vim.fn.searchpairpos('(', '', ')', 'bW', in_literal)
    return pos[1] > 0 and pos or nil
end

function _G.lisp_indent()
    local lnum = vim.v.lnum
    pcall(function() vim.treesitter.get_parser():parse() end)

    vim.fn.cursor(lnum, 1)
    local definition = enclosing_paren()
    local bindings = definition and enclosing_paren()
    local form = bindings and enclosing_paren()

    if form and form[1] == bindings[1] then
        local head = vim.fn.getline(form[1]):sub(form[2] + 1, bindings[2] - 1):match('^%s*([%w-]+)%s*$')
        if head and local_fn_forms[head:lower()] then
            return vim.fn.virtcol(definition) + 1
        end
    end
    return vim.fn.lispindent(lnum)
end

vim.bo.lispoptions = 'expr:1'
vim.bo.indentexpr = 'v:lua.lisp_indent()'

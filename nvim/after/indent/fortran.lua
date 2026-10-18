-- The built-in indenter only re-indents on single words like `end`, so `end if`,
-- `end subroutine` and `contains` never dedent. Trigger on the full keyword too.
local words = { 'contains', 'class', 'rank', 'type is' }
for _, w in ipairs({ 'if', 'do', 'select', 'where', 'forall', 'associate', 'block', 'critical', 'enum',
    'type', 'interface', 'team', 'function', 'subroutine', 'module', 'submodule', 'program' }) do
    table.insert(words, 'end ' .. w)
end
for _, w in ipairs(words) do
    vim.bo.indentkeys = vim.bo.indentkeys .. ',=~' .. w
end
vim.bo.indentkeys = vim.bo.indentkeys .. ',*<CR>'
vim.b.fortran_do_enddo = 1

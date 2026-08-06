local cmp = require 'cmp'

cmp.setup {
    completion = {
        -- autocomplete = false,
        completeopt = "menu,menuone",
    },
    preselect = cmp.PreselectMode.None,
    mapping = cmp.mapping.preset.insert {
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-l>'] = function()
            if not cmp.visible() then
                cmp.complete()
            end
        end,
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'lazydev' },
    },
}

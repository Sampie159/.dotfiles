local cmp = require 'cmp'

cmp.setup {
    completion = {
        autocomplete = false,
        completeopt = "menu,menuone",
    },
    mapping = cmp.mapping.preset.insert {
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-i>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        },
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-l>'] = cmp.mapping.complete {},
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'lazydev' },
        { name = 'orgmode' },
    },
}

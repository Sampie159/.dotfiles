local ls = require('luasnip')

require('luasnip.loaders.from_lua').load({ paths = '~/.config/nvim/snippets/' })

ls.config.set_config({
  history = true,
  updateevents = 'TextChanged,TextChangedI',
  enable_autosnippets = true,
  ext_opts = {
    [require('luasnip.util.types').choiceNode] = {
      active = {
        virt_text = { { '+', 'GruvboxOrange' } },
      }
    }
  }
})

vim.keymap.set({ 'i', 's' }, '<C-j>', function() if ls.jumpable(1) then ls.jump(1) end end, { silent = true })
vim.keymap.set({ 'i', 's' }, '<C-k>', function() if ls.jumpable(-1) then ls.jump(-1) end end, { silent = true })
vim.keymap.set({ 'i', 's' }, '<C-e>', function() if ls.choice_active() then ls.change_choice(1) end end, { silent = true })

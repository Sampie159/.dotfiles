-- Main plugins

require('core.plugin_config.harpoon_config')
require('core.plugin_config.cmp_config')
require('core.plugin_config.telescope_config')
require('core.plugin_config.treesitter_config')
require('core.plugin_config.lsp_config')
require('core.plugin_config.luasnip_config')

vim.g.aqua_bold = 0
local hour = tonumber(os.date("%H"))
if hour > 17 or hour < 9 then
    vim.g.aquarium_style = "dark"
    vim.o.background = "dark"
else
    vim.g.aquarium_style = "light"
    vim.o.background = "light"
end
vim.cmd.colorscheme("aquarium")

-- I got tired of creating separate files for each plugin configuration, so I'll
-- just put them all here. I'll try to keep it organized, though.

require('mini.surround').setup()

-- Git stuf
-- Neogit
require('neogit').setup {}
vim.keymap.set("n", "<leader>gn", "<cmd>Neogit<CR>")

-- Lazygit
vim.keymap.set("n", "<leader>gl", "<CMD>LazyGit<CR>")

-- Orgmode
local orgmode = require('orgmode')
-- orgmode.setup_ts_grammar()
orgmode.setup({
    org_agenda_files = { '~/org/**/*' },
    org_default_notes_file = '~/org/notes.org',
    mappings = {
        org = {
            org_toggle_checkbox = '<C-b>',
        },
    },
})

-- nvim-spectre
vim.keymap.set('n', '<leader>S', '<cmd>lua require("spectre").toggle()<CR>', {
    desc = "Toggle Spectre"
})
vim.keymap.set('n', '<leader>sw', '<cmd>lua require("spectre").open_visual({select_word=true})<CR>', {
    desc = "Search current word"
})
vim.keymap.set('v', '<leader>sw', '<esc><cmd>lua require("spectre").open_visual()<CR>', {
    desc = "Search current word"
})
vim.keymap.set('n', '<leader>sp', '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>', {
    desc = "Search on current file"
})

vim.g.haskell_tools = {
    hls = {
        settings = {
            haskell = {
                formattingProvider = "stylish-haskell"
            }
        }
    }
}

require'nvim-web-devicons'.get_icons()

require('todo-comments').setup {
    highlight = {
        pattern = [[(KEYWORDS)\s*(\([^\)]*\))?:]],
    },
}

-- Gruvbox config
vim.g.gruvbox_material_background = 'hard'
vim.g.gruvbox_material_foreground = 'mix'

-- Catppuccin config
require("catppuccin").setup {
  color_overrides = {
    mocha = {
      base = "#000000"
    }
  }
}

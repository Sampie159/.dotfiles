-- Main plugins

require('core.plugin_config.harpoon_config')
require('core.plugin_config.cmp_config')
require('core.plugin_config.telescope_config')
require('core.plugin_config.treesitter_config')
require('core.plugin_config.lsp_config')
require('core.plugin_config.luasnip_config')

vim.g.agua_bold = 0
vim.g.zenbones_darkness = "stark"
vim.g.komau_bold = 0

if vim.g.neovide then
    vim.o.guifont = "IosevkaTerm Nerd Font:h13"
end

--vim.o.background = "light"
vim.cmd.colorscheme("flow")

-- I got tired of creating separate files for each plugin configuration, so I'll
-- just put them all here. I'll try to keep it organized, though.

-- Git stuff
-- Neogit
require('neogit').setup {}
vim.keymap.set("n", "<leader>gn", "<cmd>Neogit<CR>")

-- Lazygit
vim.keymap.set("n", "<leader>gl", "<CMD>LazyGit<CR>")

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
    keywords = {
        TODO = { 
            color = "#ff0000",
            alt = {
                'ToDo',
            },
        },
        HACK = {
            color = "#ff6600",
            alt = {
                'Hack',
            },
        },
        NOTE = {
            color = "#008000",
            alt = {
                'Note',
            },
        },
        FIXME = {
            color = "#f06292",
            alt = {
                'FixMe',
                'Fix',
            },
        },
        WARN = { 
            color = "#ffff00",
            alt = {
                'Warn',
            },
        },
        nocheckin = {
            color = "#ff00ff",
        },
    },
    highlight = {
        pattern = [[\@*(KEYWORDS|keywords)\s*(\([^\)]*\))?:]],
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

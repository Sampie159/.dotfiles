local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable', -- latest stable release
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    -- Todo Comments
    'travisvroman/todo-comments.nvim',

    -- Ouroboros (C/C++ Source/Header Switching)
    'Sampie159/ouroboros.nvim',

    -- Lazygit
    'kdheepak/lazygit.nvim',

    -- Neogit
    {
        "NeogitOrg/neogit",
        dependencies = {
            "sindrets/diffview.nvim",
        },
        config = true,
    },

    -- git-conflict.nvim
    { 'akinsho/git-conflict.nvim', version = "*", config = true },

    -- Harpoon
    {
        'ThePrimeagen/harpoon',
        branch = "harpoon2",
    },

    {
        -- LSP Configuration & Plugins
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs to stdpath for neovim
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',

            { 'j-hui/fidget.nvim', opts = {}, tag = 'legacy' },
        },
    },

    -- Plenary
    'nvim-lua/plenary.nvim',

    {
        -- Autocompletion
        'hrsh7th/nvim-cmp',
        dependencies = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip', 'hrsh7th/cmp-buffer' },
    },

    -- Which key
    { 'folke/which-key.nvim',      opts = {} },

    {
        -- Adds git releated signs to the gutter, as well as utilities for managing changes
        'lewis6991/gitsigns.nvim',
        opts = {
            signs = {
                add = { text = '+' },
                change = { text = '~' },
                delete = { text = '_' },
                topdelete = { text = '‾' },
                changedelete = { text = '~' },
            },
        },
    },

    -- Catppuccin
    {
        "catppuccin/nvim", name = "catppuccin"
    },

    -- Oxocarbon
    "nyoom-engineering/oxocarbon.nvim",

    {
        -- Set lualine as statusline
        'nvim-lualine/lualine.nvim',
        opts = {
            options = {
                icons_enabled = true,
                component_separators = '|',
                section_separators = '',
            },
        },
    },

    -- "gc" to comment visual regions/lines
    { 'numToStr/Comment.nvim',         opts = {} },

    -- Fuzzy Finder
    { 'nvim-telescope/telescope.nvim', version = '*', dependencies = { 'nvim-lua/plenary.nvim' } },
    'nvim-telescope/telescope-ui-select.nvim',

    {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
            return vim.fn.executable 'make' == 1
        end,
    },

    {
        -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
        },
        config = function()
            pcall(require('nvim-treesitter.install').update { with_sync = true })
        end,
    },

    -- TreeSitter Context
    'nvim-treesitter/nvim-treesitter-context',

    -- Web devicons
    'nvim-tree/nvim-web-devicons',

    -- KMonad
    'kmonad/kmonad-vim',

    -- Poimandres
    'olivercederborg/poimandres.nvim',

    -- Gruvbox
    'sainnhe/gruvbox-material',

    -- Parinfer
    'gpanders/nvim-parinfer',

    -- Yuck.vim
    'elkowar/yuck.vim',

    -- nvim-spectre
    'nvim-pack/nvim-spectre',

    -- kanagawa.nvim
    'rebelot/kanagawa.nvim',

    -- undotree
    'mbbill/undotree',

    -- cyberdream
    -- {
    --     'scottmckendry/cyberdream.nvim',
    --     config = function()
    --         require("cyberdream").setup({
    --             transparent = true,
    --             italic_comments = true,
    --             hide_fillchars = true,
    --             borderless_telescope = false,
    --             terminal_colors = true,
    --         })
    --     end,
    -- },

    -- miasma
    'xero/miasma.nvim',

    -- night-owl
    'oxfist/night-owl.nvim',

    -- oil.nvim
    {
        'stevearc/oil.nvim',
        config = function()
            require("oil").setup({
                skip_confirm_for_simple_edits = true,
            })
            vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
        end,
    },

    -- komau
    'ntk148v/komau.vim',

    -- Mies
    'jaredgorski/Mies.vim',

    -- fogbell
    'Sampie159/fogbell.vim',

    -- lsp_signature
    {
        'ray-x/lsp_signature.nvim',
        event = "VeryLazy",
        opts = {},
        config = function(_, opts) require 'lsp_signature'.setup(opts) end,
    },

    -- text-case.nvim
    {
        'johmsalas/text-case.nvim',
        config = function ()
            require('textcase').setup({})
            require('telescope').load_extension('textcase')
        end,
        keys = {
            "ga",
            { "ga.", "<cmd>TextCaseOpenTelescope<CR>", mode = { "n", "x" }, desc = "Telescope" },
        },
        cmd = {
            "Subs",
            "TextCaseOpenTelescope",
            "TextCaseOpenTelescopeQuickChange",
            "TextCaseOpenTelescopeLSPChange",
            "TextCaseStartReplacingCommand",
        },
        lazy = false,
    },

    -- aquarium
    {
        'FrenzyExists/aquarium-vim',
        branch = "develop",
    },

    -- lazydev
    {
        'folke/lazydev.nvim',
        ft = 'lua',
        opts = {
            library = {
                { path = 'luvit-meta/library', words = { 'vim%.uv' } },
            },
        },
    },

    -- luvit (no idea)
    { 'Bilal2453/luvit-meta', lazy = true },

    -- c3
    'Sampie159/c3.vim',

    -- dadbox
    'tpope/vim-dadbod',

    -- sweetie
    "NTBBloodbath/sweetie.nvim",

    -- modus
    "miikanissi/modus-themes.nvim",

    -- flow
    { 
        "0xstepit/flow.nvim",
        config = function()
            require('flow').setup{}
        end,
    },

    -- lush
    "rktjmp/lush.nvim",

    -- tokyonight
    "folke/tokyonight.nvim",

    -- mellifluous
    "ramojus/mellifluous.nvim",

    -- deepwhite
    'Verf/deepwhite.nvim',

    -- minimal
    'Yazeed1s/minimal.nvim',

    -- quickmath
    'jbyuki/quickmath.nvim',

    -- hex.nvim
    {
        'RaafatTurki/hex.nvim',
        config = function()
            require('hex').setup()
        end,
    },

    -- vacme
    'raphael-proust/vacme',

    -- nightfox
    'EdenEast/nightfox.nvim',
}

require('lazy').setup(plugins, {})

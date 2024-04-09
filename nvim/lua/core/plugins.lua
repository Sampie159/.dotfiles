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
    -- Copilot.lua
    {
        'zbirenbaum/copilot.lua',
        cmd = "Copilot",
        event = "InsertEnter",
    },

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

    -- vim-be-good
    'ThePrimeagen/vim-be-good',

    {
        -- LSP Configuration & Plugins
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs to stdpath for neovim
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',

            { 'j-hui/fidget.nvim', opts = {}, tag = 'legacy' },

            'folke/neodev.nvim',
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
    { 'folke/which-key.nvim',          opts = {} },

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
                theme = 'palenight',
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

    -- nvlime
    {
        'monkoose/nvlime',
        dependencies = {
            'monkoose/parsley',
        },
    },

    -- Yuck.vim
    'elkowar/yuck.vim',

    -- mini.surround
    'echasnovski/mini.surround',

    -- Orgmode
    {
        'nvim-orgmode/orgmode',
        event = 'VeryLazy',
    },

    -- Gleam
    'gleam-lang/gleam.vim',

    -- Elixir
    'elixir-editors/vim-elixir',

    -- nvim-spectre
    'nvim-pack/nvim-spectre',

    -- Conjure
    {
        'Olical/conjure',
        dependencies = {
            'PaterJason/cmp-conjure',
            'wlangstroth/vim-racket',
        },
    },

    -- aniseed
    'Olical/aniseed',
}

require('lazy').setup(plugins, {})

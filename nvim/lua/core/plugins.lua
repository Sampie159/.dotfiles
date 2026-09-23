-- Plugins are installed by home-manager (home-manager/nvim.nix); this only sets them up.

-- Git
require('git-conflict').setup()

require('gitsigns').setup {
    signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
    },
}

-- LSP helpers
require('fidget').setup {}
require('lsp_signature').setup {}
require('lazydev').setup {
    library = {
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
    },
}

require('which-key').setup {}

require('Comment').setup()

do
    local elixir = require('elixir')
    local elixirls = require('elixir.elixirls')

    elixir.setup({
        nextls = {
            enable = false,
        },

        elixirls = {
            enable = true,

            settings = elixirls.settings({
                dialyzerEnabled = false,
                enableTestLenses = false,
                fetchDeps = false,

                suggestSpecs = true,
            }),
        },

        projectionist = {
            enable = true,
        },
    })
end

require("oil").setup({
    skip_confirm_for_simple_edits = true,
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

require('textcase').setup({})
require('telescope').load_extension('textcase')
vim.keymap.set({ "n", "x" }, "ga.", "<cmd>TextCaseOpenTelescope<CR>", { desc = "Telescope" })

---@module "compile-mode"
---@type CompileModeOpts
vim.g.compile_mode = {
    input_word_completion = true,
    baleia_setup = true,
    bang_expansion = true,
    default_command = {
        c = "./build.sh",
        cpp = "./build.sh",
        odin = "./build.sh",
        rust = "cargo build",
        zig = "zig build",
        lua = "./build.sh",
        go = "go build",
    },
    focus_compilation_buffer = true,
}

-- Colorschemes
vim.g.gruvbox_material_background = 'hard'
vim.g.gruvbox_material_foreground = 'original'

vim.g.BorlandStyle = 'classic'
vim.g.BorlandParen = 1

require('flow').setup {}

require('darkvoid').setup {
    glow = true,
}

require('neogotham'):setup()

require('evergarden').setup {
    theme = {
        variant = 'winter', -- 'winter'|'fall'|'spring'|'summer'
        accent = 'green',
    },
    editor = {
        transparent_background = false,
        sign = { color = 'none' },
        float = {
            color = 'mantle',
            solid_border = false,
        },
        completion = {
            color = 'surface0',
        },
    },
}

vim.opt.guicursor = {
    "n:block-CursorNormal",
    "v:block-CursorVisual",
    "i:block-CursorInsert",
    "r-cr:block-CursorReplace",
    "c:block-CursorCommand",
}
require('mfd').enable_cursor_sync()

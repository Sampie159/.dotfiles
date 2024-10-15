local lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    client.server_capabilities.semanticTokensProvider = nil
    local nmap = function(keys, func, desc)
        if desc then
            desc = 'LSP: ' .. desc
        end

        vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end
    -- Lesser used LSP functionality
    nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
    nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
    nmap('<leader>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, '[W]orkspace [L]ist Folders')

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
        vim.lsp.buf.format()
    end, { desc = 'Format current buffer with LSP' })
end

vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action)
vim.keymap.set('n', 'gd', vim.lsp.buf.definition)
vim.keymap.set('n', 'gr', require('telescope.builtin').lsp_references)
vim.keymap.set('n', 'gI', vim.lsp.buf.implementation)
-- vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition)
-- vim.keymap.set('n', '<leader>ds', require('telescope.builtin').lsp_document_symbols)
-- vim.keymap.set('n', '<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols)
vim.keymap.set('n', 'gD', vim.lsp.buf.declaration)
vim.keymap.set("n", "<leader>F", function()
    vim.lsp.buf.format()
    vim.api.nvim_command('write')
end)

local servers = {
    lua_ls = {
        Lua = {
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
        },
    },
    rust_analyzer = {
        ['rust-analyzer'] = {
            checkOnSave = {
                command = 'clippy',
            },
        },
    },
    gopls = {
    },
}

-- Go lsp config
lsp.gopls.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
        gopls = {
            gofumpt = true,
            analyses = {
                unusedparams = true,
            },
            staticcheck = true,
        },
    },
}

lsp.asm_lsp.setup {
    cmd = { 'asm-lsp' },
    filetypes = { 'asm', 's', 'vmasm' },
    root_dir = lsp.util.root_pattern('.git'),
    on_attach = on_attach,
    capabilities = capabilities
}

-- C/C++ lsp config
lsp.clangd.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = { 'clangd', '--compile-commands-dir=./debug', '--background-index', '--clang-tidy', '--completion-style=bundled', '--header-insertion=never', '--header-insertion-decorators=0' },
    filetypes = { 'c', 'cpp' },
    init_options = {
        clangdFileStatus = true,
        usePlaceholders = true,
        completeUnimported = true,
        semanticHighlighting = false,
    },
    root_dir = lsp.util.root_pattern('.clangd', '.clang-format', '.clang-tidy', '.clang=format', 'configure.ac',
        'compile_commands.json',
        'compile_flags.txt', '.git'),
}

lsp.ocamllsp.setup {
    cmd = { 'ocamllsp' },
    filetypes = { 'ocaml', 'ocaml.interface', 'ocaml.ocamllex', 'ocaml.menhir' },
    root_dir = lsp.util.root_pattern('dune-project', '.git', 'dune-workspace', '*.opam'),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.zls.setup {
    cmd = { 'zls' },
    filetypes = { 'zig', 'zir' },
    root_dir = lsp.util.root_pattern('.git', 'build.zig', 'zls.json'),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.glsl_analyzer.setup {
    cmd = { "glsl_analyzer" },
    filetypes = { "glsl", "hlsl", "vert", "tesc", "tese", "geom", "frag", "comp", "mesh", "task", "rgen", "rint", "rahit", "rchit", "rmiss", "rcall" },
    root_dir = lsp.util.root_pattern(".git", "compile_commands.json", "compile_flags.txt", ".hg"),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.ols.setup {
    cmd = { "ols", "--stdio" },
    filetypes = { "odin" },
    root_dir = lsp.util.root_pattern(".git", "odinfmt.json", "ols.json"),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.sourcekit.setup {
    cmd = { "sourcekit-lsp" },
    filetypes = { "swift" },
    root_dir = lsp.util.root_pattern("Package.swift")
}

lsp.gleam.setup {}

lsp.racket_langserver.setup {}

lsp.fennel_ls.setup {}

lsp.roc_ls.setup {}

lsp.erlangls.setup {}

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
    function(server_name)
        require('lspconfig')[server_name].setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
        }
    end,
}

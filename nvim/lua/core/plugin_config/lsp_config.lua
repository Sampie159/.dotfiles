local lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    local nmap = function(keys, func, desc)
        if desc then
            desc = 'LSP: ' .. desc
        end

        vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end

    nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
    nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
    nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
    nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
    nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
    nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
    -- nmap('<C-K>', vim.lsp.buf.signature_help, 'Signature Documentation')

    -- Lesser used LSP functionality
    nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
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

local servers = {
    -- clangd = {},
    -- gopls = {},
    -- pyright = {},
    -- rust_analyzer = {},
    -- tsserver = {},

    lua_ls = {
        Lua = {
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
        },
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

-- Rust lsp config
lsp.rust_analyzer.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
        ['rust-analyzer'] = {
            checkOnSave = {
                command = 'clippy',
            },
            cargo = {
                loadOutDirsFromCheck = true,
            },
            procMacro = {
                enable = true,
            },
            rustfmt = {
                extraArgs = { '--emit=stdout' },
            },
            imports = {
                granularity = {
                    group = "module",
                },
                prefix = "self",
            },
        },
    },
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
        semanticHighlighting = true,
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

lsp.hls.setup {
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = { 'haskell', 'lhaskell', },
    root_dir = lsp.util.root_pattern('*.cabal', 'stack.yaml', 'cabal.project', 'package.yaml', 'hie.yaml', '.git'),
    settings = {
        haskell = {
            formattingProvider = "fourmolu",
        }
    },
    single_file_support = true,
    on_attach = on_attach,
}

lsp.glsl_analyzer.setup {
    cmd = { "glsl_analyzer" },
    filetypes = { "glsl", "hlsl", "vert", "tesc", "tese", "geom", "frag", "comp", "mesh", "task", "rgen", "rint", "rahit", "rchit", "rmiss", "rcall" },
    root_dir = lsp.util.root_pattern(".git", "compile_commands.json", "compile_flags.txt", ".hg"),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.gdscript.setup {}

lsp.ols.setup {
    cmd = { "ols", "--stdio" },
    filetypes = { "odin" },
    root_dir = lsp.util.root_pattern(".git", "odinfmt.json", "ols.json"),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.pyright.setup {
    cmd = { "pyright-langserver", "--stdio" },
    filetypes = { "python" },
    root_dir = lsp.util.root_pattern(".git", "requirements.txt", "pyproject.toml", "setup.py", "Pipfile", "pyrightconfig.json"),
    on_attach = on_attach,
    capabilities = capabilities
}

lsp.gleam.setup{}

lsp.racket_langserver.setup{}

lsp.fennel_ls.setup{}

-- Setup neovim lua configuration
require('neodev').setup()

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

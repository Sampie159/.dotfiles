local on_attach = function(client, bufnr)
    -- client.server_capabilities.semanticTokensProvider = nil
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

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- C/C++ lsp config
local clangd_config = {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = {
        'clangd',
        '--background-index',
        '--clang-tidy',
        '--completion-style=bundled',
        '--header-insertion=never',
        '--header-insertion-decorators=0',
        '--experimental-modules-support',
    },
    filetypes = { 'c', 'cpp' },
    init_options = {
        clangdFileStatus = true,
        usePlaceholders = true,
        completeUnimported = true,
        semanticHighlighting = true,
    },
    root_markers = {
        '.clangd', '.clang-format', '.clang-tidy', '.clang=format', 'configure.ac',
        'compile_commands.json',
        'compile_flags.txt', '.git'
    },
}

vim.lsp.config("clangd", clangd_config)

vim.lsp.config("rust_analyzer", {
    settings = {
        ["rust-analyzer"] = {
            check = {
                command = "clippy",
            },
            cargo = {
                allFeatures = true,
            },
            procMacro = {
                enable = true,
            },
        },
    },
})

vim.lsp.config('c3lsp', {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = { 'c3-lsp', '-c3c-path', vim.fn.exepath('c3c') },
    filetypes = { 'c3', },
    root_markers = {
        'project.json'
    },
})
vim.lsp.enable('c3lsp')

vim.lsp.config('hls', {
    settings = {
        haskell = {
            formattingProvider = "stylish-haskell",
        },
    },
})

vim.lsp.config('nixd', {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
        nixd = {
            nixpkgs = {
                expr = 'import (builtins.getFlake "/home/sampie/.dotfiles").inputs.nixpkgs { }',
            },
            formatting = {
                command = { 'nixfmt' },
            },
            options = {
                nixos = {
                    expr = '(builtins.getFlake "/home/sampie/.dotfiles").nixosConfigurations.nixos.options',
                },
                home_manager = {
                    expr = '(builtins.getFlake "/home/sampie/.dotfiles").nixosConfigurations.nixos.options.home-manager.users.type.getSubOptions []',
                },
            },
        },
    },
})

-- Enable every lspconfig server whose binary is on PATH (devenv decides per project).
-- Servers with a function `cmd` (ts_ls, eslint, html, ...) can't be checked; enable those by hand.
local generic = { node = 1, npx = 1, python = 1, python3 = 1, java = 1, dotnet = 1, perl = 1, nc = 1, R = 1, julia = 1, racket = 1, swipl = 1 }
-- Deferred: scanning ~400 configs costs ~50ms; enable() attaches already-open buffers.
vim.schedule(function()
    local names = {}
    for _, file in ipairs(vim.api.nvim_get_runtime_file('lsp/*.lua', true)) do
        local ok, config = pcall(dofile, file)
        local cmd = ok and type(config) == 'table' and config.cmd
        if type(cmd) == 'table' and type(cmd[1]) == 'string' and not generic[cmd[1]] and vim.fn.executable(cmd[1]) == 1 then
            table.insert(names, vim.fn.fnamemodify(file, ':t:r'))
        end
    end
    vim.lsp.enable(names)
end)

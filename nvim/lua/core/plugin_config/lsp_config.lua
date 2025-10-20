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

local servers = {
    lua_ls = {
        Lua = {
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
        },
    },
}

vim.lsp.enable('gopls')

-- C/C++ lsp config
local clangd_config = {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = { 'clangd', '--background-index', '--clang-tidy', '--completion-style=bundled', '--header-insertion=never', '--header-insertion-decorators=0' },
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
vim.lsp.enable('clangd')

vim.lsp.enable('rust_analyzer')

vim.lsp.enable("zls")

vim.lsp.enable('glsl_analyzer')

vim.lsp.enable("ols")

vim.lsp.enable("omnisharp")

vim.lsp.enable('neocmake')

vim.lsp.enable('ocamllsp')

vim.lsp.enable('asm_lsp')

vim.lsp.config('c3lsp', {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = { 'c3lsp', '-c3c-path', '/usr/local/bin/c3c' },
    filetypes = { 'c3', },
    root_markers = {
        'project.json'
    },
})
vim.lsp.enable('c3lsp')

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
    ensure_installed = vim.tbl_keys(servers),
}

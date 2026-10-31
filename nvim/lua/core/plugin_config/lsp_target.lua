vim.api.nvim_create_user_command('LspTarget', function(opts)
    local target = opts.args
    if target ~= '' and target ~= 'linux' and target ~= 'windows' then
        vim.notify('Usage: LspTarget [linux|windows]', vim.log.levels.ERROR)
        return
    end

    local client = vim.lsp.get_clients({ bufnr = 0, name = 'clangd' })[1]
    if not client or not client.initialized then
        vim.notify('Wait for clangd to attach to a C/C++ buffer', vim.log.levels.WARN)
        return
    end

    local current = client.config._lsp_target or 'linux'
    if target == '' then
        target = current == 'linux' and 'windows' or 'linux'
    end
    if target == current then
        vim.notify('clangd target: ' .. target)
        return
    end

    local config = vim.deepcopy(client.config._lsp_linux_config or client.config)
    if target == 'windows' then
        local compiler = vim.fn.exepath('x86_64-w64-mingw32-gcc')
        if compiler == '' then
            vim.notify('Windows target needs x86_64-w64-mingw32-gcc on PATH', vim.log.levels.ERROR)
            return
        end

        config._lsp_linux_config = vim.deepcopy(config)
        config._lsp_target = target
        -- A private user config overrides the target while keeping the project's compile flags.
        local directory = vim.fn.tempname()
        vim.fn.mkdir(directory .. '/clangd', 'p')
        local user_config = ((config.cmd_env or {}).XDG_CONFIG_HOME
            or vim.env.XDG_CONFIG_HOME or (vim.env.HOME .. '/.config')) .. '/clangd/config.yaml'
        local lines = vim.fn.filereadable(user_config) == 1 and vim.fn.readfile(user_config) or {}
        vim.list_extend(lines, { '---', vim.json.encode({
            CompileFlags = {
                Compiler = compiler,
                Remove = { '--target=*', '-target', '--sysroot=*', '-isysroot', '--gcc-toolchain=*' },
            },
        }) })
        vim.fn.writefile(lines, directory .. '/clangd/config.yaml')
        config.cmd_env = vim.tbl_extend('force', config.cmd_env or {}, { XDG_CONFIG_HOME = directory })
        table.insert(config.cmd, '--query-driver=' .. compiler)
    end

    local replacement = vim.lsp.start(config, { reuse_client = function() return false end })
    if not replacement then
        vim.notify('Could not start clangd for ' .. target, vim.log.levels.ERROR)
        return
    end
    for bufnr in pairs(client.attached_buffers) do
        vim.lsp.buf_attach_client(bufnr, replacement)
        vim.lsp.buf_detach_client(bufnr, client.id)
    end
    client:stop()
    vim.notify('clangd target: ' .. target .. ' (current project)')
end, {
    nargs = '?',
    complete = function() return { 'linux', 'windows' } end,
    desc = 'Switch the current clangd session between Linux and Windows; no argument toggles',
})

vim.keymap.set('n', '<leader>lt', '<cmd>LspTarget<cr>', { desc = 'Toggle clangd Linux/Windows target' })

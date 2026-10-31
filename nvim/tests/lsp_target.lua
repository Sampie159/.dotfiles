-- Run from the dotfiles root: nvim --headless -u NONE -l nvim/tests/lsp_target.lua
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
vim.o.hidden = true
require('core.plugin_config.lsp_target')

local directory = vim.fn.tempname()
vim.fn.mkdir(directory .. '/include', 'p')
vim.fn.mkdir(directory .. '/config/clangd', 'p')
vim.fn.writefile({ '#define PROJECT_HEADER 1' }, directory .. '/include/project.h')
vim.fn.writefile({ 'CompileFlags:', '  Add: [-DUSER_FLAG]' }, directory .. '/config/clangd/config.yaml')

local source = {
    '#include "project.h"',
    '#if !defined(PROJECT_FLAG) || !defined(USER_FLAG) || !defined(PROJECT_HEADER)',
    '#error lost project or user configuration',
    '#endif',
    '#ifdef __cplusplus',
    '#include <vector>',
    'std::vector<int> values;',
    '#endif',
    '#ifdef _WIN32',
    '#include <windows.h>',
    'typedef char correct_long_size[sizeof(long) == 4 ? 1 : -1];',
    'void windows_pause(void) { Sleep(1000); }',
    '#else',
    '#include <unistd.h>',
    'typedef char correct_long_size[sizeof(long) == 8 ? 1 : -1];',
    'void linux_pause(void) { sleep(1); }',
    '#endif',
}

local commands, buffers = {}, {}
for _, extension in ipairs({ 'c', 'cpp' }) do
    local file = directory .. '/example.' .. extension
    vim.fn.writefile(source, file)
    table.insert(commands, {
        directory = directory,
        file = file,
        arguments = { vim.fn.exepath('clang'), '-DPROJECT_FLAG', '-I' .. directory .. '/include', '-c', file },
    })
    vim.cmd.edit(vim.fn.fnameescape(file))
    vim.bo.filetype = extension
    table.insert(buffers, vim.api.nvim_get_current_buf())
end
vim.fn.writefile({ vim.json.encode(commands) }, directory .. '/compile_commands.json')

local diagnostics = {}
local config = {
    name = 'clangd',
    cmd = { 'clangd', '--background-index=false', '--clang-tidy=false' },
    cmd_env = { XDG_CONFIG_HOME = directory .. '/config' },
    root_dir = directory,
    handlers = {
        ['textDocument/publishDiagnostics'] = function(_, result, ctx)
            diagnostics[ctx.client_id] = diagnostics[ctx.client_id] or {}
            diagnostics[ctx.client_id][result.uri] = result.diagnostics
        end,
    },
}

local function wait_for_analysis(id, platform)
    assert(vim.wait(30000, function()
        local published = diagnostics[id] or {}
        for _, bufnr in ipairs(buffers) do
            if not published[vim.uri_from_bufnr(bufnr)] then return false end
        end
        return true
    end, 50), platform .. ': timed out waiting for diagnostics')
    local client = assert(vim.lsp.get_client_by_id(id))
    for _, bufnr in ipairs(buffers) do
        local uri = vim.uri_from_bufnr(bufnr)
        for _, diagnostic in ipairs(diagnostics[id][uri]) do
            assert(diagnostic.severity ~= 1, platform .. ': ' .. diagnostic.message)
        end
        local response = assert(client:request_sync('textDocument/documentSymbol', {
            textDocument = { uri = uri },
        }, 10000, bufnr))
        assert(not response.err, vim.inspect(response.err))
        local names = {}
        for _, symbol in ipairs(response.result) do names[symbol.name] = true end
        assert(names[platform .. '_pause'], platform .. ': wrong #ifdef branch')
        assert(not names[(platform == 'windows' and 'linux' or 'windows') .. '_pause'])
        if platform == 'windows' then
            local column = assert(source[12]:find('Sleep', 1, true)) - 1
            local params = {
                textDocument = { uri = uri },
                position = { line = 11, character = column + 2 },
            }
            local completion = assert(client:request_sync('textDocument/completion', params, 10000, bufnr))
            assert(not completion.err, vim.inspect(completion.err))
            local found = false
            for _, item in ipairs(completion.result.items) do
                if (item.filterText or item.label):match('^Sleep') then found = true end
            end
            assert(found, 'no Windows API completion for Sleep')
            local definition = assert(client:request_sync('textDocument/definition', params, 10000, bufnr))
            assert(not definition.err, vim.inspect(definition.err))
            assert(#definition.result > 0, 'no definition for Sleep')
            assert(definition.result[1].uri:match('mingw'), 'definition did not resolve to Windows headers')
        end
    end
    return client
end

local id = assert(vim.lsp.start(config))
vim.lsp.buf_attach_client(buffers[1], id)
local native = wait_for_analysis(id, 'linux')
vim.api.nvim_buf_set_lines(buffers[2], -1, -1, false, { 'int unsaved_edit;' })

local function current_client()
    local client
    assert(vim.wait(30000, function()
        client = vim.lsp.get_clients({ bufnr = 0, name = 'clangd' })[1]
        return client ~= nil
    end, 50), 'clangd did not initialize')
    return client
end

vim.cmd.LspTarget('windows')
local windows = current_client()
assert(windows.id ~= native.id)
wait_for_analysis(windows.id, 'windows')

-- New buffers in the same project reuse the selected session.
local new_file = directory .. '/new.c'
vim.fn.writefile({ 'int new_buffer;' }, new_file)
vim.cmd.edit(vim.fn.fnameescape(new_file))
vim.bo.filetype = 'c'
local new_buffer = vim.api.nvim_get_current_buf()
assert(vim.lsp.start(config) == windows.id)
vim.cmd.LspTarget('windows')
assert(vim.lsp.get_clients({ bufnr = 0, name = 'clangd' })[1].id == windows.id)

vim.cmd.LspTarget('linux')
local linux = current_client()
wait_for_analysis(linux.id, 'linux')
assert(vim.lsp.buf_is_attached(new_buffer, linux.id), 'new buffer was not switched back to Linux')
assert(vim.deep_equal(linux.config.cmd, config.cmd), 'native command was not restored')
assert(vim.deep_equal(linux.config.cmd_env, config.cmd_env), 'native environment was not restored')
assert(vim.bo[buffers[2]].modified, 'unsaved buffer was saved or discarded')
assert(vim.api.nvim_buf_get_lines(buffers[2], -2, -1, false)[1] == 'int unsaved_edit;')

local messages, notify = {}, vim.notify
vim.notify = function(message) table.insert(messages, message) end
local path = vim.env.PATH
vim.env.PATH = ''
vim.cmd.LspTarget('windows')
vim.env.PATH = path
assert(vim.lsp.get_clients({ bufnr = 0, name = 'clangd' })[1].id == linux.id)
assert(messages[#messages]:match('needs x86_64'))
vim.cmd.LspTarget('invalid')
assert(messages[#messages]:match('Usage:'))
vim.notify = notify

vim.cmd.LspTarget() -- No argument toggles; the keybinding uses the same command.
wait_for_analysis(current_client().id, 'windows')
for _, client in ipairs(vim.lsp.get_clients()) do client:stop(true) end
print('PASS: C/C++ target switching, Windows completion/definitions, project flags, C++ headers, new buffers, unsaved edits, missing toolchain')
vim.cmd('qa!')

autocmd! FileType {c,cpp} nnoremap<buffer> <C-e> :Ouroboros<CR>

function! s:insert_header_guard()
    let filename = expand("%:t:r")
    let ext = "_" . toupper(expand("%:t:e"))
    let guard = "_" . toupper(filename) . ext . "_"
    execute "normal! i#ifndef " . guard . "\n#define " . guard . "\n\n\n"
    execute "normal! o#endif // " . guard
    execute "normal! 4gg"
endfunction

function! s:auto_include()
    let ext = substitute("." . expand("%:t:e"), "c", "h", "g")
    let filename = expand("%:t:r")
    if filename != "main"
        execute "normal! i#include \"" . filename . ext . "\"\n\n"
    endif
endfunction

autocmd BufNewFile *.{h,hh,hpp} call <SID>insert_header_guard()
autocmd BufNewFile *.{c,cc,cpp} call <SID>auto_include()

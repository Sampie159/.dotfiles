autocmd! FileType {c,cpp} nnoremap<buffer> <C-e> :Ouroboros<CR>

function! s:insert_header_guard()
    let filename = expand("%:t:r")
    let ext = "_" . toupper(expand("%:t:e"))
    let guard = "_" . toupper(filename) . ext . "_"
    execute "normal! i#if !defined(" . guard . ")\n\n\n\n#define " . guard
    execute "normal! o#endif /* " . guard . " */"
    execute "normal! 3gg"
endfunction

" function! s:auto_include()
"     let filename = expand("%:t:r")
"     let single_h_header = findfile(filename . ".h", ".;")
"     if single_h_header != ""
"         execute "normal! i#include \"" . filename . ".h\"\n\n"
"     else
"         let ext = substitute("." . expand("%:t:e"), "c", "h", "g")
"         if filename != "main"
"             execute "normal! i#include \"" . filename . ext . "\"\n\n"
"         endif
"     endif
" endfunction

autocmd BufNewFile *.{h,hh,hpp} call <SID>insert_header_guard()
" autocmd BufNewFile *.{c,cc,cpp} call <SID>auto_include()

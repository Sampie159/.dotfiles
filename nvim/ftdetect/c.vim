function! s:insert_header_guard()
    let filename = expand("%:t:r")
    let ext = "_" . toupper(expand("%:t:e"))
    let guard = "_" . toupper(filename) . ext . "_"
    execute "normal! i#if !defined(" . guard . ")\n\n\n\n#define " . guard
    execute "normal! o#endif /* " . guard . " */"
    execute "normal! 3gg"
endfunction

autocmd BufNewFile *.{h,hh,hpp} call <SID>insert_header_guard()

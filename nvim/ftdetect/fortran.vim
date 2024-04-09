" function! s:fortran_ident_on_save()
"     let line = line('.')
"     execute "normal! gg=G"
"     execute line
" endfunction
"
" autocmd BufWritePre *.f90 call <SID>fortran_ident_on_save()
autocmd BufRead,BufNew *.f90 let b:fortran_free_source=1
autocmd BufRead,BufNew *.f90 let b:fortran_do_enddo=1
autocmd BufRead,BufNew *.f90 let b:fortran_indent_more=1
autocmd BufRead,BufNew *.f90 let b:fortran_more_precise=1

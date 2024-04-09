set autoindent
filetype on
filetype plugin on
filetype indent on
syntax on
set number
set relativenumber
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set scrolloff=10
set nowrap
set incsearch
set ignorecase
set smartcase
set showcmd
set showmode
set showmatch
set hlsearch
set history=1000
set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx
set termguicolors
set omnifunc=syntaxcomplete#Complete
highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=steelblue
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon0
set guicursor+=i:blinkwait10
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"
let g:loaded_matchparen = 1

" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'prabirshrestha/vim-lsp'

call plug#end()

" }}}

"MAPPINGS --------------------------------------------------------------- {{{

let mapleader = " "

nnoremap n nzz
nnoremap N Nzz
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <leader>bk :bdelete<CR>

nnoremap ;s :%s/\<<C-r><C-w>\>/<C-w>/gI<Left><Left><Left>

nnoremap <leader>x :!chmod +x %<CR>

nnoremap ;g :G<CR>
nnoremap <leader>fga :G add .<CR>
nnoremap <leader>fc :G commit<CR>
nnoremap <leader>fpl :G pull<CR>
nnoremap <leader>fps :G push<CR>

vnoremap <leader>s :sort<CR>
vnoremap <leader>S :sort!<CR>

" }}}

" VIMSCRIPT -------------------------------------------------------------- {{{

" This will enable code folding.
" Use the marker method of folding.
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END

" }}}

" STATUS LINE ------------------------------------------------------------ {{{

set statusline=

set statusline+=\ %F\ %M\ %Y\ %R

set statusline+=%=

set statusline+=\ row:\ %l\ col:\ %c\ percent:\ %p%%

set laststatus=2

" }}}

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" ## added by OPAM user-setup for vim / ocp-indent ## dfe14ae5b37921576aeaa8ae4a7bdc27 ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/home/sampie/.opam/default/share/ocp-indent/vim/indent/ocaml.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line

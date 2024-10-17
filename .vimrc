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
Plug 'Sampie159/fogbell.vim'
Plug 'prabirshrestha/vim-lsp'

call plug#end()

" }}}

"MAPPINGS --------------------------------------------------------------- {{{

let mapleader = " "

nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

nnoremap n nzz
nnoremap N Nzz
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <leader>bk :bdelete!<CR>

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

" }}}

" STATUS LINE ------------------------------------------------------------ {{{

set statusline=

set statusline+=\ %F\ %M\ %Y\ %R

set statusline+=%=

set statusline+=\ row:\ %l\ col:\ %c\ percent:\ %p%%

set laststatus=2

" }}}

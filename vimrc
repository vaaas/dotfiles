syntax on
set background=dark
if has("gui_running")
	colors srcery-drk
	set columns=86
	set guioptions=-m
	set guioptions=-e
	set guioptions=-M
	set guioptions=-T
	set guioptions=-R
	set guifont=Monospace\ 15px
endif
set nocompatible
set backspace=indent,eol,start
set history=50
set ruler
set number
set hlsearch
set nobackup
set swapfile
set directory=$XDG_RUNTIME_DIR
set backupdir=$XDG_RUNTIME_DIR
set undodir=$XDG_RUNTIME_DIR
set wildmenu
set wrap
set linebreak
set breakindent
set tabstop=4
set shiftwidth=4
set list
set listchars=tab:··
set autoindent
set scrolljump=10
set lazyredraw
set cursorline
set ignorecase
set smartcase
set title
set foldmethod=indent
set cc=81
set numberwidth=5
let loaded_netrwPlugin = 1 "fuck netrw

" keyboard mappings
nnoremap <Space> :
" gtk3 file selector!
nnoremap <Leader>o :e `zenity --file-selection --separator=$'\n' --filename=%`<CR>
nnoremap <Leader>w :exe '!firefox %'<CR>
" switching buffers
nnoremap <Leader>n :bn<CR>
nnoremap <Leader>p :bp<CR>
nnoremap <Leader>tp :tabp<CR>
nnoremap <Leader>tn :tabn<CR>
nnoremap <Leader>ls :call ListDir()<CR>
nnoremap <Leader>e :exe 'edit!' getline(".")<CR>
nnoremap <Leader>v "+P
nnoremap <Leader>b :b<space>
nnoremap <Leader>cd :cd `zenity --file-selection --directory --separator=$'\n' -- filename='~'`<CR>
nnoremap <C-j> gj
nnoremap <C-k> gk
nnoremap <C-h> g^
nnoremap <C-l> g$

"functions

function! ListDir()
	:enew
	:read !find -type f
endfunction

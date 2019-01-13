syntax on
set background=dark
if has("gui_running")
	colors jellybeans
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
let loaded_netrwPlugin = 1

" keyboard mappings
noremap <Space> :
noremap j gj
noremap k gk
noremap L $
noremap H ^
noremap J <C-f>
noremap K <C-b>
noremap <A-j> }
noremap <A-k> {
noremap <A-h> B
noremap <A-l> W
noremap <A-o> <C-w>w
noremap <A-O> <C-w>q
inoremap <C-g> <Esc>
noremap <Leader>o :e `zenity --file-selection --separator=$'\n' --filename=%`<CR>
noremap <Leader>w :exe '!firefox %'<CR>
noremap <Leader>n :bn<CR>
noremap <Leader>p :bp<CR>
noremap <Leader>tp :tabp<CR>
noremap <Leader>tn :tabn<CR>
noremap <Leader>ls :call ListDir()<CR>
noremap <Leader>e :exe 'edit!' getline(".")<CR>
noremap <Leader>v "+P
noremap <Leader>b :b<space>
noremap <Leader>cd :cd `zenity --file-selection --directory --separator=$'\n' -- filename='~'`<CR>

"functions

function! ListDir()
	:enew
	:read !find -type f
endfunction

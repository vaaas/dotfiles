syntax on
set background=dark
set t_Co=16
if has("gui_running")
	set columns=86
	set guioptions=-m
	set guioptions=-e
	set guioptions=-M
	set guioptions=-T
	set guioptions=-R
	set guifont=Unifont\ 16px
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
set ignorecase
set smartcase
set title
set foldmethod=indent
set cc=81
set numberwidth=5
set display=lastline
let loaded_netrwPlugin = 1

" keyboard mappings
noremap <Space> :
noremap j gj
noremap k gk
noremap L $
noremap H ^
noremap J <C-f>
noremap K <C-b>
noremap z za
noremap Z zA
noremap <A-j> }
noremap <A-k> {
noremap <A-h> B
noremap <A-l> W
noremap <A-o> <C-w>w
noremap <A-O> <C-w>q
noremap <C-A-o> :only<CR>
noremap ] :bn<CR>
noremap [ :bp<CR>
noremap <A-[> :tabp<CR>
noremap <A-]> :tabn<CR>
noremap w W
noremap W w
noremap e }
noremap E {
noremap b B
noremap B b
noremap x J
noremap <C-l> zt
noremap <Leader>o :e `zenity --file-selection --separator=$'\n' --filename=%`<CR>
noremap <Leader>o :e `zenity --file-selection --separator=$'\n' --filename=%`<CR>
noremap <Leader>v "+P
noremap <Leader>c "+y
noremap <Leader>b :b<space>
noremap <Leader>cd :cd `zenity --file-selection --directory --separator=$'\n' -- filename='~'`<CR>
noremap <Leader>wm :set nonumber<CR>:set cc=0<CR>
noremap <Leader>ref :silent !sxiv -b -- '%:p:h/ref/' &<CR>
noremap <Leader>wc g<C-g>
noremap <Leader>ok ^i✔<Esc>

autocmd FileType markdown normal \wm

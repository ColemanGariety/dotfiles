"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/surround.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
" Plug 'roxma/nvim-yarp'
Plug 'leafgarland/typescript-vim'
" Plug 'liuchengxu/vim-clap'
" Plug 'roxma/vim-hug-neovim-rpc'
" Plug 'joshdick/onedark.vim'
" Plug 'altercation/vim-colors-solarized'
" Plug 'morhetz/gruvbox'
" Plug 'vim-airline/vim-airline'
" Plug 'ddrscott/vim-side-search'
" Plug 'kien/ctrlp.vim'
" Plug 'wincent/command-t'
" Plug 'wincent/command-t', { 'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make' }

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible

set hidden

" Use the system clipboard (Linux)
set clipboard=unnamedplus

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e ~/.vimrc<CR>
nmap <silent> <leader>sv :so ~/.vimrc<CR>

" :W sudo saves the file
" (useful for handling the permission-denied error)
command W w !sudo tee % > /dev/null

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the cursor - when moving vertically using j/k
set so=4

" NERDtree
" map <C-n> :NERDTreeToggle<CR>

" Avoid garbled characters in Chinese language windows OS
let $LANG='en'
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the Wild menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" Show matching brackets when text indicator is over them
set showmatch

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" terminal title
set title

" more performant regexp
set regexpengine=1

" C-b and C-f scroll instead of page
nnoremap <C-b> <C-u>
nnoremap <C-f> <C-d>

set lazyredraw

" Ranger
" nnoremap <C-l> :Ranger<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Grep, Command-T
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use ag over grep
set grepprg=rg\ --nogroup\ --nocolor

nnoremap <c-P> :CommandT<CR>
nnoremap <c-O> :CommandTBuffer<CR>
let g:CommandTAlwaysShowDotFiles = 1
let g:CommandTTraverseSCM = 'pwd'
let g:CommandTCancelMap = '<Esc>'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Colors
set background=dark
" colorscheme onedark
" colorscheme solarized
" colorscheme gruvbox

" Line numbers
set number
highlight LineNr ctermbg=NONE


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Be smart when using tabs ;)
set smarttab

" 1 tab == 2 spaces
set shiftwidth=2
set tabstop=2
set autoindent
set smartindent

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Smart way to move between windows
nnoremap <silent> <A-h>j <C-W>h
nnoremap <silent> <A-j>j <C-W>j
nnoremap <silent> <A-k>k <C-W>k
nnoremap <silent> <A-l>l <C-W>l
function! Altmap(char)
  if has('gui_running') | return ' <A-'.a:char.'> ' | else | return ' <Esc>'.a:char.' '|endif
endfunction
if $TERM == 'rxvt-unicode-256color'&&!has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    autocmd InsertEnter * set timeoutlen=400
    autocmd InsertLeave * set timeoutlen=2000
  augroup END
  execute 'nnoremap <silent>'.Altmap('h').'<C-w>h'
  execute 'nnoremap <silent>'.Altmap('j').'<C-w>j'
  execute 'nnoremap <silent>'.Altmap('k').'<C-w>k'
  execute 'nnoremap <silent>'.Altmap('l').'<C-w>l'
endif

" Close all the buffers
map <leader>ba :bufdo bd<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" <Leader>f{char} to move to {char}
" let g:EasyMotion_do_shade = 0
map F <Plug>(easymotion-F)
map f <Plug>(easymotion-f)
map T <Plug>(easymotion-T)
map t <Plug>(easymotion-t)
let g:EasyMotion_off_screen_search = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
map 0 ^

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <C-j> mz:m+<cr>`z
nmap <C-k> mz:m-2<cr>`z
vmap <C-j> :m'>+<cr>`<my`>mzgv`yo`z

vmap <C-k> :m'<-2<cr>`>my`<mzgv`yo`z

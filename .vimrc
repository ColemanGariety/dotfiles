"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'vim-scripts/surround.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
" Plug 'roxma/nvim-yarp'
" Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
" Plug 'liuchengxu/vim-clap'
Plug 'prabirshrestha/vim-lsp'
" Plug 'maxmellon/vim-jsx-pretty'
" Plug 'jparise/vim-graphql'
Plug 'neoclide/coc.nvim' , { 'branch' : 'release' }
" Plug 'rescript-lang/vim-rescript'

Plug 'ntk148v/vim-horizon'

" Plug 'roxma/vim-hug-neovim-rpc'
" Plug 'joshdick/onedark.vim'
" Plug 'altercation/vim-colors-solarized'
" Plug 'morhetz/gruvbox'
" Plug 'vim-airline/vim-airline'
" Plug 'ddrscott/vim-side-search'
" Plug 'kien/ctrlp.vim'
" Plug 'wincent/command-t'
" Plug 'wincent/command-t', { 'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make' }
" Plug 'Shougo/unite.vim'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible

set shortmess=at

set hidden

" Use the system clipboard (Linux)
set clipboard=unnamedplus

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype plugin on
filetype indent on

set foldmethod=marker

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
nnoremap <SPACE> <Nop>
let mapleader = " "

" :W sudo saves the file
" (useful for handling the permission-denied error)
command W w !sudo tee % > /dev/null

" Set CWD to to that of current file
set autochdir

set timeoutlen=1000 ttimeoutlen=0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the cursor - when moving vertically using j/k
set so=4

" Turn on the Wild menu
set wildmenu

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set nohlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw
set ttyfast

" x line
set cursorline

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

" Horizon
set termguicolors
colorscheme horizon
" let g:lightline = {'colorscheme' : 'horizon'}

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

set expandtab

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
" nnoremap <silent> <A-h>j <C-W>h
" nnoremap <silent> <A-j>j <C-W>j
" nnoremap <silent> <A-k>k <C-W>k
" nnoremap <silent> <A-l>l <C-W>l
" function! Altmap(char)
"   if has('gui_running') | return ' <A-'.a:char.'> ' | else | return ' <Esc>'.a:char.' '|endif
" endfunction
" if $TERM == 'rxvt-unicode-256color'&&!has('gui_running')
"   set ttimeoutlen=10
"   augroup FastEscape
"     autocmd!
"     autocmd InsertEnter * set timeoutlen=400
"     autocmd InsertLeave * set timeoutlen=2000
"   augroup END
"   execute 'nnoremap <silent>'.Altmap('h').'<C-w>h'
"   execute 'nnoremap <silent>'.Altmap('j').'<C-w>j'
"   execute 'nnoremap <silent>'.Altmap('k').'<C-w>k'
"   execute 'nnoremap <silent>'.Altmap('l').'<C-w>l'
" endif

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
map <Leader>F <Plug>(easymotion-F)
map <Leader>f <Plug>(easymotion-f)
map <Leader>T <Plug>(easymotion-T)
map <Leader>t <Plug>(easymotion-t)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" " map <Leader> <Plug>(easymotion-prefix)
let g:EasyMotion_off_screen_search = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
" map 0 ^

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <C-j> mz:m+<cr>`z
nmap <C-k> mz:m-2<cr>`z
vmap <C-j> :m'>+<cr>`<my`>mzgv`yo`z

vmap <C-k> :m'<-2<cr>`>my`<mzgv`yo`z

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Completions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:coc_global_extensions = [ 'coc-tsserver' ]

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
	" Recently vim can merge signcolumn and number column into one
	set signcolumn=number
else
	set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"


function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Cursor Shape (Gnome and Kitty)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" NOTE: Changed instances of "redraw!" to "redraw" (no exclamation mark)
" because it is jarring if the screen clears when switching modes.
if has("autocmd")
  au VimEnter,InsertLeave * silent execute '!echo -ne "\e[2 q"' | redraw
  au InsertEnter,InsertChange *
    \ if v:insertmode == 'i' | 
    \   silent execute '!echo -ne "\e[6 q"' | redraw |
    \ elseif v:insertmode == 'r' |
    \   silent execute '!echo -ne "\e[4 q"' | redraw |
    \ endif
  au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERD
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let NERDSpaceDelims=1

"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/otakumesi/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/Users/otakumesi/.cache/dein')
  call dein#begin('/Users/otakumesi/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/Users/otakumesi/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/denite.nvim')
  call dein#add('tomasr/molokai')
  call dein#add('osyo-manga/vim-anzu')
  call dein#add('Shougo/neocomplcache.vim')
  call dein#add('tpope/vim-pathogen')
  call dein#add('vim-syntastic/syntastic')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/deol.nvim', { 'rev': 'a1b5108fd' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

filetype plugin indent on
syntax enable
set number

set fenc=utf-8
set noswapfile
set autoread
set hidden
set showcmd
set cursorline
set cursorcolumn
set virtualedit=onemore
set smartindent
set visualbell
set showmatch
set laststatus=2
set wildmode=list:longest
set clipboard+=unnamedplus

nnoremap j gj
nnoremap k gk
noremap <C-j> <esc>
noremap! <C-j> <esc>
noremap <C-x><C-f> <esc>:Denite file<Enter>

set expandtab
set shiftwidth=2
set tabstop=2

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
noremap <esc><esc> :nohlsearch<cr><esc>

"NeoComplete
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_min_syntax_length = 3
let g:complcache_dictionary_filetype_lists = {'default' : '', 'vimshell' : $HOME.'/.vimshell_hist'}

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Syntastic
execute pathogen#infect()
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

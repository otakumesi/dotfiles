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
  call dein#add('koron/nyancat-vim')
  call dein#add('nefo-mi/nyan-modoki.vim')

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
set encoding=utf-8
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
set nostartofline
set matchtime=1

" statusline
set statusline=%F
set statusline+=%m
set statusline+=%r
set statusline+=[%{&fileencoding}]
set statusline+=(%l/%L)

nnoremap j gj
nnoremap k gk

" replace ESC
noremap <C-g> <esc>
noremap! <C-g> <esc>
noremap <C-h> <esc>
noremap! <C-h> <esc>
noremap <C-j> <esc>
noremap! <C-j> <esc>
noremap <C-k> <esc>
noremap! <C-k> <esc>
noremap <C-l> <esc>
noremap! <C-l> <esc>

" remap Denite
noremap <C-x><C-x> <esc>:Denite 
noremap <C-x><C-f> <esc>:DeniteBufferDir file_rec<Enter>
noremap <C-x><C-r> <esc>:Denite file_old<Enter>
noremap <C-x><C-b> <esc>:Denite buffer<Enter>
noremap <C-x><C-t> <esc>:Denite tab<Enter>
noremap <C-x><C-g> <esc>:DeniteBufferDir grep<Enter>
noremap <C-x>pg <esc>:DeniteProjectDir grep<Enter>
noremap <C-x><C-y> <esc>:Denite neoyank

" remap split window
noremap <C-x>0 <esc>:close<Enter>
noremap <C-x>2 <esc>:split<Enter>
noremap <C-x>3 <esc>:vsplit<Enter>

set expandtab
set shiftwidth=2
set tabstop=2

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
noremap <esc><esc> :nohlsearch<cr><esc>

set autowrite
autocmd CursorHold *  wall
autocmd CursorHoldI *  wall

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

" Denite.vim
if executable('rg')
  call denite#custom#var('file_rec', 'command',
        \ ['rg', '--files', '--glob', '!.git'])
  call denite#custom#var('grep', 'command', ['rg'])
endif

" golang
au BufWritePost *.go !goimports -w % call s:ExecuteEnter()

" Nyancat
set laststatus=2
set statusline+=%{g:NyanModoki()}
let g:nyan_modoki_select_cat_face_number = 1
let g:nayn_modoki_animation_enabled= 1

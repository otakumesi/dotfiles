"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=$HOME/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state($HOME.'/.cache/dein')
  call dein#begin($HOME.'/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add($HOME.'/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/denite.nvim')
  call dein#add('Shougo/neomru.vim')
  call dein#add('Shougo/neoyank.vim')
  call dein#add('osyo-manga/vim-anzu')
  call dein#add('haya14busa/vim-migemo')
  call dein#add('Shougo/deoplete.nvim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
  call dein#add('tpope/vim-pathogen')
  call dein#add('vim-syntastic/syntastic')
  call dein#add('koron/nyancat-vim')
  call dein#add('nefo-mi/nyan-modoki.vim')
  call dein#add('tpope/vim-bundler')
  call dein#add('tpope/vim-dispatch')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-rails')
  call dein#add('slim-template/vim-slim')
  call dein#add('thoughtbot/vim-rspec')
  call dein#add('thinca/vim-ref')
  call dein#add('jiangmiao/auto-pairs')
  call dein#add('tpope/vim-fugitive')
  " call dein#add('scrooloose/nerdtree')
  call dein#add('zchee/deoplete-go', {'build': 'make'})
  call dein#add('twitvim/twitvim')
  call dein#add('fatih/vim-go')
  call dein#add('rhysd/vim-goyacc')
  call dein#add('jodosha/vim-godebug')
  call dein#add('cocopon/vaffle.vim')
  call dein#add('ekalinin/Dockerfile.vim')
  call dein#add('hashivim/vim-terraform')
  call dein#add('juliosueiras/vim-terraform-completion')
  call dein#add('cespare/vim-toml')

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
set spelllang=en,cjk
set spell
set fenc=utf-8
set noswapfile
set autoread
set autowrite
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

set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
autocmd InsertLeave * set nopaste

" molokai
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai

" statusline
set statusline=%F
set statusline+=%m
set statusline+=%r
set statusline+=[%{&fileencoding}]
set statusline+=(%l/%L)
set statusline+=%{fugitive#statusline()}

" keymap
nnoremap j gj
nnoremap k gk
nnoremap <C-s> <ESC>:%s/

" replace ESC
noremap <C-g> <ESC>
noremap! <C-g> <ESC>
inoremap <silent>jj <ESC>

" remap move
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <M-h> <Left>
inoremap <M-l> <Right>

" remap Denite
noremap <C-x><C-x> <ESC>:Denite 
noremap <C-x><C-f> <ESC>:DeniteProjectDir file_rec<Enter>
noremap <C-x><C-r> <ESC>:Denite file_old<Enter>
noremap <C-x><C-b> <ESC>:Denite buffer<Enter>
noremap <C-x><C-t> <ESC>:Denite tab<Enter>
noremap <C-x><C-g> <ESC>:DeniteBufferDir grep<Enter>
noremap <C-x><C-p> <ESC>:DeniteProjectDir grep<Enter>
noremap <C-x><C-y> <ESC>:Denite neoyank<Enter>

" remap split window
noremap <C-x>0 <ESC>:close<Enter>
noremap <C-x>- <ESC>:split<Enter>
noremap <C-x><Bar> <ESC>:vsplit<Enter>

set expandtab
set shiftwidth=2
set tabstop=2

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
noremap <ESC><ESC> :nohlsearch<cr><ESC>

set autowrite
autocmd CursorHold *  wall
autocmd CursorHoldI *  wall

" deoplete
let g:deoplete#enable_at_startup = 1

" Syntastic:
execute pathogen#infect()
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1

" Denite:
if executable('rg')
  call denite#custom#var('file_rec', 'command', ['rg', '--files', '--glob', '!.git'])
  call denite#custom#var('grep', 'command', ['rg'])
  call denite#custom#map('insert', '<C-g>', '<denite:enter_mode:normal>', 'noremap')
  call denite#custom#map('normal', '<C-g>', '<denite:quit>', 'noremap')
  call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
  call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')
  call denite#custom#map('insert', '<C-x>3', '<denite:do_action:split>', 'noremap')
  call denite#custom#map('insert', '<C-x>2', '<denite:do_action:vsplit>', 'noremap')
endif

" Shell:
" autocmd BufNewFile,BufRead .aliases,.functions,.profile SetFileTypeSH("bash")

" Golang:
if $GOPATH != ''
  execute "set rtp+=".globpath($GOPATH, "src/github.com/golang/lint/misc/vim")
  let g:syntastic_go_checkers = ['go', 'golint', 'govet']
endif

let g:go_highlight_types = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1
let g:go_auto_sameids = 1
let g:go_fmt_command = 'goimports'
let g:go_addtags_transform = 'snakecase'
let g:go_snippet_engine = 'neosnippet'
let g:go_list_type = 'quickfix'

autocmd BufNewFile,BufRead *.go setlocal tabstop=4 shiftwidth=4 noexpandtab 

augroup Go
  autocmd!
  autocmd BufWritePre *.go GoImports
augroup END

" Ruby:
" TODO
let g:syntastic_php_checkers = ['ruby', 'rubocop']
autocmd Filetype ruby setlocal ts=2 sw=2 expandtab

" PHP:
" TODO
let g:syntastic_php_checkers = ['php']
autocmd Filetype php setlocal ts=4 sw=4 expandtab

" Python:
" TODO

" JavaScript:
let g:syntastic_javascript_checkers = ['eslint']
autocmd Filetype javascript setlocal ts=2 sw=2 expandtab

" Terraform:
let g:terraform_align=1
let g:terraform_fold_sections=1
let g:terraform_remap_spacebar=1
autocmd Filetype terraform setlocal ts=4 sw=4 expandtab

" Makefile:
autocmd BufNewFile,BufRead Makefile setlocal tabstop=4 shiftwidth=4 noexpandtab 

" Nyancat:
set laststatus=2
set statusline+=%{g:NyanModoki()}
let g:nyan_modoki_select_cat_face_number = 1
let g:nayn_modoki_animation_enabled= 1

" Anzu:
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)
nmap <C-g><C-g> <Plug>(anzu-clear-search-status)
set statusline=%{anzu#search_status()}

" NERDTree:
" nnoremap <C-x><C-w> :NERDTreeToggle<Enter>

" Vaffle
nnoremap <C-x><C-w> :Vaffle<Enter>

" NeoSnippet:
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

if has('conceal')
  set conceallevel=2 concealcursor=i
endif

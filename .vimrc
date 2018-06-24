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

  call dein#add('Shougo/denite.nvim')
  call dein#add('Shougo/vimproc.vim', {'build' : 'make'})

  " Add or remove your plugins here:
  " Common
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
  call dein#add('Shougo/deol.nvim')
  call dein#add('prabirshrestha/async.vim')

  " IDEnize
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('SirVer/ultisnips')
  call dein#add('Shougo/neomru.vim')
  call dein#add('Shougo/neoyank.vim')
  call dein#add('thinca/vim-quickrun')
  call dein#add('sbdchd/neoformat')
  call dein#add('vim-scripts/gtags.vim')
  call dein#add('tpope/vim-surround')
  " call dein#add('Shougo/echodoc.vim')
  call dein#add('cocopon/vaffle.vim')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-pathogen')
  call dein#add('vim-syntastic/syntastic')
  call dein#add('terryma/vim-multiple-cursors')
  call dein#add('jiangmiao/auto-pairs')
  call dein#add('osyo-manga/vim-anzu')
  call dein#add('vim-vdebug/vdebug')
  call dein#add('editorconfig/editorconfig-vim')

  " Productivity
  call dein#add('wakatime/vim-wakatime')
  call dein#add('rhysd/accelerated-jk')
  call dein#add('rizzatti/dash.vim')
  call dein#add('haya14busa/vim-migemo')
  call dein#add('thinca/vim-ref')
  call dein#add('Rykka/clickable.vim')
  call dein#add('Rykka/riv.vim')

  " Ruby
  call dein#add('tpope/vim-dispatch')
  call dein#add('tpope/vim-bundler')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-rails')
  call dein#add('5t111111/denite-rails')
  call dein#add('slim-template/vim-slim')
  call dein#add('thoughtbot/vim-rspec')
  call dein#add('Shougo/deoplete-rct')

  " Golang
  call dein#add('fatih/vim-go')
  call dein#add('rhysd/vim-goyacc')
  call dein#add('zchee/deoplete-go', {'build': 'make'})
  call dein#add('jodosha/vim-godebug')

  " JavaScript
  call dein#add('neovim/node-host', { 'build': 'npm install' })
  call dein#add('billyvg/tigris.nvim', { 'build': './install.sh' })
  call dein#add('leafgarland/typescript-vim')
  call dein#add('Quramy/tsuquyomi')
  call dein#add('mxw/vim-jsx')
  call dein#add('posva/vim-vue')
  call dein#add('styled-components/vim-styled-components')

  " HTML / CSS
  call dein#add('mattn/emmet-vim')
  call dein#add('ap/vim-css-color')

  " Clojure
  call dein#add('tpope/vim-fireplace')
  call dein#add('tpope/vim-salve')
  call dein#add('guns/vim-clojure-static')
  call dein#add('clojure-vim/vim-cider')

  " Markdown
  call dein#add('plasticboy/vim-markdown')
  call dein#add('godlygeek/tabular')

  " VimScript
  call dein#add('junegunn/vader.vim')

  " SettingFiles
  call dein#add('pearofducks/ansible-vim')
  call dein#add('cespare/vim-toml')
  call dein#add('https://raw.githubusercontent.com/google/protobuf/master/editors/proto.vim', {'script_type' : 'protobuf'})
  call dein#add('hashivim/vim-terraform')
  call dein#add('ekalinin/Dockerfile.vim')
  call dein#add('aklt/plantuml-syntax')
  call dein#add('juliosueiras/vim-terraform-completion')

  " Python
  call dein#add('nvie/vim-flake8')
  call dein#add('Vimjas/vim-python-pep8-indent')
  call dein#add('Glench/Vim-Jinja2-Syntax')
  call dein#add('zchee/deoplete-jedi')
  call dein#add('tweekmonster/django-plus.vim')
  
  " etc
  call dein#add('kristijanhusak/vim-carbon-now-sh')
  call dein#add('nathanaelkane/vim-indent-guides')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('twitvim/twitvim')

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
if has('conceal')
  set conceallevel=0
endif
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

filetype plugin indent on

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
set showmatch
set ambiwidth=double
set autochdir
set matchpairs+=<:>

set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
au InsertLeave * set nopaste
" au BufWritePre * :set binary binary | set noeol

" Molokai:
" let g:molokai_original = 1
" let g:rehash256 = 1
" colorscheme molokai

" Solarized:
let g:solarized_termcolors=256
set background=light
colorscheme solarized

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
vnoremap <C-s> <ESC>:'<,'>s/

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
noremap <C-x><C-d><C-g> <ESC>:DeniteBufferDir grep<Enter>
noremap <C-x><C-p><C-g> <ESC>:DeniteProjectDir grep<Enter>
noremap <C-x><C-y> <ESC>:Denite neoyank<Enter>

" remap split window
noremap <C-x>0 <ESC>:close<Enter>
noremap <C-w>x <ESC>:close<Enter>
noremap <C-w>- <ESC>:split<Enter>
noremap <C-w><Bar> <ESC>:vsplit<Enter>

set expandtab
set shiftwidth=2
set tabstop=2

" for echomode
set noshowmode

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
noremap <ESC><ESC> :nohlsearch<cr><ESC>

set autowrite
au CursorHold *  wall
au CursorHoldI *  wall

" Deoplete:
let g:deoplete#enable_at_startup = 1
set completeopt+=noinsert
call deoplete#enable_logging('DEBUG', $HOME.'/deoplete.log')
call deoplete#custom#source('jedi', 'is_debug_enabled', 1)

" Syntastic:
execute pathogen#infect()
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_wq = 0

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
au BufNewFile,BufRead .aliases,.functions,.profile setlocal syntax=sh

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
" let g:go_fmt_command = 'goimrpots'
let g:go_addtags_transform = 'snakecase'
" let g:go_snippet_engine = 'neosnippet'
let g:go_list_type = 'quickfix'

au BufNewFile,BufRead *.go setlocal tabstop=4 shiftwidth=4 noexpandtab 

augroup Go
  autocmd!
  au BufWritePre *.go GoImports
augroup END

" Ruby:
let g:syntastic_php_checkers = ['ruby', 'rubocop']
let g:neoformat_enabled_ruby = ['rubocop']
augroup ruby
  autocmd!
  au Filetype ruby,slim setlocal ts=2 sw=2 expandtab
augroup END

" PHP:
let g:syntastic_php_checkers = ['php']
au Filetype php setlocal ts=4 sw=4 expandtab

" Python:
let g:syntastic_python_flake8_exec = 'python3'
let g:syntastic_python_flake8_args = ['-m', 'flake8']
let g:neoformat_enabled_python = ['autopep8', 'yapf']
let g:python3_host_prog = $HOME.'/.pyenv/shims/python3'
let g:flake8_show_in_file = 1
augroup python
  autocmd!
  au Filetype python setlocal ts=4 sw=4 sts=4 expandtab
augroup END

" Sphinx:
let g:syntastic_rst_checkers = ['sphinx']

" JavaScript:
let g:syntastic_enable_javascript_checkers = 1
let g:syntastic_javascript_checkers = ['eslint']
let g:neoformat_enabled_javascript = ['prettier']
augroup javascript
  autocmd!
  au BufNewFile,BufRead .eslintrc,.babelrc setfiletype javascript
  au Filetype javascript setlocal ts=2 sw=2 expandtab
  au BufWritePre *.js Neoformat
augroup END

" TypeScript:
let g:syntastic_enable_typescript_checkers = 1
let g:tsuquyomi_disable_quickfix = 1
let g:syntastic_typescript_checkers = ['tsuquyomi']
let g:tsuquyomi_completion_detail = 1
augroup typescript
  autocmd!
  au BufNewFile,BufRead *.tsx setfiletype typescript.jsx
augroup END

" Scala:
augroup Scala
  autocmd!
  autocmd BufWritePost *.scala silent :EnTypeCheck
  nnoremap <localleader>t :EnType<CR>
augroup END

" Terraform:
let g:terraform_align = 1
let g:terraform_fold_sections = 1
let g:terraform_remap_spacebar = 1
augroup terraform
  autocmd!
  au Filetype terraform setlocal ts=4 sw=4 expandtab
augroup END

" MISC:
augroup Apache
  autocmd!
  au BufRead,BufNewFile /etc/httpd/conf/*,/etc/httpd/conf.d/* set filetype=apache
augroup END

" let g:neoformat_enabled_css = ['prettier']
let g:neoformat_enabled_json = ['prettier']
let g:neoformat_enabled_yaml = ['pyaml']
au BufNewFile,BufRead Makefile setlocal tabstop=4 shiftwidth=4 noexpandtab

" Ansible:
" let g:neoformat_enabled_ansible = ['pyaml']
" augroup ansible
"   autocmd!
"   au BufRead,BufNewFile */provisioning/*.yml set filetype=ansible
" augroup END

" Markdown:
let g:vim_markdown_folding_disabled = 1

"HTML:
" Jinjaだとtidyがうまく動かないためpassiveにしておく
let g:syntastic_mode_map = {'mode':'active',
      \'passive_filetypes':['html']}

" Anzu:
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)
nmap <C-g><C-g> <Plug>(anzu-clear-search-status)
set statusline=%{anzu#search_status()}

" Vaffle:
nnoremap <C-x><C-w> :Vaffle <Enter>
let g:vaffle_show_hidden_files = 1
let g:vaffle_auto_cd = 1

" NeoSnippet:
" imap <C-a> <Plug>(Neosnippet_expand_or_jump)
" smap <C-a> <Plug>(neosnippet_expand_or_jump)
" xmap <C-a> <Plug>(neosnippet_expand_or_jump)

" Ultisnips:
let g:UltiSnipsExpandTrigger       = "<tab>"
let g:UltiSnipsJumpForwardTrigger  = "<C-a>"
" let g:UltiSnipsJumpBackwardTrigger = "<c-p>"
" let g:UltiSnipsListSnippets        = "<c-k>"

" IME:
if executable('swim')
  let s:AsciiIM = 'com.apple.keyboardlayout.all'

  function! s:insertLeave()
      call system('swim use ' . s:AsciiIM)
      call system('swim list --current')
  endfunction

  augroup ime
    autocmd!
    au InsertLeave * call s:insertLeave()
  augroup END
endif

" EchoDoc:
" let g:echodoc#enable_at_startup = 1

" Emmet:
let g:user_emmet_leader_key = '<C-c>'
let g:user_emmet_install_global = 0
au FileType html,css,javascript,typescript,scss,slim,jade,vue,jinja.html EmmetInstall

" AcceleratedJk:
nmap j <Plug>(accelerated_jk_gj)
nmap k <Plug>(accelerated_jk_gk)

" PlantUML:
let g:plantuml_executable_script = "plantuml -tsvg $@"

" IndentGuide:
let g:indent_guides_enable_on_vim_startup = 1

" Gtags:
map <C-x><C-t> :GtagsCursor<Enter>
map <C-n> :cn<Enter>
map <C-p> :cp<Enter>

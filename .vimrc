if has('nvim')
  if &compatible
    set nocompatible
  endif

  augroup MySettings
    autocmd!
  augroup END
  
  set runtimepath+=$HOME/.cache/dein/repos/github.com/Shougo/dein.vim
  set inccommand=split
  
  if dein#load_state($HOME.'/.cache/dein')
    call dein#begin($HOME.'/.cache/dein')
  
    call dein#add($HOME.'/.cache/dein/repos/github.com/Shougo/dein.vim')
    call dein#load_toml($HOME.'/.dein.toml', {'lazy': 0})
    call dein#load_toml($HOME.'/.dein.lazy.toml', {'lazy': 1})
  
    " Ocaml:
    if executable('opam')
      let g:opamshare = substitute(system('opam config var share'), '\n$', '', '''')
      call dein#add(g:opamshare . '/merlin/vim', {'lazy': 1, 'on_ft': 'ocaml', 'on_event': 'InsertEnter'})
    end
  
    " Golang:
    if $GOPATH != ''
      call dein#add(globpath($GOPATH, "src/github.com/golang/lint/misc/vim"), {'lazy': 1, 'on_ft': 'go', 'on_event': 'InsertEnter'})
      let g:syntastic_go_checkers = ['go', 'golint', 'govet']
    endif
  
    call dein#end()
    call dein#save_state()
  endif
  
  if dein#check_install()
    call dein#install()
  endif
end

  if has('conceal')
    set conceallevel=0
  endif
syntax enable
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
set cmdheight=2
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
set ambiwidth=double
set autochdir
set matchpairs+=<:>

set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
au InsertLeave * set nopaste

" statusline
set statusline=%F
set statusline+=%m
set statusline+=%r
set statusline+=[%{&fileencoding}]
set statusline+=(%l/%L)

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

" remap split window
noremap <C-x>0 <ESC>:close<CR>
noremap <C-w>x <ESC>:close<CR>
noremap <C-w>- <ESC>:split<CR>
noremap <C-w><Bar> <ESC>:vsplit<CR>

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
au CursorHold * wall
au CursorHoldI * wall

" Shell:
au BufNewFile,BufRead .aliases,.functions,.profile setlocal syntax=sh

" Golang:
let g:go_highlight_types = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1
let g:go_auto_sameids = 1
let g:go_fmt_command = 'gofmt'
let g:go_addtags_transform = 'snakecase'
let g:go_snippet_engine = 'neosnippet'
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
let g:python3_host_prog = substitute(system('pyenv which python2'), '\n$', '', '''')
let g:python3_host_prog = substitute(system('pyenv which python3'), '\n$', '', '''')
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
  " au BufWritePre *.js Neoformat
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

" OCaml:
let g:syntastic_ocaml_checkers = ['merlin']

au BufNewFile,BufRead ocaml setlocal tabstop=2 shiftwidth=2 noexpandtab, g:deoplete#complete_method="complete"

" Rust:
let g:rustfmt_autosave = 1

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

" PluginTest:
command! -bang -nargs=* PluginTest call PluginTest(<bang>0, <q-args>)
function! PluginTest(is_gui, extraCommand)
  let cmd = a:is_gui ? 'gvim' : 'vim'
  let extraCommand = empty(a:extraCommand) ? '' : ' -c"au VimEnter * ' . a:extraCommand . '"'
  execute '!' . cmd . ' -u NONE -i NONE -N --cmd "set rtp+=' . getcwd() . '"' . extraCommand
endfunction

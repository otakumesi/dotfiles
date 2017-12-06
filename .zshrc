export GOROOT_BOOTSTRAP=$HOME/go1.4/bin
export GOPATH=$HOME/goprojects
export PATH=$GOPATH/bin:$HOME/.rbenv/shims:$HOME/.rbenv/bin:$GOPATH/bin:$HOME/.meteor:$HOME/.cargo/bin:$PATH
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export CARGO_HOME=$HOME/.cargo

eval "$(rbenv init -)"
eval "$(direnv hook zsh)"
export ZSH=$HOME/.oh-my-zsh
plugins=(git ssh-agent ruby gem zsh-syntax-highlighting)

ZSH_THEME="agnoster"

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"

COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"
XDG_CONFIG_HOME=$HOME

plugins=(git)
source $ZSH/oh-my-zsh.sh
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export EDITOR='nvim'

alias zshconfig="nvim ~/.zshrc"

source ~/.aliases
source ~/.functions

if [ $SHLVL = 1 ]; then
  tmux
fi

autoload colors
colors

setopt no_beep
setopt no_nomatch
setopt prompt_subst
setopt transient_rprompt
setopt hist_ignore_dups
setopt auto_cd
setopt auto_pushd
setopt auto_list
setopt auto_menu
setopt mark_dirs
setopt auto_param_keys
setopt always_last_prompt
setopt extended_glob
setopt globdots
setopt complete_in_word

source ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
eval `gdircolors ~/dotfiles/dircolors/dircolors.256dark`

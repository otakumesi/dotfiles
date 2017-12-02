export PATH=$HOME/.rbenv/shims:$HOME/.rbenv/bin:$GOPATH/bin:$HOME/.meteor:$HOME/.cargo/bin:$PATH
export GOPATH=$HOME/projects/golang:$HOME/goprojects
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

# export CARGO_HOME=$HOME/.cargo
eval "$(rbenv init -)"
eval "$(direnv hook zsh)"
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="agnoster"

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"

COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"

plugins=(git)
source $ZSH/oh-my-zsh.sh
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export EDITOR='nvim'

alias zshconfig="nvim ~/.zshrc"


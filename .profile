export LANG=en_US.UTF-8
export EDITOR='nvim'

export XDG_CONFIG_HOME=$HOME

export JAVA_HOME=`/usr/libexec/java_home -v 11`
export CARGO_HOME=$HOME/.cargo
export GOPATH=$HOME/goprojects
export PYENV_ROOT="$HOME/.pyenv"
export PATH=/usr/local/bin:$GOPATH/bin:$HOME/.rbenv/shims:$PYENV_ROOT/bin:$PATH:$HOME/.rbenv/bin:$HOME/.meteor:$CARGO_HOME/bin:$HOME/.tfenv/bin:$JAVA_HOME/bin:$PATH:$HOME/.ndenv/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"

export GOROOT_BOOTSTRAP=$HOME/go1.4/bin

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export TMUX_POWERLINE_SEG_WEATHER_LOCATION="28677518"

export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

export FZF_TMUX=1
export FZF_TMUX_HEIGHT=30

export NVIM_PYTHON_LOG_FILE=/tmp/log
export NVIM_PYTHON_LOG_LEVEL=DEBUG

eval "$(nodenv init -)"
eval "$(rbenv init -)"
eval "$(pyenv init -)"
eval "$(pipenv --completion)"
eval "$(direnv hook zsh)"

if [ $SHLVL = 1 ]; then
  tmux new-session -s $(basename $(pwd))
fi

alias kc=kubectl

source $HOME/.aliases
source $HOME/.functions

[[ -s "/Users/200448/.gvm/scripts/gvm" ]] && source "/Users/200448/.gvm/scripts/gvm"

[[ -f $HOME/.acme.sh/.acme.sh.env ]] && . $HOME/.acme.sh/.acme.sh.env

export PATH="$HOME/.cargo/bin:$PATH"
eval `tset -s xterm-24bits`

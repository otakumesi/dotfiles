# export JAVA_HOME=`/usr/libexec/java_home -v 13`

export LANG=en_US.UTF-8
export EDITOR='emacs'

export XDG_CONFIG_HOME=$HOME

export JAVA_HOME=`/usr/libexec/java_home -v 1.8` export CARGO_HOME=$HOME/.cargo
export GOPATH=$HOME/goprojects
export PYENV_ROOT="$HOME/.pyenv"
export PATH="/usr/local/bin:$HOME/.tfenv/bin:$JAVA_HOME/bin:$PATH:$HOME/.ndenv/bin:$PATH"
export PATH="$HOME/.poetry/bin:$GOPATH/bin:$HOME/.rbenv/shims:$PYENV_ROOT/bin:$CARGO_HOME/bin:/usr/local/opt/sqlite/bin:$PATH"
export MANPATH="/usr/local/man:$MANPATH"

export GOROOT_BOOTSTRAP=$HOME/go1.4/bin

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

eval "$(nodenv init -)"
eval "$(rbenv init -)"
eval "$(pyenv init -)"
eval "$(pipenv --completion)"
eval "$(direnv hook zsh)"
[[ -s "/Users/200448/.gvm/scripts/gvm" ]] && source "/Users/200448/.gvm/scripts/gvm"
[[ -f $HOME/.acme.sh/.acme.sh.env ]] && . $HOME/.acme.sh/.acme.sh.env

export FZF_TMUX=1
export FZF_DEFAULT_OPTS='--no-height --no-reverse'
export FZF_CTRL_R_OPTS='--sort'
export FZF_TMUX_HEIGHT=30

if [ -z $TMUX ]; then
    if $(tmux has-session 2> /dev/null); then
	TERM=xterm-24bits tmux attach
    else
	TERM=xterm-24bits tmux new-session -s $(basename $(pwd))
    fi
fi

source $HOME/.aliases
source $HOME/.functions
which navi >/dev/null 2>&1 && source "$(navi widget zsh)"

export TERM=xterm-24bits
eval `tset -s xterm-24bits`

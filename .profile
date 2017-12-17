export LANG=en_US.UTF-8
export EDITOR='nvim'

XDG_CONFIG_HOME=$HOME

export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export CARGO_HOME=$HOME/.cargo
export PATH=$GOPATH/bin:$HOME/.rbenv/shims:$HOME/.rbenv/bin:$GOPATH/bin:$HOME/.meteor:$CARGO_HOME/bin:$HOME/.tfenv/bin:$JAVA_HOME/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"

export GOPATH=$HOME/goprojects
export GOROOT_BOOTSTRAP=$HOME/go1.4/bin

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

eval "$(rbenv init -)"
eval "$(direnv hook zsh)"

if [ $SHLVL = 1 ]; then
  tmux
fi

source $HOME/.aliases
source $HOME/.functions

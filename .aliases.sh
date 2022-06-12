# User
alias emacs='emacs -nw'
alias rootdir='git rev-parse --show-toplevel'
alias ein="emacs -e ein:login"
alias taskpaper="emacs -e open-taskpaper-file"

alias dict='open dict:://'
alias vi='vim -u NONE --noplugin'
alias vim='nvim -u ~/.vimrc'
alias ema='emacs'
alias ls="ls -a"
alias tm=tmux
which exa >/dev/null 2>&1 && alias ls=exa
which fd >/dev/null 2>&1 && alias find=fd
which rg >/dev/null 2>&1 && alias grep=rg

alias gpy="gtags --gtagslabel=pygments"
alias goprj="cd $GOPATH/src/github.com/otakumesi"

case "$(uname)" in
  'Darwin' )
    alias marp="open -a /Applications/Marp.app/";;
esac


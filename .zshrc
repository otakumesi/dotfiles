[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

export ZSH=$HOME/.oh-my-zsh
source $HOME/.profile

source ~/.zplug/init.zsh

zplug 'plugins/git', from:oh-my-zsh
zplug 'plugins/tmux', from:oh-my-zsh
zplug 'plugins/ssh-agent', from:oh-my-zsh
zplug 'plugins/zsh-completions', from:oh-my-zsh
zplug 'b4b4r07/enhancd', use:init.sh
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'themes/gnzh', from:oh-my-zsh, defer:2

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"

COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"
ENHANCD_FILTER=fzf
HISTFILE=~/.zsh_history

alias zshconfig="emacs ~/.zshrc"

autoload colors
colors

setopt no_beep
setopt no_nomatch
setopt prompt_subst
setopt transient_rprompt
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
setopt inc_append_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt EXTENDED_HISTORY
setopt hist_reduce_blanks
setopt hist_verify
setopt hist_no_store
setopt hist_expand
setopt inc_append_history

HISTFILE=~/.zsh_history
export HISTSIZE=1000
export SAVEHIST=10000

if [ "$(uname)" == 'Darwin' ]; then
  eval `gdircolors ~/dotfiles/dircolors/dircolors.256dark`
fi

# . /Users/200448/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source $HOME/.machinerc


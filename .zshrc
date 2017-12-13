source .profile

export ZSH=$HOME/.oh-my-zsh

plugins=(git ssh-agent ruby gem zsh-syntax-highlighting)

ZSH_THEME="agnoster"

CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"

COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"

plugins=(git)
source $ZSH/oh-my-zsh.sh

alias zshconfig="nvim ~/.zshrc"

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

export ZSH=$HOME/.oh-my-zsh
source $HOME/.profile

source ~/.zplug/init.zsh

zplug 'plugins/git', from:oh-my-zsh
zplug 'plugins/tmux', from:oh-my-zsh
zplug 'plugins/ssh-agent', from:oh-my-zsh

zplug 'plugins/pip', from:oh-my-zsh
zplug "darvid/zsh-poetry"

zplug 'plugins/ruby', from:oh-my-zsh
zplug 'plugins/rails', from:oh-my-zsh
zplug 'plugins/gem', from:oh-my-zsh

zplug 'plugins/zsh-completions', from:oh-my-zsh
zplug "b4b4r07/enhancd", use:init.sh
zplug "mrowa44/emojify", as:command
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'halfo/lambda-mod-zsh-theme', as:theme

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

ENHANCD_FILTER=peco
TMUX_POWERLINE_SEG_WEATHER_LOCATION="12796587"

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

if [ "$(uname)" == 'Darwin' ]; then
  eval `gdircolors ~/dotfiles/dircolors/dircolors.256dark`
fi

. /Users/200448/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source $HOME/.machinerc


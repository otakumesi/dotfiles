source $HOME/.profile
source $HOME/.zsh/vendor/antigen.zsh

antigen use oh-my-zsh

# defaultRepo
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle ruby
antigen bundle gem
antigen bundle ssh-agent
antigen bundle zsh-completions

antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme halfo/lambda-mod-zsh-theme

antigen apply

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
end

. /Users/200448/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

source $HOME/.machinerc

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

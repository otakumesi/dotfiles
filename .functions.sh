#!/bin/zsh

function pjroot() {
  cd $(git rev-parse --show-toplevel)
}

function cdghq() {
  local src="$(ghq list | fzf --query "$LBUFFER" --reverse)"
  if [ -n "$src" ]; then
      cd $(ghq root)/"${src}"
  fi
}

function nbacks() {
  for i in `seq 1 $1`
  do
    cd ..
  done
}

function fdcd() {
  local src="$(fd . --type d | fzf --select-1 --reverse)"
  if [ -n "$src" ]; then
    cd  $src
  fi
}

function fdema() {
  local src="$(fd . | fzf --select-1 --reverse)"
  if [ -n "$src" ]; then
    emacs  $src
  fi
}

function rgp() {
  rg $1 -l  | fzf --preview 'bat --color=always --style=header,grid {}' --preview-window=right:60% --height 50% --border --reverse --print-query
}

function fdp() {
  fd .  | fzf --preview 'bat --color=always --style=header,grid {}'  --preview-window=right:60%
}

function pjcd() {
  cd $(git rev-parse --show-toplevel)
  cd $(find . -type d | fzf --select-1 --reverse)
}

function pjema() {
  cd $(git rev-parse --show-toplevel)
  ema $(find . | fzf --select-1 --reverse)
}

function git-select-branch() {
  git branch | fzf --select-1 --reverse --query "$RBUFFER" | sed 's/\*//'
}

function git-checkout-list() {
  git checkout $(git-select-branch)
}
alias gcbl="git-checkout-list"

function git-push-list() {
  git push origin $(git-select-branch)
}
alias gpsl="git-push-list"

function git-pull-list() {
  git pull origin $(git-select-branch)
}
alias gpll="git-pull-list"

function ssh-select-host {
  ssh $(cat ~/.ssh/config | grep '^Host\s' | sed 's/Host//' | fzf --select-1 --reverse --query "$RBUFFER" )
}

function docker-image-select {
  docker images --format "{{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.CreatedSince}}\t{{.CreatedAt}}" | fzf | cut -f 1
}

function docker-container-select {
  docker ps --format "{{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Command}}\t{{.RunningFor}}" | fzf | cut -f 1
}

function start-project {
  cp -R "$HOME/.project.template/$(ls $HOME/.project.template | fzf)" $1
}

function fzf-select-tmux-session()
{
  local res
  res=$(tmux list-sessions | fzf | awk -F':' '{print $1}')
  if [ -n "$res" ]; then
    _cool-fzf-insert-command-line "tmux attach -t $res"
  fi
}

function rename-tmux-session-into-repository-name()
{
  local tabname=`basename $(git rev-parse --show-toplevel)`
  tmux rename-session $tabname
  echo "renamed current tmux session into $tabname"
  zle reset-prompt
}
zle -N rename-tmux-session-into-repository-name
bindkey "^Q" rename-tmux-session-into-repository-name

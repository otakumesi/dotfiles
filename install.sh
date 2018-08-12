#!/bin/bash

for f in .??*
do
  [[ "$f" == ".DS_Store" ]] && continue
  [[ "$f" == "README.md" ]] && continue
  [[ "$f" == ".git"* ]] && continue
  [[ "$f" =~ "^init\." ]] && continue

  echo $f
  ln -snf "$HOME/dotfiles/$f" "$HOME"
done

ln -snf "$HOME/.vim" "$HOME/.config/nvim"
ln -snf "$HOME/.vimrc" "$HOME/.config/nvim/init.vim"

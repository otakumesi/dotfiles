#!/bin/bash

for f in .??*
do
    [[ "$f" == ".DS_Store" ]] && continue
    [[ "$f" == "README.md" ]] && continue
    [[ "$f" =~ "^\.git" ]] && continue
    [[ "$f" =~ "^init\." ]] && continue

    ln -snf "$HOME/dotfiles/$f" "$HOME/$f"
done
ln -snf "$HOME/.vim" "$HOME/.config/nvim"
ln -snf "$HOME/dotfiles/.vimrc" "$HOME/.config/nvim/init.vim"

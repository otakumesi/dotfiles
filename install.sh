#!/bin/bash

for f in .??*
do
    [[ "$f" =~ "^\.git" ]] && continue
    [[ "$f" == ".DS_Store" ]] && continue
    [[ "$f" == "README.md" ]] && continue
    [[ "$f" =~ "^init\." ]] && continue

    ln -sf "$HOME/dotfiles/$f" "$HOME/$f"
done
ln -sf "$HOME/.vim" "$HOME/.config/nvim"
ln -sf "$HOME/dotfiles/.vimrc" "$HOME/.config/nvim/init.vim"

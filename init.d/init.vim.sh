curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
if [-e $HOME/.cache]; then
  mkdir $HOME/.cache
fi
sh ./installer.sh $HOME/.cache/dein

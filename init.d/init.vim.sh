curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
if [-e .cache]; then
  mkdir .cache
fi
sh ./installer.sh .cache/dein

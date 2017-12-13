for file in `\find ./init.d -maxdepth 1 -type f`; do
  chmod +x $file
  ./$file
done

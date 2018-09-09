#!/bin/bash

stack build brittany stylish-haskell
for source_file in `find src -name "*.hs"`; do
    stack exec -- brittany --columns=120 --write-mode=inplace "$source_file"
    stack exec -- stylish-haskell -i -c "styler/stylish-haskell.conf" "$source_file"
done

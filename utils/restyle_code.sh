#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "${SCRIPTPATH}/../"
stack build brittany stylish-haskell
for source_file in `find src -name "*.hs"`; do
    stack exec -- brittany --columns=120 --write-mode=inplace "$source_file"
    stack exec -- stylish-haskell -i -c "third-party/restyler/stylish-haskell.conf" "$source_file"
done

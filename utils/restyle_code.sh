#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
CONFIGPATH="third-party/brittany/config.yaml"
cd "${SCRIPTPATH}/../"
for source_file in `find src -name "*.hs"`; do
    brittany --config-file "${CONFIGPATH}"  --write-mode=inplace "$source_file"
done

#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "${SCRIPTPATH}/../"

echo "Clearing old licenses..."
rm -f "third-party/licenses/"*".txt"

function failure() {
    echo "!! Failure while downloading: $1"
    rm -f "$1"
}

for dependency in `stack ls dependencies --separator=-`; do
    if [[ "${dependency%-*}" != "ejstand" ]]
    then
        echo "Downloading license for ${dependency}..."
	filename="third-party/licenses/${dependency%-*}.txt"
	weburl="http://hackage.haskell.org/package/${dependency%-*}/src/LICENSE"
        wget -q -O "${filename}" "${weburl}" || failure "${filename}"
    fi
done

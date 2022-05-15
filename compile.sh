#!/usr/bin/env bash

ghc Main.hs
bash clear.sh

if [[ $1 = "run" ]]; then
 ./Main
fi

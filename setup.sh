#! /bin/bash

# install
echo '[begin]'
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
git clone https://github.com/Sekomer/Victim
cd Victim
cabal install -O2 --overwrite-policy=always

# test
echo ''
echo 'Testing factorial function'
echo 'fact 150 is:'
Victim examples/factorial.v
echo '[done]'
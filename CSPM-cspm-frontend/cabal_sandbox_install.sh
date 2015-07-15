#!/bin/bash
# cabal sandbox delete                       # Built-in command
rm -rf .cabal-sandbox cabal.sandbox.config # uninstalling sandbox

# initialise sandboc
cabal sandbox init

#add sources to local repository
cabal sandbox add-source ../CSPM-Frontend
cabal sandbox add-source ../CSPM-ToProlog

# install only dependencies
cabal install --only-dependencies

#build cspmf-tool
cabal build

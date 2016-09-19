#!/bin/bash

# install_cspmf_for_prob.sh
system=$(uname -s)
lbits=$(uname -m)

# get system type
if [[ "$system" == "Darwin" ]]; then
  sys="darwin"
elif [[ "$system" == "Linux" ]]; then
  sys="linux"
else
  sys="windows"
fi

# get system bits
if [[ $lbits =~ .*64.* ]]; then
  bits=64
else
  bits=32
fi

if [[ $sys == "windows" ]]; then
  install_dir=$sys
else
  install_dir=$sys-$bits
fi

rm -rf ../artifacts/$install_dir
mkdir -p ../artifacts/$install_dir

# install cspmf with cabal
./cabal_sandbox_install.sh

cp dist/build/cspmf/cspmf ../artifacts/$install_dir

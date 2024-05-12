#!/usr/bin/env bash

git clone git@github.com:prog-lang/pure.git
cd pure
stack install
cd ..
rm -rf pure

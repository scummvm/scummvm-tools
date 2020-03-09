#! /bin/bash

cp -v ../../../scummvm/devtools/create_prince/en.po .
perl po-parse.pl en en.po
../../scummvm-tools-cli --tool pack_prince .
cp -v prince_translation.dat ../../../scummvm/dists/engine-data/

#! /bin/bash

set -e

translations_dir="$1"
if [ -z "$translations_dir" ] || [ ! -d "$translations_dir" ]; then
  echo "Usage: $0 <path-to-scummvm-game-translations>" >&2
  echo "Download or clone from https://github.com/scummvm/game-translations/tree/prince-and-the-coward" >&2
  exit 1
fi

cp -v "$translations_dir"/en.po .
perl po-parse.pl en en.po
../../scummvm-tools-cli --tool pack_prince .
cp -v prince_translation.dat ../../../scummvm/dists/engine-data/

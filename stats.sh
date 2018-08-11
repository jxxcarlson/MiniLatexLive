#!/bin/sh

COMPILER="/Users/carlson/Downloads/2/elm"

set -e

## CHECK FOR BINARIES

if ! [ -x "$(command -v uglifyjs)" ]
then
  echo 'Error: need uglifyjs to be available for asset size test.'
  echo 'You can run `npm install --global uglify-js` to get it.'
  exit 1
fi

if [ ! -x ./elm ] || [ "$(./elm --version)" != "0.19.0" ]
then
  echo 'Error: need preview `elm` binary to be in the root of your project.'
  exit
fi


## ACTUALLY MEASURE THINGS

js="elm.js"
min="elm.min.js"

echo "======== SETUP ============================"
./elm make --output=$js $@
rm -rf elm-stuff

echo "\n======== COMPILE TIME ====================="
time ./elm make --optimize --output=$js $@

echo "\n======== ASSET SIZE ======================="
uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$min
echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"
rm $js $min

echo "\n======== PROJECT SIZE ====================="
find . -name '*.elm' | xargs wc -l

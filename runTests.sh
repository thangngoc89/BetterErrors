#!/bin/sh

if [[ -z $ESY__PREFIX ]]; then
  echo "You must run the tests in the correct sandbox. Do:"
  echo ""
  echo "  esy ./runTests.sh.sh"
  echo ""
  exit 1
fi

refmt ./tests/test.re --print ml > ./tests/test.ml
ocaml ./tests/test.ml

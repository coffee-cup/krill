set -e

stack build
stack test

stack exec krill -- test/code.kr

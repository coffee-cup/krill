set -e

stack build
stack test
stack exec krill -- run test/code.kr

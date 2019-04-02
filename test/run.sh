set -e

stack build
stack exec krill -- run test/code.kr

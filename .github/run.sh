#!/bin/bash

set -e

no_test=0
no_lint=0

while (( $# > 0 )); do
   case "$1" in
   	--help)
			printf "run.sh [OPTION]... [DIR]\n"
			printf "options:\n"
			printf "\t--help			Show help\n"
			printf "\t--no-test		Skip tests\n"
			printf "\t--no-lint		Skip linting\n"
			exit 0
      	;;
      --no-test)
			no_test=1
			shift
      	;;
      --no-lint)
			no_lint=1
			shift
			;;
		*)
			break
	      ;;
   esac
done

./millw __.compile

if (( no_test == 0 )); then
  if [[ -z "$1" ]]; then
    ./millw __.test
  elif ./millw resolve "$1".test &>/dev/null; then
    ./millw "$1".test
  else
    red='\033[0;31m'
    no_color='\033[0m'
	printf "%bNo tests found in: %s%b\n" "$red" "$1" "$no_color"
  fi
fi

if (( no_lint == 0 )); then
	if [[ -z "${CI}" ]]; then
	  ./millw mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
	else
		./millw mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll __.sources
	fi
fi

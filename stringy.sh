#!/bin/bash

DEV_MODE=false
COMPILER=python
READ=false

OPTIND=1

while getops ":hdf:" opt; do
  case "$opt" in
    h)
      COMPILER="haskell"
      ;;
    d)
      DEV_MODE=true
      ;;
    f)
      FILE=$OPTARG
      ;;
  esac
done

if [$COMPILER == "haskell"]; then
  echo "Whoops! Haskell is not set up yet!"
else
  if [$FILE -n]; then
    if [$DEV_MODE == true]; then
      python stringy_loop_dev.py $FILE
    else
      python stringy_loop.py $FILE
    fi
  else
    if [$DEV_MODE == true]; then
      python stringy_dev.py
    else
      python stringy.py
    fi
  fi
fi

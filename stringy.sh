#!/bin/bash

DEV_MODE=false
COMPILER=python
READ=false

OPTIND=1

pyloopdev () {
  python ./stringy_py/stringy_loop_dev.py $FILE
}

pyloop () {
  python ./stringy_py/stringy_loop.py $FILE
}

pydev () {
  python ./stringy_py/stringy_dev.py $FILE
}

py () {
  python ./stringy_py/stringy.py $FILE
}

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
      pyloodev
    else
      pyloop
    fi
  else
    if [$DEV_MODE == true]; then
      pydev
    else
      py
    fi
  fi
fi

#!/bin/bash

DEV_MODE=false
COMPILER=python
READ=false

OPTIND=1

alias pyloopdev="python ./stringy_py/stringy_loop_dev.py $FILE"
alias pyloop="python ./stringy_py/stringy_loop.py $FILE"
alias pydev="python ./stringy_py/stringy_dev.py $FILE"
alias py="python ./stringy_py/stringy.py $FILE"
alias hask=""
alias haskdev=""

stringy_loop () {
  COMPILERS='Python Haskell Exit'
  BOOLS='true false Exit'
  FILES='file one-command Exit'
  PS3='Select compiler: '
  select COMPILER in $COMPILERS
  do
    case $COMPILER in
      Python | Haskell)
        PS3='File or one command? '
        select OP in $FILES
        do
          case $OP in
            file | "one-command")
              PS3='Dev mode? '
              select DEV in $BOOLS
              do
                case $DEV in
                  true | false)
                    newvar=$COMPILER" "$OP" "$DEV
                    case $newvar in
                      "Python one-command false")
                        py
                        ;;
                      "Python file false")
                        pyloop
                        ;;
                      "Python one-command true")
                        pydev
                        ;;
                      "Python file true")
                        pyloopdev
                        ;;
                      "Haskell one-command false")
                        hask
                        ;;
                      "Haskell file false")
                        echo Haskell file reading is not complete
                        ;;
                      "Haskell one-command true")
                        haskdev
                        ;;
                      "Haskell file true")
                        echo Haskell file reading is not complete
                        ;;
                    esac
                    break 2
                    ;;
                  Exit)
                    break 3
                    ;;
                  *)
                    ;;
                esac
              done
              ;;
            Exit)
              break 2
              ;;
            *)
              ;;
          esac
        done
        ;;
      Exit)
        break
        ;;
      *)
        ;;
    esac
    PS3='Select compiler: '
  done
}

# needs completing bridging the gap ^v

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

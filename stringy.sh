#!/bin/bash

DEV_MODE=false
COMPILER=Python
READ="one-command"

OPTIND=1

alias pyloopdev="python ./stringy_py/stringy_loop_dev.py $FILE"
alias pyloop="python ./stringy_py/stringy_loop.py $FILE"
alias pydev="python ./stringy_py/stringy_dev.py $FILE"
alias py="python ./stringy_py/stringy.py $FILE"
alias hask="echo This option has not been completed"
alias haskdev="echo This option has not been completed"
alias haskloop="echo This option has not been completed"
alias haskloodev="echo This option has not been completed"
alias stringy_info="cat ./stringy_info.txt"

while getops ":ihdf:" opt; do
  case "$opt" in
    i)
      stringy_info
      exit 1
      ;;
    h)
      COMPILER="Haskell"
      ;;
    d)
      DEV_MODE=true
      ;;
    f)
      READ="file"
      FILE=$OPTARG
      ;;
  esac
done

if [ $OPTARG -n ] 
then
  stringy_loop
else
  stringy $COMPILER" "$READ" "$DEV_MODE
fi

stringy_loop () {
  COMPILERS='Python Haskell Exit'
  BOOLS='true false Exit'
  FILES='file one-command Exit'
  PS3='Select compiler: '
  select COMP in $COMPILERS
  do
    case $COMP in
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
                    newvar=$COMP" "$OP" "$DEV
                    stringy $newvar
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

stringy () {
  case $1 in
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
      haskloop
      ;;
    "Haskell one-command true")
      haskdev
      ;;
    "Haskell file true")
      haskloopdev
      ;;
  esac
}

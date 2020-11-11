#!/bin/sh

keep_going=
direction=1

if test "$1" = "-k"; then
    keep_going=true
    shift
fi

if test "$1" = "-r"; then
    direction="-1"
    shift
fi

TPL="$1"

FAULTSTART='' "$@" 2>faultinject.stderr
EXIT_CODE="$?"
if test $EXIT_CODE -gt 127
then
    echo "initial run crashed"
    exit 1
fi

FAULTEND=$(awk '/CDEBUG FAULT INJECTION MAX/{print $5}' faultinject.stderr)

if test -z "$FAULTEND"
then
    echo "couldn't find the maximum number of fault injections"
    exit 1
fi

ulimit -S -c unlimited

export FAULTSTART=1

if test "$direction" != 1
then
    FAULTSTART=$FAULTEND
    FAULTEND=0
fi

while test "$FAULTSTART" -ne "$FAULTEND"
do
    echo "trying $FAULTSTART"
    "$@" 2>faultinject.stderr >faultinject.stdout
    EXIT_CODE="$?"
    if test "$EXIT_CODE" -gt 127; then
        echo "crashed with exit code $EXIT_CODE"
        mv faultinject.stderr faultinject$FAULTSTART.stderr
        mv faultinject.stdout faultinject$FAULTSTART.stdout
        gdb -batch -ex 'bt full' "$TPL" core >faultinject$FAULTSTART.bt
        if test -z "$keep_going"; then
            exit 1
        fi
    fi
    rm -f faultinject.stderr faultinject.stdout
    FAULTSTART=$((FAULTSTART + direction))
done

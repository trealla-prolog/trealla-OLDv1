#!/bin/sh

keep_going=
direction=1
valgrind=
show=
quiet=
filter=
cont=
timeout=60

while test "$1" != "${1#-}"
do
    case "$1" in
    -k|--keep-going) # Do not stop at first crash
        keep_going=true
        cont=
        shift
        ;;
    -r|--reverse) # Run fault injection tests in reverse direction
        direction="-1"
        shift
        ;;
    -s|--show) # Start 'less' for each failed test to inspect the results (use :n/:p to go through files)
        show=true
        quiet=
        shift
        ;;
    -g|--valgrind) # Add a valgrind report
        valgrind=true
        shift
        ;;
    -q|--quiet) # Don't keep reports
        quiet=true
        show=
        shift
        ;;
    -t|--timeout) # Set a timeout (in seconds) for the tests
        timeout="$(( 0 + $2 ))"
        shift 2
        ;;
    -f|--filter) # Filter only the given failures (segv, abort, timeout) can appear multiple times
        case "$2" in
        *ABRT|*abrt|*abort|134)
            filter="134,$filter"
            ;;
        *SEGV|*segv|139)
            filter="139,$filter"
            ;;
        *TIMEOUT|*timeout|152)
            filter="152,$filter"
            ;;
        *)
            echo "Illegal filter expression: $2" 1>&2
            exit 1;
        esac
        shift 2
        ;;
    -c|--continue) # Continue with the previously failed iteration
        keep_going=
        cont=true
        shift
        ;;
    -h|--help) # Show this help
        cat <<EOF
fault injection driver

Usage:

 $0 [options] [-- <command> [arguments..]]

Options:
$(sed 's/ *\([-|[:alpha:]]*\)[^)]*) *# \(.*\)/  \1\n     \2\n/p;d' < "$0")

EOF
        exit 0;
        ;;
    --) # Stop processing commandline arguments
        shift
        break
        ;;
    esac
done

if test -z "$1"; then
        cat <<EOF
fault injection driver

Usage:
  $0 [options..] [-- <command> [arguments..]]
  $0 --help
EOF
    exit 0
fi

TPL="$1"
export FAULTSTART

if test "$cont" -a -f faultinject_state
then
    . ./faultinject_state
else
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

    if test "$direction" = 1
    then
        FAULTSTART=1
    else
        FAULTSTART=$FAULTEND
        FAULTEND=0
    fi
    echo "Testing from $FAULTSTART to $FAULTEND"
fi

ulimit -S -c unlimited

FAULTS=0

while test "$FAULTSTART" -ne "$FAULTEND"
do
    cat <<EOF >faultinject_state
FAULTSTART=$FAULTSTART
FAULTEND=$FAULTEND
direction=$direction
EOF
    echo "Faultinject $FAULTSTART"
    echo "        $*"

    (
        ulimit -S -t $timeout
        "$@" 2>faultinject.stderr >faultinject.stdout
    )
    EXIT_CODE="$?"

    if test "$EXIT_CODE" -gt 127; then
        if test ! "$filter" || expr "$filter" : ".*$EXIT_CODE,.*"; then
            FAULTS=$((FAULTS + 1))
            case $EXIT_CODE in
            134)
                echo "                crashed with SIGABRT"
                ;;
            152)
                echo "                crashed with TIMEOUT"
                ;;
            139)
                echo "                crashed with SIGSEGV"
                ;;
            *)
                echo "                crashed with exit-code $EXIT_CODE"
                ;;
            esac | tee -a faultinject$FAULTSTART.stderr
            if test -z "$quiet" ; then
                tail -1 faultinject$FAULTSTART.stderr >faultinject$FAULTSTART.bt
                gdb -batch -ex 'bt full' "$TPL" core >>faultinject$FAULTSTART.bt
                mv faultinject.stderr faultinject$FAULTSTART.stderr
                mv faultinject.stdout faultinject$FAULTSTART.stdout
            fi
            vglog=
            if test "$valgrind" ; then
                vglog=faultinject$FAULTSTART.vg
                valgrind --log-file=$vglog "$@"
            fi
            if test "$show"; then
                less faultinject$FAULTSTART.bt $vglog faultinject$FAULTSTART.stderr faultinject$FAULTSTART.stdout
            fi
            if test -z "$keep_going"; then
                exit 1
            fi
        fi
    else
        echo "                OK with exit-code $EXIT_CODE"
    fi

    rm -f faultinject.stderr faultinject.stdout
    FAULTSTART=$((FAULTSTART + direction))
done

echo "Found $FAULTS crashes"

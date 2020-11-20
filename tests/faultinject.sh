#!/bin/sh

keep_going=
direction=1
valgrind=
show=
quiet=
filter=
cont=
timeout=60
input=

export TPL="${TPL:-./tpl}"

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
    -t|--timeout) #<timeout> Set a timeout (in seconds) for the tests
        timeout="$(( 0 + $2 ))"
        shift 2
        ;;
    -f|--filter) #<filter> Filter only the given failures (segv, abort, timeout), can appear multiple times
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
    -i|--input) #<file> read tests to execute from file (or - for stdin)
        input="$2"
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

  $0 [options..] [-- <command> [arguments..]]

Options are:
$(sed 's/ *\([-|[:alpha:]]*\)[^)]*) *#\([^ ]*\) *\(.*\)/  \1 \2\n     \3\n/p;d' < "$0")

EOF
        exit 0;
        ;;
    --) # Stop processing commandline arguments
        shift
        break
        ;;
    esac
done

if test -z "$input" -a -z "$1"; then
        cat <<EOF
fault injection driver

Usage:
  $0 [options..] [-- <command> [arguments..]]
  $0 --help
EOF
    exit 0
fi

FAULTS=0

ITERATION=1

case "$input" in
-)
    cat
    ;;
?*)
    cat "$input"
    ;;
'')
    echo "$*"
    ;;
esac | while read test; do

    test="${test%%#*}"
    if test -z "$test"; then
        continue
    fi

    export FAULTSTART

    test=$(eval echo "$test")
    PROGRAM="${test%% *}"
    echo "Testing: $test"

    if test "$cont" -a -f faultinject_state
    then
        . ./faultinject_state
    else
        (
            ulimit -S -t $timeout
            FAULTSTART='' $test 2>faultinject.stderr >/dev/null
        )
        EXIT_CODE="$?"
        if test $EXIT_CODE -gt 127
        then
            echo "initial run did not complete"
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

    while test "$FAULTSTART" -ne "$FAULTEND"
    do
        cat <<EOF >faultinject_state
FAULTSTART=$FAULTSTART
FAULTEND=$FAULTEND
direction=$direction
EOF
        echo "Faultinject $FAULTSTART"
        echo "        $test"

        (
            ulimit -S -c unlimited
            ulimit -S -t $timeout
            $test 2>faultinject.stderr >faultinject.stdout
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
                esac | tee -a faultinject${ITERATION}_${FAULTSTART}.stderr
                if test -z "$quiet" ; then
                    tail -1 faultinject${ITERATION}_${FAULTSTART}.stderr >faultinject${ITERATION}_${FAULTSTART}.bt
                    gdb -batch -ex 'bt full' "$PROGRAM" core >>faultinject${ITERATION}_${FAULTSTART}.bt
                    mv faultinject.stderr faultinject${ITERATION}_${FAULTSTART}.stderr
                    mv faultinject.stdout faultinject${ITERATION}_${FAULTSTART}.stdout
                fi
                vglog=
                if test "$valgrind" ; then
                    vglog=faultinject${ITERATION}_${FAULTSTART}.vg
                    valgrind --log-file=$vglog $test
                fi
                if test "$show"; then
                    less faultinject${ITERATION}_${FAULTSTART}.bt $vglog faultinject${ITERATION}_${FAULTSTART}.stderr faultinject${ITERATION}_${FAULTSTART}.stdout
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

    ITERATION=$((ITERATION + 1))
done


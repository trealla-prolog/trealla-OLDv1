#!/bin/sh

# may need OS specific config
SIGABRT=134
SIGSEGV=139
TIMEOUT=152

keep_going=
direction=1
valgrind=
show=
quiet=
filter=
cont=
timeoutctl=auto
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
    -t|--timeout) #<timeout> Set a timeout (in seconds or the word 'auto') for the tests
        if test "$2" = "auto"; then
            timeoutctl="auto"
        else
            timeoutctl="$(( 0 + $2 ))"
        fi
        shift 2
        ;;
    -f|--filter) #<filter> Filter only the given failures (segv, abort, timeout), can appear multiple times
        case "$2" in
        *ABRT|*abrt|*abort|$SIGABRT)
            filter="$SIGABRT,$filter"
            ;;
        *SEGV|*segv|$SIGSEGV)
            filter="$SIGSEGV,$filter"
            ;;
        *TIMEOUT|*timeout|$TIMEOUT)
            filter="$TIMEOUT,$filter"
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

    if test "$cont" -a -f faultinject_state
    then
        . ./faultinject_state
        if test "$pending_test" != "$test"; then
            continue
        else
            cont=
        fi
    else
        echo "Testing: $test"
        starttime=$(awk 'BEGIN {srand(); print srand()}') # tricky, portable hack
        (
            FAULTSTART='' $test 2>faultinject.stderr >/dev/null
        )
        EXIT_CODE="$?"
        if test $EXIT_CODE -gt 127
        then
            echo "initial run did not complete"
            continue
        fi

        FAULTEND=$(awk '/CDEBUG FAULT INJECTION MAX/{print $5}' faultinject.stderr)

        if test -z "$FAULTEND"
        then
            echo "couldn't find the maximum number of fault injections"
            continue
        fi

        if test "$timeoutctl" = "auto"; then
            timeout=$(( $(awk 'BEGIN {srand(); print srand()}') - starttime + 2 ))
            echo "Set auto timeout to: $timeout"
        else
            timeout=$timeoutctl
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
pending_test="$test"
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
                $SIGABRT)
                    echo "                crashed with SIGABRT"
                    ;;
                $TIMEOUT)
                    echo "                crashed with TIMEOUT"
                    ;;
                $SIGSEGV)
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


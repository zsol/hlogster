#!/bin/bash
#
# Simple test runner for hlogster
#
# Tests are defined as directories cantaining
#   * metrics.json: metric definitions to use (argument to hlogster)
#   * input.txt: log lines passed to hlogster on stdin
#
# And any number of the following:
#   * tcp.txt: expected output sent to Graphite, without the timestamps; no check done if doesn't exist
#   * stdout.txt: standard output of hlogster; no check done if doesn't exist
#   * stderr.txt: standard error of hlogster; no check done if doesn't exist
#   * retval.txt: return code of hlogster; defaults to 0
#
# Usage: ./run.sh [-b hlogster_binary_path] [-p graphite_port] [-t "testcase[ testcase...]"]
#
# If -t is omitted, all directories on cwd are treated as test cases and run
#

pass=0
fail=0
error=0

function assertEquals
{
    expected=`cat $1`
    actual=`cat $2`
    name=$3

    if [[ "$expected" != "$actual" ]]; then
        fail=$(expr $fail + 1)
        echo "  FAIL: $name"
        echo "    Expected:"
        cat $1 | awk '{print "      " $0}'
        echo "    Actual:"
        cat $2 | awk '{print "      " $0}'
    else
        pass=$(expr $pass + 1)
        echo "  PASS($name)"
    fi
}


hlogster=../dist/build/hlogster/hlogster
port=2003
tests=$(ls -l | grep '^d' | awk '{print $9}')

while getopts ":b:p:t:" opt; do
    case $opt in
        b)
            hlogster=$OPTARG
            ;;
        p)
            port=$OPTARG
            ;;
        t)
            tests=$OPTARG
            ;;
    esac
done

if [ ! -f $hlogster ]; then
    echo ERROR: hlogster binary not found at $hlogster
    exit 1
fi

if [[ $tests == '' ]]; then
    echo No tests to run
    exit 1
else
    echo Test cases to run: $tests
fi

ret=0

for t in $tests
do
    echo Case \"$t\"
    if [ ! -f $t/input.txt ]; then
        echo "  ERROR: Input file $t/input.txt not found"
        error=$(expr $error + 1)
        ret=2
        continue
    elif [ ! -f $t/metrics.json ]; then
        ret=3
        error=$(expr $error + 1)
        echo "  ERROR: Metrics file $t/metrics.json not found"
        continue
    fi

    tcp=$(mktemp hlogster.XXXXX)
    stdout=$(mktemp hlogster.XXXXX)
    stderr=$(mktemp hlogster.XXXXX)
    nc -l 2003 | awk '{print $1 " " $2}' > $tcp 2>&1 &
    nc_pid=$!
    $hlogster $t/metrics.json >$stdout 2>$stderr < $t/input.txt

    if [ -f $t/retval.txt ]; then
        expected_retval=$(cat $t/retval.txt)
    else
        expected_retval=0
    fi

    if [ $? -ne $expected_retval ]; then
        echo "  FAIL(retval): hlogster exited with code $?, expected $exepcted_retval"
        kill $nc_pid
        ret=5
    fi
    if [ $? -eq 0 ]; then
        wait $nc_pid
    fi

    if [ -f $t/tcp.txt ]; then
        assertEquals $t/tcp.txt $tcp 'TCP'
    fi

    if [ -f $t/stdout.txt ]; then
        assertEquals $t/stdout.txt $stdout 'STDOUT'
    fi

    if [ -f $t/stderr.txt ]; then
        assertEquals $t/stderr.txt $stderr 'STDERR'
    fi

    rm $tcp
    rm $stdout
    rm $stderr
done

echo $pass assertions passed, $fail assertions failed, $error invalid test cases

exit $ret

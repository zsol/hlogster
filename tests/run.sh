#!/bin/bash
#
# Simple test runner for hlogster
#
# Tests are defined as directories cantaining
#   * metrics.json: metric definitions to use (argument to hlogster)
#   * input.txt: log lines passed to hlogster on stdin
#   * output.txt: expected output sent to Graphite, without the timestamps
#
# Usage: ./run.sh [-b hlogster_binary_path] [-p graphite_port] [-t testcase[ testcase...]]
#
# If -t is omitted, all directories on cwd are treated as test cases and run
#

hlogster=../dist/build/hlogster/hlogster
port=2003
tests=$(ls -l | grep '^d' | awk '{print $9}')

while getopts ":b:p:" opt; do
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
    echo Error: hlogster binary not found at $hlogster
    exit 1
fi

for t in $tests; do
    if [ ! -d "$t" ]; then
        echo Test $t not found, skipping
    fi
done

if [[ $tests == '' ]]; then
    echo No tests to run
    exit 1
else
    echo Test cases to run: $tests
fi

ret=0

for t in $tests
do
    echo -n Case \"$t\":' '
    if [ ! -f $t/input.txt ]; then
        echo ERROR: Input file $t/input.txt not found
        ret=2
        continue
    elif [ ! -f $t/output.txt ]; then
        ret=3
        echo Error: Expected output file $t/output.txt not found
        continue
    elif [ ! -f $t/metrics.json ]; then
        ret=4
        echo Error: Metrics file $t/metrics.json not found
        continue
    fi

    tmp=$(mktemp -t hlogster)
    nc -l 2003 > $tmp 2>&1 &
    $hlogster $t/metrics.json < $t/input.txt
    if [ $? -ne 0 ]; then
        echo Error: hlogster exited with code $?
        kill $!
        ret=5
    fi
    wait $!

    actual=$(cat $tmp | awk '{print $1 " " $2}')
    expected=`cat $t/output.txt`

    if [[ "$actual" != "$expected" ]]; then
        echo FAIL
        echo Expected:
        cat $t/output.txt
        echo Actual:
        awk '{print $1 " " $2}' $tmp
        ret=1
    else
        echo PASS
    fi
    rm $tmp
done

exit $ret

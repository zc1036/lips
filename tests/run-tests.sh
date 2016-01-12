#!/bin/bash

# From http://stackoverflow.com/a/246128/4487961
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

TEST_OUTPUTFILE="$DIR/test-output.tmp"
LIPS="$DIR/../bin/lips"

echo Running tests...

FAIL=0

# Loop through each file with the extension ".input" in the lips/
# directory, run lips on it, and compare the output with the
# corresponding file with the extension ".output", and report the
# result.
for TEST in "$DIR"/lips/*.input; do
    TESTFILE=$(basename "$TEST")
    TESTNAME=${TESTFILE:0:-6}

    printf "Running test \"${TESTNAME}\"... "

    SOLUTION_NAME="${TESTNAME}.output"
    SOLUTION_FILE="$DIR/lips/$SOLUTION_NAME"

    if [ ! -f "$SOLUTION_FILE" ]; then
        echo "Error: solution to test, expected to be at \"$SOLUTION_FILE\", does not exist"
        ((FAIL++))
    else
        (cd "$DIR/lips" && eval $LIPS <"$TEST" >"$TEST_OUTPUTFILE")

        diff --brief "$TEST_OUTPUTFILE" "$SOLUTION_FILE" >/dev/null

        if [ $? -eq 0 ]; then
            echo pass
        else
            echo "fail; saving bad output to ${SOLUTION_NAME}.failed"
            mv "$TEST_OUTPUTFILE" "${DIR}/lips/${SOLUTION_NAME}.failed"
            ((FAIL++))
        fi
    fi
done

rm -f "$TEST_OUTPUTFILE"

if [ $FAIL -eq 0 ]; then
    echo 'All tests passed!'
else
    echo "$FAIL test(s) failed"
    exit 1
fi

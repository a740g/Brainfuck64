#!/bin/bash

# Brainfuck64 test runner for Linux/macOS

BF="./Brainfuck64"
FAILED=0

# Ensure the executable exists
if [ ! -f "$BF" ]; then
    echo "Error: $BF not found. Please run 'make' first."
    exit 1
fi

for test_file in tests/*.bf; do
    name=$(basename "$test_file" .bf)
    expected_file="tests/$name.expected"
    
    if [ -f "$expected_file" ]; then
        echo -n "Running test $name..."
        
        # Run the interpreter and capture output
        # Normalize newlines by removing \r
        output=$($BF "$test_file" | tr -d '\r')
        expected=$(cat "$expected_file" | tr -d '\r')
        
        if [ "$output" == "$expected" ]; then
            echo " PASSED"
        else
            echo " FAILED"
            echo "Expected: [$expected]"
            echo "Got:      [$output]"
            FAILED=$((FAILED + 1))
        fi
    fi
done

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0

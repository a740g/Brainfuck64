#!/bin/bash

# Brainfuck64 test runner for Linux/macOS

if [ -f "./Brainfuck64.exe" ]; then
    BF="./Brainfuck64.exe"
elif [ -f "./Brainfuck64" ]; then
    BF="./Brainfuck64"
else
    echo "Error: Brainfuck64 executable not found. Please run 'make' first."
    exit 1
fi

FAILED=0

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

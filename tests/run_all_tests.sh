#!/usr/bin/env bash

RED=$(tput setaf 1)
NORMAL=$(tput sgr0)
GREEN=$(tput setaf 2)
col=30

# First run helpers tests
if ! ozengine HelpersTests.ozf ; then
    echo
    printf '%s%*s%s\n' "$RED" $col "Helper tests failed. Not running compile tests!" "$NORMAL"
    echo
    exit 1
else
    echo
    printf '%s%*s%s\n' "$GREEN" $col "Helper tests passed!" "$NORMAL"
    echo
fi

if ! ./run_test_definitions.sh ; then 
    printf '%s%*s%s\n' "$RED" $col "Compile tests failed. Stopping here!" "$NORMAL"
    echo
    exit 2
else
    printf '%s%*s%s\n' "$GREEN" $col "Compile tests passed!" "$NORMAL"
    echo
fi



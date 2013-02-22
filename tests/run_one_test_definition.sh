#!/usr/bin/env bash

################################################################################
# Source config
################################################################################
. ./tests_config.sh

################################################################################
# Validation of argument
################################################################################
if (( $# != 1 )) ; then
  echo "call $0 path/to/test.oz"
  echo "Runs the test with code in the oz file passed as argument, even if it is marked to be skipped in the complete test run."
  exit 1
fi

################################################################################
# Setup
################################################################################

# ensure tests are read-only
chmod a-w $testsdir/*
chmod +w $resultsdir



f=$1
filename=${f##*/}
basename=${filename%.*}
echo $basename
dest=$resultsdir/${filename%.*}

run_test

echo "done"

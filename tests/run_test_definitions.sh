#!/usr/bin/env bash

################################################################################
# Source config
################################################################################
. ./tests_config.sh

################################################################################
# Setup
################################################################################

# ensure tests are read-only
chmod a-w $testsdir/*
chmod +w $resultsdir



for f in $testsdir/*oz; do
  filename=${f##*/}
  basename=${filename%.*}
  echo $basename
  dest=$resultsdir/${filename%.*}
  if grep '^% --SKIP TEST--$' $f > /dev/null ; then
    display_skipped_test_warning
  else
    run_test
  fi

done
echo "done"

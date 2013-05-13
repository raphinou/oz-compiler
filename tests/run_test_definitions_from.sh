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
start=$1



for num in $(seq --equal-width  $start 999) ; do
  if ls $testsdir/$num*oz >/dev/null 2>&1; then
    f=$(ls $testsdir/$num*oz)
    filename=${f##*/}
    basename=${filename%.*}
    echo $basename
    dest=$resultsdir/${filename%.*}
    if grep '^% --SKIP TEST--$' $f > /dev/null ; then
      display_skipped_test_warning
    else
      run_test
    fi
  else
    continue
  fi

done
echo "done"

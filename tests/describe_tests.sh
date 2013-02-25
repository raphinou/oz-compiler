#!/usr/bin/env bash

################################################################################
# Source config
################################################################################
. ./tests_config.sh

for f in $testsdir/*oz; do
  filename=${f##*/}
  basename=${filename%.*}
  describe_test
done


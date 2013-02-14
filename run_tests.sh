

halt_on_error=1
view_diffs_on_error=1
diff_viewer=vimdiff



testsdir=tests
resultsdir=$testsdir/results


# colors
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
NORMAL=$(tput sgr0)

# display OK/KO in this column
col=30

################################################################################
# Setup
################################################################################

# ensure tests are read-only
chmod a-w $testsdir/*
chmod +w $testsdir/results


# Function that will check the output of the test is what was expected.
# takes the extension of the files to use from parent context (err or out)
# and set stop=1 if an error is encountered and halt_on_error is set to 1
function valid_output()
{
  # ext is from parent context
  # stop is set to 1 and used by parent
  echo -n "std$ext"
  if diff $testsdir/$basename.$ext $resultsdir/$basename.$ext >/dev/null ; then
    printf '%s%*s%s\n' "$GREEN" $col "[OK]" "$NORMAL"
    return 0
  else
    printf '%s%*s%s\n' "$RED" $col "[KO]" "$NORMAL"
    if [[ $view_diffs_on_error -eq 1 ]] ; then
      $diff_viewer $testsdir/$basename.$ext $resultsdir/$basename.$ext
    fi
    return 1
  fi

}

for f in $testsdir/*oz; do
  filename=${f##*/}
  basename=${filename%.*}
  echo $basename
  dest=$resultsdir/${filename%.*}
  ozengine src/TestRunner.ozf $f 2> $dest.err | tee $dest.debug | sed -e '1,/^--END DEBUG--$/d' > $dest.out

  extensions="out err"
  for ext in $extensions; do 
    if ! valid_output && [[ $halt_on_error -eq 1 ]] ; then
      echo "Halting on error"
      break
    fi
  done

done
echo "done"

halt_on_error=1
view_diffs_on_error=1
diff_viewer=vimdiff



testsdir=definitions
resultsdir=results


# colors
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
NORMAL=$(tput sgr0)

# display OK/KO in this column
col=30


# Function that will check the output of the test is what was expected.
# takes the extension of the files to use from parent context (err or out)
# and set stop=1 if an error is encountered and halt_on_error is set to 1
function valid_output()
{
  # basename,ext are from parent context
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
function run_test()
{
    ozengine TestRunner.ozf $f 2> $dest.err | tee $dest.debug | sed -e '1,/^--END DEBUG--$/d' > $dest.out

    extensions="err out"
    for ext in $extensions; do 
      if ! valid_output && [[ $halt_on_error -eq 1 ]] ; then
        echo "Halting on error"
        exit 1
      fi
    done
}
function display_skipped_test_warning()
{
    # this is a test to skip currently
    printf '%s%s\n' "$RED" "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "$basename is skipped as request in test definition"
    printf '%s%s%s\n' "$RED" "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" "$NORMAL"
    echo "Remove the line '% --SKIP TEST--' from the test definition file to re-enable it"
}

function describe_test()
{
  echo $basename
  sed -e '/^[^%]/,$d' $f
  echo

}

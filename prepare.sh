#/usr/bin/env sh
ozc -c src/DumpAST.oz -o lib/DumpAST.ozf
ozc -c src/Compile.oz -o lib/Compile.ozf
cd tests/
ozc -c TestRunner.oz


#/usr/bin/env sh
ozc -c src/Compile.oz -o lib/Compile.ozf
ozc -c src/DumpAST.oz -o lib/DumpAST.ozf
cd tests/
ozc -c TestRunner.oz


lib/Compile.ozf: src/Compile.oz lib/DumpAST.ozf
	ozc -c src/Compile.oz -o lib/Compile.ozf
lib/DumpAST.ozf: src/DumpAST.oz
	ozc -c src/DumpAST.oz -o lib/DumpAST.ozf
tests/TestRunner.ozf: tests/TestRunner.oz
	ozc -c tests/TestRunner.oz -o tests/TestRunner.ozf
tests/HelpersTests.ozf: tests/HelpersTests.oz lib/DumpAST.ozf lib/Compile.ozf
	ozc -c tests/HelpersTests.oz -o tests/HelpersTests.ozf
tests: tests/TestRunner.ozf tests/HelpersTests.ozf
	cd tests && ./run_all_tests.sh
lib/Run.ozf: src/Run.oz lib/Compile.ozf
	ozc -c src/Run.oz -o lib/Run.ozf
run: lib/Run.ozf
	ozengine lib/Run.ozf	
clean:
	rm lib/*
.PHONY : clean run tests

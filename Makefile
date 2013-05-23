lib/DeclsFlattener.ozf: src/DeclsFlattener.oz
	ozc -c src/DeclsFlattener.oz -o lib/DeclsFlattener.ozf
lib/Namer.ozf: src/Namer.oz
	ozc -c src/Namer.oz -o lib/Namer.ozf
lib/Desugar.ozf: src/Desugar.oz
	ozc -c src/Desugar.oz -o lib/Desugar.ozf
lib/Unnester.ozf: src/Unnester.oz
	ozc -c src/Unnester.oz -o lib/Unnester.ozf
lib/Globaliser.ozf: src/Globaliser.oz
	ozc -c src/Globaliser.oz -o lib/Globaliser.ozf
lib/CodeGen.ozf: src/CodeGen.oz
	ozc -c src/CodeGen.oz -o lib/CodeGen.ozf
lib/Helpers.ozf: src/Helpers.oz
	ozc -c src/Helpers.oz -o lib/Helpers.ozf
lib/Compile.ozf: src/Compile.oz lib/DumpAST.ozf lib/Helpers.ozf lib/Namer.ozf lib/DeclsFlattener.ozf lib/Desugar.ozf lib/Unnester.ozf lib/Globaliser.ozf lib/CodeGen.ozf
	ozc -c src/Compile.oz -o lib/Compile.ozf
lib/DumpAST.ozf: src/DumpAST.oz
	ozc -c src/DumpAST.oz -o lib/DumpAST.ozf
tests/TestRunner.ozf: tests/TestRunner.oz lib/Compile.ozf
	ozc -c tests/TestRunner.oz -o tests/TestRunner.ozf
tests/HelpersTests.ozf: tests/HelpersTests.oz lib/DumpAST.ozf lib/Compile.ozf
	ozc -c tests/HelpersTests.oz -o tests/HelpersTests.ozf
tests: tests/TestRunner.ozf tests/HelpersTests.ozf lib/Compile.ozf
	cd tests && ./run_all_tests.sh
test: tests/TestRunner.ozf lib/Compile.ozf
#Pass test prefix in variable test
	cd tests && ./run_one_test_definition.sh definitions/${test}*oz
testsfrom: tests/TestRunner.ozf lib/Compile.ozf
#Pass test prefix in variable test
	cd tests && ./run_test_definitions_from.sh ${from}
lib/Run.ozf: src/Run.oz lib/Compile.ozf
	ozc -c src/Run.oz -o lib/Run.ozf
lib/Compilec.ozf: src/Compilec.oz lib/Compile.ozf
	ozc -c src/Compilec.oz -o lib/Compilec.ozf
compile: lib/Compilec.ozf
		ozengine lib/Compilec.ozf
run: lib/Run.ozf
	ozengine lib/Run.ozf	
clean:
	rm -f lib/* tests/*ozf
desc:
	cd tests && ./describe_tests.sh
.PHONY : clean run tests desc

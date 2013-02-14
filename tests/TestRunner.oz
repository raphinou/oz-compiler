functor
import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzFile)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   Application(getArgs)
   DumpAST at '../lib/DumpAST.ozf'
   Debug at 'x-oz://boot/Debug'
   Compile at '../lib/Compile.ozf'
define
   AST
   proc {Equals Result Expected}
      if Result\=Expected then
         {Show '-------------------'}
         {Show 'Unexpected Result !'}
         {Show '-------------------'}
         {Show 'Expected :'}
         {Show Expected}
         {Show '...................'}
         {Show 'But got:'}
         {Show Result}
         {Show '*******************'}
         raise unexpectedResult end
      end
      {System.printInfo '.'}
   end
   PrivateNarratorO
   NarratorO = {New Narrator.'class' init(?PrivateNarratorO)}
   ListenerO = {New ErrorListener.'class' init(NarratorO)}

   fun {GetSwitch Switch}
      false
   end

   EnvDictionary = {NewDictionary}
   {Dictionary.put EnvDictionary 'Show' Show}

   case {Application.getArgs plain} of [FileName] then
     AST = {Compiler.parseOzFile FileName PrivateNarratorO
          GetSwitch EnvDictionary}
   end
   OpCodes = {Compile.genCode {DumpAST.dumpAST {Compile.globaliser {DumpAST.dumpAST {Compile.namer {Compile.declsFlattener AST.1} }}}} params() }
   {Show 'Generated OpCodes:'}
   {ForAll OpCodes Show}

   % Build top level abstraction
   Arity = 0
   PrintName = 'Q'
   DebugData = d(file:'Truc.oz' line:32 column:3)
   Switches = switches

   CodeArea VS
   {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   {Wait VS}
   {System.showInfo VS}
   Abs = {CompilerSupport.newAbstraction CodeArea [6]}
   {System.showInfo '--END DEBUG--'}
   {Abs}

end

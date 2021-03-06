functor
import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzFile)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
   Debug at 'x-oz://boot/Debug'
   Compile at './Compile.ozf'
   Pickle
   Application
define
   DestinationFile
   Result
   proc {BindResult Value}
        Result = Value
   end
%   {Debug.setRaiseOnBlock {Thread.this} true}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Boilerplate code for the parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Result
   AST
   PrivateNarratorO
   NarratorO = {New Narrator.'class' init(?PrivateNarratorO)}
   ListenerO = {New ErrorListener.'class' init(NarratorO)}

   fun {GetSwitch Switch}
      false
   end

   EnvDictionary = {NewDictionary}
   {Dictionary.put EnvDictionary 'Show' Show}

  %--------------------------------------------------------------------------------
  % The code we work on
  %--------------------------------------------------------------------------------

   %AST = {Compiler.parseOzFile 'src/run.oz' PrivateNarratorO
   %    GetSwitch EnvDictionary}

   case {Application.getArgs plain} of [FileName] then
     DestinationFile=FileName#"f"
     AST = {Compiler.parseOzFile FileName PrivateNarratorO
          GetSwitch EnvDictionary}
   end

   {System.showInfo '--------------------------------------------------------------------------------'}

   % With AST printed for debug:
   %OpCodes = {Compile.genCode {DumpAST.dumpAST {Compile.globaliser {DumpAST.dumpAST {Compile.unnester {DumpAST.dumpAST {Compile.desugar {DumpAST.dumpAST {Compile.namer {DumpAST.dumpAST {Compile.declsFlattener {DumpAST.dumpAST fApply(fConst(BindResult pos) [ AST.1 ] pos)}} }}}}}}}}} params() }
   % No AST printed:
   OpCodes = {Compile.genCode {Compile.globaliser {Compile.unnester {Compile.desugar {Compile.namer {Compile.declsFlattener fApply(fConst(BindResult pos) [ AST.1 ] pos)} }}}} params() }
   %{System.showInfo '--------------------------------------------------------------------------------'}
   %{Show 'Generate OpCodes:'}
   %{ForAll OpCodes Show}


   Arity = 0
   PrintName = 'Top Level Abstraction'
   DebugData = d(file:'run.oz' line:1 column:1)
   Switches = switches

   CodeArea VS
   {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   {Wait VS}
   %{System.showInfo VS}
   Abs = {CompilerSupport.newAbstraction CodeArea [6]}
   %{System.showInfo '--END DEBUG--'}
   {Abs}
   {Pickle.save Result DestinationFile}

   %{System.showInfo '################################################################################'}

end


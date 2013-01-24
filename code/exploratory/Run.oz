functor
import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
   Debug at 'x-oz://boot/Debug'
   Compile at './Compile.ozf'
define
   {Debug.setRaiseOnBlock {Thread.this} true}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Boilerplate code for the parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  %Code = 'local A = 5 B = 3 in {System.showInfo A + B} end'
   Code = 'local  A B=C in A=3.2   local A in A=6 end  A=7 end'


   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}


   {System.showInfo '################################################################################'}
   {DumpAST.dumpAST AST}
%   {System.showInfo '--------------------------------------------------------------------------------'}
%   %{DumpAST.dumpAST {YAssigner {Namer AST.1}}}
%   {DumpAST.dumpAST {Compile.namer AST.1 }}
   {System.showInfo '--------------------------------------------------------------------------------'}
   {Show {Compile.yAssigner {Compile.namer {Compile.declsFlattener AST.1} } } }
   {System.showInfo '--------------------------------------------------------------------------------'}
   OpCodes = {Compile.genCode {Compile.yAssigner {Compile.namer {Compile.declsFlattener AST.1} } } }
   {ForAll OpCodes Show}
%   {DumpAST.dumpAST AST}
   {System.showInfo '################################################################################'}
%   {DumpAST.dumpAST {Compile.declsFlattener AST}}

end


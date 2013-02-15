functor
import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at '../lib/DumpAST.ozf'
   Debug at 'x-oz://boot/Debug'
   Compile at '../lib/Compile.ozf'
define
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

   Code={NewCell ''}
   Result={NewCell unit}
   %--------------------------------------------------------------------------------
   % Test function PV
   %--------------------------------------------------------------------------------

   Code := 'local  A B=C C=4 in A=3.2 B=4 end'

   AST = {Compiler.parseOzVirtualString @Code PrivateNarratorO
          GetSwitch EnvDictionary}
   Result := {Compile.pvs AST.1.1}

   {Equals {List.length @Result} 3}
   {Equals {List.map @Result fun {$ fVar(Name _)} Name end} 'A'|'B'|'C'|nil}


   %--------------------------------------------------------------------------------
   % Test function UnwrapFAnd
   %--------------------------------------------------------------------------------
   % FIXME: good result, but fails....
   {Equals {Compile.unWrapFAnd fAnd(first fAnd(fAnd(second third) fAnd(fourth fifth)))} first|second|third|fourth|fifth|nil }
   {Equals {Compile.unWrapFAnd fProc()} 'fProc'|nil}

end

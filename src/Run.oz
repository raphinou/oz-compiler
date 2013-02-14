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
   % next step: proc ... in ... end
   Code = 'local
               P1 A
            in
               proc {P1 A11}
                  P2 C
               in
                  C=2
                  proc {P2 A21}
                    {Show A}
                    {Show A}
                    {Show C}
                  end
                  {P2 A11}
               end
               A=3
               {P1 A}
            end'

   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}


   {System.showInfo '################################################################################'}

   %{DumpAST.dumpAST AST.1 }
   %{DumpAST.dumpAST {Compile.declsFlattener AST.1 }}
   %{DumpAST.dumpAST {Compile.namer {Compile.declsFlattener AST.1 }}}
   {System.showInfo '--------------------------------------------------------------------------------'}
   %{DumpAST.dumpAST {Compile.globaliser {Compile.namer {Compile.declsFlattener AST.1 }}}}
   {System.showInfo '--------------------------------------------------------------------------------'}

   OpCodes = {Compile.genCode {DumpAST.dumpAST {Compile.globaliser {DumpAST.dumpAST {Compile.namer {Compile.declsFlattener AST.1} }}}} params() }
   {System.showInfo '--------------------------------------------------------------------------------'}
   %{Show 'Generate OpCodes:'}
   %{ForAll OpCodes Show}


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

   {System.showInfo '################################################################################'}

end


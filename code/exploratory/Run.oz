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
  %Code = 'local  A B=3 in A=3.2   local A in A=6 end {Show A}   end'
  % Code = ' local
  %             A=5 P B C
  %          in
  %             proc {P V}
  %                {Show B}
  %                {Show C}
  %                {Show V}
  %             end
  %             B = 7
  %             C = 9
  %             {P A}
  %             {For 1 5 1 P}
  %          end'

   % next step: proc ... in ... end
   %Code = 'local A=5 B=6 in local B=7 in {Show A} {Show B} end {Show A} end'
   %Code = 'local A=5 in local B=7 in local C=8 in {Show A} {Show B} {Show C} end end end'
   Code = 'local
               A P B
            in
               proc {P V}
                  T
               in
                  {Show A}
                  proc {T U}
                    Text=44
                  in
                    {Show Text}
                    {Show U}
                  end
                  {T A}
               end
               A = 5
               B = 7
               {P A}
               {For 1 5 1 P}
            end'

   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}


   {System.showInfo '################################################################################'}

   %{DumpAST.dumpAST AST.1 }
   %{DumpAST.dumpAST {Compile.declsFlattener AST.1 }}
   {DumpAST.dumpAST {Compile.namer {Compile.declsFlattener AST.1 }}}
   {System.showInfo '--------------------------------------------------------------------------------'}
   {DumpAST.dumpAST {Compile.globaliser {Compile.namer {Compile.declsFlattener AST.1 }}}}
   {System.showInfo '--------------------------------------------------------------------------------'}

   OpCodes = {Compile.genCode {Compile.globaliser {Compile.namer {Compile.declsFlattener AST.1} }} params() }
   {System.showInfo '--------------------------------------------------------------------------------'}
   {Show 'Generate OpCodes:'}
   {ForAll OpCodes Show}


   Arity = 0
   PrintName = 'Q'
   DebugData = d(file:'Truc.oz' line:32 column:3)
   Switches = switches

   CodeArea VS
   {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   {Wait VS}
   {System.showInfo VS}
   Abs = {CompilerSupport.newAbstraction CodeArea [6]}
   {Abs}

   {System.showInfo '################################################################################'}

end


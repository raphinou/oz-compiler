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
  %Code = '{Show 5+2*3}'
  %Code = 'local X in X=5+3*2 {Show X} end'
  Code = 'local
             Sum
         in
            proc {Sum O1 O2}
               {Show willShowSum}
               {Show O1+O2}
            end
            {Sum 1+2 3*(2+1)}
         end'


  %Code ='local
  %          A = 8
  %          B = 2
  %          Add Sub Mult Div
  %       in
  %          Add = A + B
  %          Sub = A - B
  %          Mult= A * B
  %          Div = A div B
  %          {Show Add}
  %          {Show Sub}
  %          {Show Mult}
  %          {Show Div}
  %       end  '
   % next step: proc ... in ... end
   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}


   {System.showInfo '################################################################################'}

%   _={DumpAST.dumpAST AST.1 }
%   {System.showInfo '--------------------------------------------------------------------------------'}
%   %_={DumpAST.dumpAST {Compile.desugar {Compile.declsFlattener AST.1 }}}
%   _={DumpAST.dumpAST {Compile.namer {Compile.desugar {Compile.declsFlattener AST.1 }}}}
%   {System.showInfo '--------------------------------------------------------------------------------'}
%   _={DumpAST.dumpAST {Compile.unnester {Compile.namer {Compile.desugar {Compile.declsFlattener AST.1 }}}}}
   {System.showInfo '--------------------------------------------------------------------------------'}
   %{DumpAST.dumpAST {Compile.globaliser {Compile.unnester {Compile.namer {Compile.declsFlattener AST.1 }}}}}
   {System.showInfo '--------------------------------------------------------------------------------'}

   OpCodes = {Compile.genCode {DumpAST.dumpAST {Compile.globaliser {DumpAST.dumpAST {Compile.unnester {Compile.desugar {Compile.namer {Compile.declsFlattener AST.1} }}}}}} params() }
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


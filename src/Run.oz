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
  %Code = 'local
  %          V
  %       in
  %          V = local A=2 in A+2 end + local B=2 in B end
  %          {Show V}
  %       end'

%Code='
%local
%   proc {MakeAdder X ?P}
%      proc {P Y ?R}
%         R = X + Y
%      end
%   end
%in
%   {Show 1}
%   {Show {{MakeAdder 3} 4}}
%   {Show 2}
%end'

Code ='local
      A = 5
      Add Sub
   in
      {Show {Add {Sub 3 2} 2}}
   end
   '

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
%   {System.showInfo '--------------------------------------------------------------------------------'}
%    _={DumpAST.dumpAST {Compile.unnester {DumpAST.dumpAST  {Compile.desugar {DumpAST.dumpAST {Compile.namer {Compile.declsFlattener AST.1} }}}}}}
%
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


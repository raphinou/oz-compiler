%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Parse an oz file and shows the resulting AST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
functor

import
   Narrator('class')
   ErrorListener('class')
   Application(getArgs)
   Compiler(parseOzFile)
   Inspector(inspect)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
define
   PrivateNarratorO
   AST
   NarratorO = {New Narrator.'class' init(?PrivateNarratorO)}
   ListenerO = {New ErrorListener.'class' init(NarratorO)}

   fun {GetSwitch Switch}
      false
   end

   %Code = 'local A = 5 B = 3 in {Show A + B} end'

   EnvDictionary = {NewDictionary}
   {Dictionary.put EnvDictionary 'Show' Show}
   case {Application.getArgs plain} of [FileName] then
     AST = {Compiler.parseOzFile FileName PrivateNarratorO
          GetSwitch EnvDictionary}
   end

   if {ListenerO hasErrors($)} then
      {System.printInfo {ListenerO getVS($)}}
      {ListenerO reset()}
   else
      {Show AST}
      {DumpAST.dumpAST AST _}
   end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Below is and example of assembling generated opcode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   % A generer
%   Arity = 2 % argument count
%   OpCodes = [
%              move(g(0) x(2))
%              callBuiltin(k(Number.'+') [x(2) x(0) x(3)])
%              unify(x(3) x(1))
%              return
%             ]
%   PrintName = 'Q'
%   DebugData = d(file:'Truc.oz' line:32 column:3)
%   Switches = switches
%
%   CodeArea VS
%   {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
%
%   {Show CodeArea}
%   {Wait VS}
%   {System.showInfo VS}
%
%   Abs = {CompilerSupport.newAbstraction CodeArea [6]}
%   {Show {Abs 3}}
end

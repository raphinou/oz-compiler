functor 

import 
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
define 
   PrivateNarratorO
   NarratorO = {New Narrator.'class' init(?PrivateNarratorO)}
   ListenerO = {New ErrorListener.'class' init(NarratorO)}

   fun {GetSwitch Switch}
      false
   end

   Code = 'local A = 5 B = 3 in {Show A + B} end'

   EnvDictionary = {NewDictionary}
   {Dictionary.put EnvDictionary 'Show' Show}
 
   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}
   
   if {ListenerO hasErrors($)} then 
      {System.printInfo {ListenerO getVS($)}}
      {ListenerO reset()}
   else
      {Show AST}
   end

% 'fLocal'('fAnd'('fEq'('fVar'('A' 'pos'('top level' 1 6 'top level' 1 7)) 'fInt'(5 'pos'('top level' 1 10 'top level' 1 11)) 'pos'('top level' 1 6 'top level' 1 11)) 'fEq'('fVar'('B' 'pos'('top level' 1 12 'top level' 1 13)) 'fInt'(3 'pos'('top level' 1 16 'top level' 1 17)) 'pos'('top level' 1 12 'top level' 1 17))) 'fApply'('fVar'('Show' 'pos'('top level' 1 22 'top level' 1 26)) 'fOpApply'('+' 'fVar'('A' 'pos'('top level' 1 27 'top level' 1 28))|'fVar'('B' 'pos'(...))|'nil' 'pos'('top level' 1 27 'top level' 1 32))|'nil' 'pos'('top level' 1 21 'top level' 1 33)) 'pos'('top level' 1 6 'top level' 1 33))|'nil'
   
   % A generer
   Arity = 2 % argument count
   OpCodes = [
              move(g(0) x(2))
              callBuiltin(k(Number.'+') [x(2) x(0) x(3)])
              unify(x(3) x(1))
              return
             ]
   PrintName = 'Q'
   DebugData = d(file:'Truc.oz' line:32 column:3)
   Switches = switches
   
   CodeArea VS
   {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}

   {Show CodeArea}

% <CodeArea for <P/2 Q> 'd'('column':3 'file':'Truc.oz' 'line':32)>
% %% Code area for Q in file "Truc.oz", line 32, column 3
% %% Code Size:
% 11 % bytecode elements
%                'move'('g'(0) 'x'(2))
%                'custom'(129 'x'(2) 'x'(0) 'x'(3))
%                'unify'('x'(3) 'x'(1))
%                'return'
   
	     {Wait VS}
	     {System.showInfo VS}

	     Abs = {CompilerSupport.newAbstraction CodeArea [6]}
	     {Show {Abs 3}}

% Gives 9 as expected
	     
end

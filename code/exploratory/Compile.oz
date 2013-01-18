functor 

import 
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
define 
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
  %Code = 'local A = 5 B = 3 in {Show A + B} end'
  Code = 'local A = 5 in {Show A} end'

  AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
         GetSwitch EnvDictionary}

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Definition of the classes we need
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  class Node
    attr
      parent
      children
    meth init(Parent Children<=nil)
      parent:=Parent
      children:=children
    end
  end

  class Program from Node
    attr 
      ast
      locals % Can it only have locals???
    meth init(AST)
      ast:=AST
      locals:=nil
    end
    % FIXME : find a better way to do this
    meth addLocal(C)
      NewList
    in
      {List.append @locals [C] NewList}
      locals:=NewList
    end
  end

  class Abstraction from Node
    attr 
      formals
      locals
      globals
      ast
      codeArea
    meth init()
      skip
    end
  end

  class Instruction from Node
    attr
      type  % expression or statement
    meth init()
      skip
    end
  end
  
  class LocalInstr from Instruction
    attr
      decls
      body
    meth init(Parent)
      parent:=Parent
      decls:=nil
      body:=nil
    end
    meth set(X V)
      X:=V
    end
    meth get(X ?V)
      V=@X
    end
  end

  class UnificationInstr from Instruction
    attr
      lhs
      rhs
    meth init(Parent Lhs Rhs)
      parent:=Parent
      lhs:=Lhs
      rhs:=Rhs
    end
  end

  class SkipStatement from Instruction
    meth init()
      skip 
    end
  end


  class CodeArea
    attr 
      opCodes
      registers
    meth init()
      skip
    end
  end



  % Debug output
  proc {D S}
    {Show S}
  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Actual work happening
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  fun {Record2ObjectsAST AST Parent}
    fun {HandleLocal AST P}
      L 
    in
      {D 'Handle local statement'}
      % the first feature is the declarations
      % the second feature is the code
      L = {New LocalInstr init(Parent)}
      % Handle the declarations
      {D 'Declarations of Local'}
      { L set(decls {Record2ObjectsAST AST.1 L}) }
      %% Handle the body
      {D 'Body of Local'}
      L.body:={Record2ObjectsAST AST.2 L}
      L
    end
    fun {HandleUnification AST P}
      {D 'Handle Unification'}
      % the first feature is lhs
      % second is rhs
      nil
    end
  in
    if {List.is AST} then
      {Show 'WE GOT A LIST'}
    elseif {Record.is AST} then
      {Show 'WE GOT A RECORD'}
    end

    case {Label AST}
    of fLocal then
      {HandleLocal AST Parent}
    [] fVar then
      {System.showInfo 'Variable'}
    [] fEq then
      {HandleUnification AST Parent}
    [] unit then 
      nil
    [] pos then
      nil
    else
      {Show 'unknown'}
      {Show AST}
      nil
    end
  end

  P = {New Program init(AST)} 
  ThisLocal = {Record2ObjectsAST AST.1 P}
  {P addLocal(ThisLocal)}




  %--------------------------------------------------------------------------------
  % Printing the AST
  %--------------------------------------------------------------------------------

  {System.showInfo '################################################################################'}
  {System.showInfo '                                     AST                                        '}
  {System.showInfo '################################################################################'}
  if {ListenerO hasErrors($)} then 
     {System.printInfo {ListenerO getVS($)}}
     {ListenerO reset()}
  else
     {DumpAST.dumpAST AST}
  end
end

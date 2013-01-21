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
      topLevelAbstraction
    meth init(AST)
      ast:=AST
      locals:=nil
      topLevelAbstraction:={New Abstraction init(AST)}
    end
    meth tla(?R)
      R=@topLevelAbstraction
    end
    % FIXME : find a better way to do this? See also Apply add argument
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
      body
      codeArea
    meth init(AST)
      body:=AST
    end
    meth setBody(Body)
      body:=Body
    end
  end

  class Instruction from Node
    attr
      type  % expression or statement
    meth init()
      skip
    end
    meth set(X V)
      X:=V
    end
    meth get(X ?V)
      V=@X
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
  end

  class UnificationInstr from Instruction
    attr
      lhs
      rhs
    meth init(Parent)
      parent:=Parent
      lhs:=nil
      rhs:=nil
    end
  end

  class SkipStatement from Instruction
    meth init()
      skip 
    end
  end

  % FIXME: not clean to inherit from Instruction, is it?
  class Variable from Instruction 
    attr
      name
    meth init(Name)
      name:=Name
    end
  end

  class Integer from Instruction 
    attr
      value
    meth init(Value)
      value:=Value
    end
  end

  class ApplyInstr from Instruction
    attr
      command
      args
    meth init(P)
      parent:=P
      args := nil
    end
    %FIXME: better way? See also program add local
    meth addArgument(A)
      NewList
    in
      {List.append @args [A] NewList}
      args:=NewList
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
      I 
    in
      {D 'Handle local statement'}
      % the first feature is the declarations
      % the second feature is the code
      I = {New LocalInstr init(Parent)}
      % Handle the declarations
      {D 'Declarations of Local'}
      {I set(decls {Record2ObjectsAST AST.1 I}) }
      %% Handle the body
      {D 'Body of Local'}
      {I set(body {Record2ObjectsAST AST.2 I})}
      I
    end
    fun {HandleUnification AST P}
      I 
    in
      % the first feature is lhs
      % second is rhs
      {D 'Handle Unification'}
      I = {New UnificationInstr init(P)}
      % Handle the rhs
      {D 'LHS of Unification'}
      {I set(lhs {Record2ObjectsAST AST.1 I}) }
      %% Handle the body
      {D 'RHS of Unification'}
      {I set(rhs {Record2ObjectsAST AST.2 I}) }
      I
    end
    fun {HandleVar AST P}
      I
    in
      % first feature is its name
      I = {New Variable init(AST.1)}
    end
    fun {HandleInt AST P}
      I
    in
      % first feature is its value
      I = {New Integer init(AST.1)}
    end
    fun {HandleApply AST P}
      I
    in
      % first feature is the command to apply
      % second feature is a list of arguments
      
      I = {New ApplyInstr init(P)}
      % first the command
      {D 'Command'}
      {I set( command {Record2ObjectsAST AST.1 I})}
      % then each argument
      {D 'Args'}
      for A in AST.2 do
        {I addArgument({Record2ObjectsAST A I})}
      end
      I
    end
  in
    if {List.is AST} then
      {Show 'WE GOT A LIST'}
    elseif {Record.is AST} then
      {Show 'WE GOT A RECORD'#{Label AST}}
    end

    case {Label AST}
    of fLocal then
      {HandleLocal AST Parent}
    [] fVar then
      {HandleVar AST Parent}
    [] fInt then
      {HandleInt AST Parent}
    [] fEq then
      {HandleUnification AST Parent}
    [] fApply then
      {HandleApply AST Parent}
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


  P = {New Program init(AST)} 
  ThisLocal = {Record2ObjectsAST AST.1 {P tla($)}}
  {{P tla($)} setBody(ThisLocal)}
  %{P addLocal(ThisLocal)}






end

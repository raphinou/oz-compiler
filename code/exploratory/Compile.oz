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
  %Code = 'local A = 5 B = 3 in {System.showInfo A + B} end'
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
      label
    meth init(Parent Children<=nil)
      parent:=Parent
      children:=children
    end
    meth label(?L)
      L=@label
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
      topLevelAbstraction:={New Abstraction init(AST tla)}
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

    meth print(Indent)
      {System.showInfo Indent#'*Program tla'}
      {@topLevelAbstraction print('  '#Indent)}
    end

    meth visit(F)
      {@topLevelAbstraction visit(F self)}
    end
  end

  class Abstraction from Node
    attr 
      formals
      locals
      globals
      body
      codeArea
    meth init(AST L)
      body:=AST
      label:=L
    end
    meth setBody(Body)
      body:=Body
    end
    meth print(Indent)
      {System.showInfo Indent#'*Abstraction'}
      {System.showInfo Indent#'Abstraction body'}
      {@body print('  '#Indent)}
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
      {@body visit(F NewNode)}
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
    meth append(Attr V)
      % FIXME if Attr not list raise exception
      NewList
    in
      {List.append @Attr [V] NewList}
      Attr:=NewList
    end
  end
  
  class LocalInstr from Instruction
    attr
      decls
      body
    feat
      type:fLocal
    meth init(Parent L)
      parent:=Parent
      decls:=nil
      body:=nil
      label:=L
    end
    meth print(Indent)
      {System.showInfo Indent#'*Local'}
      {System.showInfo Indent#'Local declarations'}
      for Decl in @decls do 
        {Decl print('  '#Indent)}
      end
      {System.showInfo Indent#'Local body'}
      for Instr in @body do 
        {Instr print('  '#Indent)}
      end
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
      for Decl in @decls do 
        {Decl visit(F NewNode)}
      end
      for Instr in @decls do 
        {Instr visit(F NewNode)}
      end
    end

  end

  class UnificationInstr from Instruction
    attr
      lhs
      rhs
    meth init(Parent L)
      parent:=Parent
      lhs:=nil
      rhs:=nil
      label:=L
    end
    meth print(Indent)
      {System.showInfo Indent#'*Unification'}
      {System.showInfo Indent#'Unification LHS'}
      {@lhs print('  '#Indent)}
      {System.showInfo Indent#'Unification RHS'}
      {@rhs print('  '#Indent)}
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
      {@lhs visit(F NewNode)}
      {@rhs visit(F NewNode)}
    end

  end

  class SkipStatement from Instruction
    meth init(P L<=fSkip)
      label:=L
      skip 
    end
    meth print(Indent)
      {System.showInfo Indent#'*Skip statement'}
    end
    meth visit(F P)
      NewNode
    in
      NewNode={F {self label($)} self P}
      skip
    end
  end

  % FIXME: not clean to inherit from Instruction, is it?
  class Variable from Instruction 
    attr
      name
    meth init(Name L)
      name:=Name
      label:=L
    end
    meth print(Indent)
      {System.showInfo Indent#'*Variable '#@name}
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
    end
  end

  class Integer from Instruction 
    attr
      value
    meth init(Value L)
      value:=Value
      label:=L
    end
    meth print(Indent)
      {System.showInfo Indent#'*Integer '#@value}
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
    end
  end

  class ApplyInstr from Instruction
    attr
      command
      args
    meth init(P L)
      parent:=P
      args := nil
      label:=L
    end
    %FIXME: better way? See also program add local
    meth addArgument(A)
      NewList
    in
      {List.append @args [A] NewList}
      args:=NewList
    end
    meth print(Indent)
      {System.showInfo Indent#'*Apply'}
      {System.showInfo Indent#'Apply command'}
      {@command print('  '#Indent)}
      {System.showInfo Indent#'Apply args'}
      for A in @args do
        {A print('  '#Indent)}
      end
    end
    meth visit(F P)
      NewNode
    in
      NewNode = {F {self label($)} self P}
      {@command visit(F NewNode)}
      for A in @args do
        {A visit(F NewNode)}
      end
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
    {System.showInfo S}
  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Actual work happening
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  fun {Record2ObjectsAST AST Parent}
    fun {HandleLocal AST P L}
      I 
    in
      {D 'Handle local statement'}
      % the first feature is the declarations
      % the second feature is the code
      I = {New LocalInstr init(Parent L)}
      % Handle the declarations
      {D 'Declarations of Local'}
      {I append(decls {Record2ObjectsAST AST.1 I}) }
      %% Handle the body
      {D 'Body of Local'}
      {I append(body {Record2ObjectsAST AST.2 I})}
      I
    end
    fun {HandleUnification AST P L}
      I 
    in
      % the first feature is lhs
      % second is rhs
      {D 'Handle Unification'}
      I = {New UnificationInstr init(P L)}
      % Handle the rhs
      {D 'LHS of Unification'}
      {I set(lhs {Record2ObjectsAST AST.1 I}) }
      %% Handle the body
      {D 'RHS of Unification'}
      {I set(rhs {Record2ObjectsAST AST.2 I}) }
      I
    end
    fun {HandleVar AST P L}
      I
    in
      % first feature is its name
      I = {New Variable init(AST.1 L)}
    end
    fun {HandleInt AST P L}
      I
    in
      % first feature is its value
      I = {New Integer init(AST.1 L)}
    end
    fun {HandleApply AST P L}
      I
    in
      % first feature is the command to apply
      % second feature is a list of arguments
      
      I = {New ApplyInstr init(P L)}
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
    L
  in
    if {List.is AST} then
      {System.showInfo 'WE GOT A LIST'}
    elseif {Record.is AST} then
      {System.showInfo 'WE GOT A RECORD'#{Label AST}}
    end

    case L={Label AST}
    of fLocal then
      {HandleLocal AST Parent L}
    [] fVar then
      {HandleVar AST Parent L}
    [] fInt then
      {HandleInt AST Parent L }
    [] fEq then
      {HandleUnification AST Parent L}
    [] fApply then
      {HandleApply AST Parent L}
    [] unit then 
      nil
    [] pos then
      nil
    else
      {System.showInfo 'unknown'}
      {System.showInfo AST}
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

  {P print('')}

  % Dumb visitor of nodes, to validate it works
  fun{F Label Node Parent}
    {Show Label}
    Node
  end

  % Visitor that moves unifications from declaration to body of 'local'
  fun{Namer Label Node Parent}
    case Label of
    fLocal then
         {Show fLocal}
        for C in {Node get(decls $)} do
          case {C label($)} of 
            fVar then
              {Show 'Variable declared:'#{C get(name $)}}
          []fEq then
              {Show unification}
              {C print('')}
%              put unification in body
          end
        end
    else
        skip
    end
    Node
  end

  {P visit(Namer)}
  {System.showInfo '################################################################################'}
  {P print('')}






end

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
  Code = 'local  A in A=3  local A in A=6 end end'


  AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
         GetSwitch EnvDictionary}

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Support classes
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  class Symbol
    attr
      name
      pos
    meth init(Name Pos)
      name:=Name
      pos:=Pos
    end
    meth toVS(?R)
      pos(File Line Col _ _ _)=@pos
    in
      R="'Symbol for "#@name#" from "#File#"("#Line#","#Col#")'"
    end
  end

  class Environment
    attr
      dict
    meth init()
      dict:={NewDictionary}
    end

    meth addOrGetSymbol(Name Pos ?Res)
      if {Dictionary.member @dict Name} then
        Res={Dictionary.get @dict Name}
      else
        NewSymbol={New Symbol init(Name Pos)}
      in
        {Dictionary.put @dict Name NewSymbol}
        Res=NewSymbol
      end
    end
    meth getSymbol(Name ?R)
      R={Dictionary.get @dict Name}
    end
    meth setSymbol(Name Pos ?NewSymbol)
      NewSymbol = { New Symbol init(Name Pos)}
      {Dictionary.put @dict Name NewSymbol}
    end
  end
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Actual work happening
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  fun {Pass AST}
%    % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
%    % AST = the record
%    % Env = mapping of var names to symbols built in parents
%    % InDecl = should new vars be mapped to new symbols, ie are we in declarations?
%    fun {PassInt AST Env InDecl}
%      case AST 
%      of fLocal(Decl Body Pos) then
%        {Show fLocal}
%        flocal(
%          {PassInt Decl Env true}
%          {PassInt Body Env false}
%          Pos
%        )
%      [] fVar(Name Pos) then Sym in
%        {Show 'fVar declaration or use'}
%        if InDecl then
%          Sym={Env setSymbol(Name Pos $)}
%        else
%          Sym={Env getSymbol(Name $)}
%        end
%        fVar( Sym Pos)
%      [] fEq(LHS RHS Pos) then
%        {Show 'Unification'}
%        feq(
%          {PassInt LHS Env InDecl}
%          {PassInt RHS Env InDecl}
%          Pos
%        )
%      [] fInt(Value _) then
%        {Show 'Integer '#Value}
%        AST
%      [] fAnd(First Second) then
%        fAnd({PassInt First Env InDecl} {PassInt Second Env InDecl})
%      end
%    end
%  in
%    {PassInt AST {New Environment init()} false}
%  end

  % This is the function to use for default handling of an AST.
  % Eg, the namer only has to do specific work on fLocal and fVar,
  % for which it has specific code. But for all other labels, it
  % just needs to recursively call itself on all features, which 
  % is easily done with this function.
  fun {DefaultPass F AST Env InDecl}
    if {Record.is AST} then
      {Record.map AST fun {$ I} {F I Env InDecl} end}
    else
      AST
    end
  end

  % The namer replaced variable names with a Symbol instance, all identical
  % variable instances referencing the same symbol.
  fun {Namer AST}
    % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
    % AST = the record
    % Env = mapping of var names to symbols built in parents
    % InDecl = should new vars be mapped to new symbols, ie are we in declarations?
    fun {NamerInt AST Env InDecl}
      case AST 
      of fLocal(Decl Body Pos) then
        flocal(
          {NamerInt Decl Env true}
          {NamerInt Body Env false}
          Pos
        )
      [] fVar(Name Pos) then Sym in
        if InDecl then
          Sym={Env setSymbol(Name Pos $)}
        else
          Sym={Env getSymbol(Name $)}
        end
        fVar( Sym Pos)
      else
        {DefaultPass NamerInt AST Env InDecl}
      end
    end
  in
    {NamerInt AST {New Environment init()} false}
  end

  {System.showInfo '################################################################################'}
  {DumpAST.dumpAST AST}
  {System.showInfo '--------------------------------------------------------------------------------'}
  {DumpAST.dumpAST {Namer AST.1}}
  {System.showInfo '################################################################################'}
end

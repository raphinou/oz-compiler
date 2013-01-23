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
   Code = 'local  A in A=3   local A in A=6 end  A=7 end'


   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Support classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   class Accessors
      meth set(Attr Value)
         Attr:=Value
      end
      meth get(Attr ?R)
         R=@Attr
      end
   end


   class Symbol from Accessors
      attr
         name
         pos
         yindex
      meth init(Name Pos)
         name:=Name
         pos:=Pos
         yindex:=nil
      end
      meth toVS(?R)
         pos(File Line Col _ _ _)=@pos
      in
         R="'Sym "#@name#"@"#File#"("#Line#","#Col#") y:"#@yindex#"'"
      end
      meth hasYIndex(?B)
         B=(@yindex\=nil)
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
%        fLocal(
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
   fun {DefaultPass F AST Params}
      if {Record.is AST} then
         {Record.map AST fun {$ I} {F I Params} end}
      else
         AST
      end
   end

  % The namer replaces variable names with a Symbol instance, all identical
  % variable instances referencing the same symbol.
   fun {Namer AST}
    % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
    % AST = the record
    % Params is a record with 2 features:
    %   env = mapping of var names to symbols built in parents
    %   indecls = should new vars be mapped to new symbols, ie are we in declarations?
      fun {NamerInt AST Params}
         case AST
         of fLocal(Decl Body Pos) then
            fLocal(
               {NamerInt Decl {Record.adjoin Params params(indecls:true)}}
               {NamerInt Body {Record.adjoin Params params(indecls:false)}}
               Pos
               )
         [] fVar(Name Pos) then Sym in
            if Params.indecls then
               Sym={Params.env setSymbol(Name Pos $)}
            else
               Sym={Params.env getSymbol(Name $)}
            end
            fVar( Sym Pos)
         else
            {DefaultPass NamerInt AST Params}
         end
      end
   in
      {NamerInt AST params(env:{New Environment init()} indecls:false)}
   end

  % traverses the tree and assigns Y registers to variables with no Y index yet
   fun {YAssigner AST}
    % initialise index value to 1
      Index = {NewCell 1}
      fun {YAssignerInt AST Params}
         case AST
         of fVar(Sym _) then
        %only when we see a variable with no y assigned, assign it
            if {Sym get(yindex $)}==nil then
               { Sym set(yindex @Index)}
               Index:=@Index+1
            end
            AST
         else
            {DefaultPass YAssignerInt AST Params}
         end
      end
   in
      {YAssignerInt AST unit}
   end

   fun {CodeGen AST}

      fun {CodeGenInt AST Params}
         case AST
         of fLocal(Decls Body _) then
            {CodeGenInt Decls {Record.adjoin Params params(indecls: true)}}#' '#{CodeGenInt Body Params}
         [] fVar(Sym _) then
            if Params.indecls then
               'createVar(y('#{Sym get(yindex $)}#'))\n'
            else
               'y('#{Sym get(yindex $)}#')'
            end
         [] fAnd(First Second ) then
            {CodeGenInt First Params}#'\n'#{CodeGenInt Second Params}
         [] fEq(LHS RHS _) then
            'unify('#{CodeGenInt LHS Params}#' '#{CodeGenInt RHS Params}#')\n'
         [] fInt(Value _) then
            'k('#Value#')'
         else
            {Show 'missing clause for '#{Label AST}}
            nil
         end
      end
   in
      {CodeGenInt AST params(indecls:false)}
   end

   {System.showInfo '################################################################################'}
   {DumpAST.dumpAST AST}
   {System.showInfo '--------------------------------------------------------------------------------'}
   {DumpAST.dumpAST {YAssigner {Namer AST.1}}}
   {System.showInfo '################################################################################'}
   {System.showInfo {CodeGen {YAssigner {Namer AST.1}}} }
end

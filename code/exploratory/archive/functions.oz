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
            Res
         in
            {Params.env backup()}
            Res=fLocal(
               {NamerInt Decl {Record.adjoin Params params(indecls:true)}}
               {NamerInt Body {Record.adjoin Params params(indecls:false)}}
               Pos
               )
            {Params.env restore()}
            Res
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

   %% generates code to send to assembler
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

   % Will apply the function F under the feature {Label AST} in FsR. FsR has label fs.
   % This function F should take as arguments:
   %  - the record it will work on (AST)
   %  - a parameters record
   %  - a function, which will be Pass itself. The goal is that Pass is called on the features of AST by F.
   % FsR also has a feature params, which is the initial parameters to use
   fun {Pass AST FsR Params}
      L
   in
      if {Record.is AST} andthen {List.member L={Label AST} {Record.arity FsR}} then
         {FsR.L  AST Params Pass}
      elseif {Record.is AST} then
         {Record.map AST fun {$ I} {Pass I FsR Params} end}
      else
         AST
      end
   end


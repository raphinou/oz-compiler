functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
export
   namer: Namer
   genCode: GenCode
   declsFlattener: DeclsFlattener
   pv: PV
define


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
         backups
      meth init()
         dict:={NewDictionary}
         backups:=nil
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
      meth hasSymbol(Name ?R)
        {Dictionary.member @dict Name R}
      end
      meth setSymbol(Name Pos ?NewSymbol)
         NewSymbol = { New Symbol init(Name Pos)}
         {Dictionary.put @dict Name NewSymbol}
      end
      meth backup()
         backups:={Dictionary.clone @dict}|@backups
      end
      meth restore()
         dict:=@backups.1
         backups:=@backups.2
      end
   end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Actual work happening
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % This is the function to use for default handling of an AST.
  % Eg, the namer only has to do specific work on fLocal and fVar,
  % for which it has specific code. But for all other labels, it
  % just needs to recursively call itself on all features, which
  % is easily done with this function.
   fun {DefaultPass AST F Params}
      % beware of the order. a record is also a list!
      if {List.is AST} then
         {List.map AST fun {$ I} {F I Params} end}
      elseif {Record.is AST} then
         {Record.map AST fun {$ I} {F I Params} end}
      else
         AST
      end
   end
   % see http://www.mozart-oz.org/documentation/notation/node6.html
   fun {PV AST}
      case AST
      of fLocal(Decls Body _) then
         % {PV Body} - {PV Decls}
         {Show fLocal}
         {Show 'fLocal returns'#{Record.subtractList{PV Body}  {Record.arity {PV Decls}}}}
         {Record.subtractList{PV Body}  {Record.arity {PV Decls}}}
      [] fVar(Name _) then
         {Show fVar#' '#Name}
         pv(Name:unit)
      [] fAnd(First Second ) then
         %{PV First} + {PV Second}
         {Show fAnd}
         {Record.adjoin {PV First}  {PV Second}}
      [] fEq(LHS RHS _) then
         {Show fEq}
         {Record.adjoin {PV LHS}  {PV RHS}}
      else
         {Show 'else'}
         {Show AST}
         pv()
      end
   end



   % Leaves only declarations in the first feature of fLocal and moves all code
   % to the second feature

   %#######################
   fun {DeclsFlattener AST}
   %#######################

      F = DeclsFlattenerInt
      fun {DeclsFlattenerInt AST Params}
         case AST
         %------------------------
         of fLocal(Decls Body Pos) then
         %------------------------

            Acc=params(acc:{NewCell nil})
            FinalAcc
            NewDecls
            NewBody
            Res
         in
            % this call will collect in Acc.acc all code to add to the body
            NewDecls={F Decls  Acc}
            % list from which we'll build the fAnds.
            % The original Body is the last part of the body's code
            FinalAcc=Body|@(Acc.acc)
            % buidl the fAnd records
            NewBody={List.foldL FinalAcc.2 fun {$ A I} fAnd(I A) end FinalAcc.1}
            % Put all transformed parts in the new fLocal
            Res = fLocal(
               NewDecls
               NewBody
               Pos
            )

         %------------------
         [] fEq(LHS RHS Pos) then
         %------------------
            (Params.acc):= AST|@(Params.acc)
            {F LHS  Params}

         %---
         else
         %---
            {DefaultPass AST F Params}
         end
      end
   in
      {DeclsFlattenerInt AST unit}
   end


   % The namer replaces variable names with a Symbol instance, all identical
   % variable instances referencing the same symbol.
   % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
   % AST = the record
   % Params is a record with 2 features:
   %   env = mapping of var names to symbols built in parents
   %   indecls = should new vars be mapped to new symbols, ie are we in declarations?

   % FIXME : we add Show manually to the base environment.
   AugmentedBase={AdjoinAt Base 'Show' Show}

   %##############
   fun {Namer AST}
   %##############
      F=NamerInt
      fun {NamerInt AST Params}
         case AST
         %-----------------------
         of fLocal(Decl Body Pos) then
         %-----------------------
            Res
         in
            {Params.env backup()}
            Res=fLocal(
               {F Decl  {Record.adjoin Params params(indecls:true)}}
               {F Body  {Record.adjoin Params params(indecls:false)}}
               Pos
               )
            {Params.env restore()}
            Res

         %---------------------------------
         [] fProc(Name Args Body Flags Pos) then
         %---------------------------------
            % We need define new symbols for the proc name and the arguments
            % In both these cases we set procdecl to true
            NewParams={Record.adjoin Params params(procdecl:true)}
            %GOrigins is the list of symbols that are used in the procedure
            %body, but that are globals
         in
            fProc(
               % The procedure's variable has to be declared explicitely
               {F Name Params}
               {List.map Args fun {$ I} {F I NewParams} end }
               {F Body Params}
               Flags
               Pos
            )


         %------------------
         [] fEq(LHS RHS Pos) then
         %------------------
            if Params.indecls then
               % in declarations, only decend in the LHS because only the LHS variables are declared
               fEq(
                  {F LHS  Params}
                  RHS
                  Pos
               )
            else
               fEq(
                  {F LHS  Params}
                  {F RHS  Params}
                  Pos
               )
            end

         %----------------
         [] fVar(Name Pos) then
         %----------------
            Sym
         in
            % in declarations or procedure procdecl, assign symbol to variables
            if Params.indecls orelse ({HasFeature Params procdecl}  andthen Params.procdecl) then
               % assign symbol in declarations
               Sym={Params.env setSymbol(Name Pos $)}
               fSym( Sym Pos)
            elseif {Params.env hasSymbol(Name $)} then
               % if a symbol exists for this variable, use it as
               % is it a local variable
               Sym={Params.env getSymbol(Name $)}
               fSym( Sym Pos)
            elseif {HasFeature AugmentedBase Name} then
               % variable from the Base env
               fConst(AugmentedBase.Name Pos)
            elseif Name == 'Base' then
               % the special variable representing the Base env itself
               fConst(AugmentedBase Pos)
            else
               % this variable is not declared
               % TODO issue an error
               %raise unnamedVariable end
               AST
            end

         %-----------
         [] fInt(V P) then
         %-----------
            fConst(V P)

         %-------------
         [] fFloat(V P) then
         %-------------
            fConst(V P)

         %------------
         [] fAtom(V P) then
         %------------
            fConst(V P)

         %---
         else
         %---
            %{Show 'Default pass for next ast'}
            %{Show AST}
            %{Show '..............................................'}
            {DefaultPass AST F Params}
         end
      end
      InitialParams = params(env:{New Environment init()} indecls:false)
   in
      {NamerInt AST InitialParams}
   end

   fun { YAssigner Syms}
      Counter={NewCell 0}
      proc {YAssignerInt fSym(Sym _)}
            if {Sym get(yindex $)}==nil then
               {Sym set(yindex @Counter)}
               Counter:=@Counter+1
            end
      end
   in
      {ForAll Syms YAssignerInt}
      @Counter
   end
   %################
   fun {GenCode AST CallParams}
   %################

      fun {GenCodeInt AST Params}
         F = GenCodeInt
      in
         case AST
         %----------------------
         of fLocal(Decls Body _) then
         %----------------------
            % Should create all without recursive call
            [ {F Decls  {Record.adjoin Params params(indecls: true)}} {F Body  Params}]

         %---------------------------------
         [] fProc(fSym(Sym _) Args Body Flags Pos) then
         %---------------------------------

            %FIXME: need to add pass to replace fProc by fDefineProc to add GOrigins for globals used in the body
            %FIXME: the name of the proc can not be available, eg in the case { {GetProc 2} arg1 arg2}

            CA
            VS
            NamedBody
            YCount
            OpCodes={NewCell nil}
            NewBody
            % Number of globals, ie variables comint from parent's environment
            GlobalsCount = 0
         in
            %

            {Show 'will call GenAndAssemble ni fProc case to create CA'}
            {GenAndAssemble Body Args 'test' d(file:Pos.1 line:Pos.2 column:Pos.3) switches CA VS}
            {Show 'will append opcodes ni fProc case'}


            OpCodes:={List.append @OpCodes  [createAbstractionUnify(k(CA) GlobalsCount  y({Sym get(yindex $)}))]}
            % not needed here, we create it, we do not call it!
            %OpCodes:={List.append @OpCodes {List.mapInd Args fun {$ I fSym(Sym _)} move(x(I-1) y({Sym get(yindex $)})) end }}
            % arrayfill for globals
            {Show 'will append opcodes ni fProc case'}
            if GlobalsCount>1 then
               for Ind in 1..GlobalsCount do
                  OpCodes:={List.append @OpCodes  [arrayFill(Ind)] }
               end
            end
            @OpCodes




         %-------------
         [] fSym(Sym _) then
         %-------------
            % In declarations, assign Y register and issue createVar


            if Params.indecls then
               % assign Y register
               if {Sym get(yindex $)}==nil then
                  {Sym set(yindex @(Params.currentIndex))}
                  (Params.currentIndex):=@(Params.currentIndex)+1
               end
               createVar(y({Sym get(yindex $)}))
            % In body, use the Y register
            else
               y({Sym get(yindex $)})
            end

         %---------------
         [] fApply(Sym Args Pos) then
         %---------------
            R={NewCell nil}
         in
            {Show 'in fApply'#Params}
            %if {HasFeature Params procassemble} andthen Params.procassemble then
            % FIXME CONTINUE : assigner les registres y si on est dans proc assemble
            %L is the arguments list
            % first move arguments in x registers
            % FIXME will need to check from which type of register we need to copy!
            R:={List.mapInd Args fun {$ Index AST}
                                    case AST
                                    of fSym(S _) then
                                       {List.append @R move(y({S get(yindex $)}) x(Index-1))}
                                    [] fConst(V _) then
                                       {List.append @R move(k(V) x(Index-1))}
                                    end
                                 end}

            case Sym
            of fConst(Const _) then
               {List.append @R [call(k(Sym.1) {List.length Args})] }
            [] fSym(Sym _) then
               {List.append @R [call(y({Sym get(yindex $)}) 1)] }
            end

         %--------------------
         [] fAnd(First Second) then
         %--------------------
            [{F First  Params} {F Second  Params}]

         %----------------
         [] fEq(LHS RHS _) then
         %----------------
            % no recursive call needed because everything is atomic at this stage (fSym of fConst)
            unify({F LHS  Params} {F RHS  Params})

         %-----------------
         [] fConst(Value _) then
         %-----------------
            % should never happen
            k(Value)
         end
      end
      InitialParams = {Record.adjoin params(indecls:false opCodes:{NewCell nil} currentIndex:{NewCell 0}) CallParams}
      OpCodes
   in
      % append deallocateY and return
      OpCodes={List.append {List.flatten {GenCodeInt AST InitialParams}} [deallocateY() 'return'()]}

      % prefix with allocateY
      %FIXME: keep prefix in Params?
      % Don't do this here!
      if {HasFeature InitialParams prefix} then
         {List.append [allocateY(@(InitialParams.currentIndex))] {List.append InitialParams.prefix  OpCodes } }
         %{Show InitialParams.prefix}
         %allocateY(@(InitialParams.currentIndex))| OpCodes
      else
         allocateY(@(InitialParams.currentIndex))| OpCodes
      end
   end


   proc {AssembleAST Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
      % FIXME
      PrintName='test'
   in
      {Show 'will call NewAssemble'}
      {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end

   proc {GenAndAssemble AST Args PrintName DebugData Switches ?CodeArea ?VS}
      %CHECKME
      OpCodes={NewCell nil}
      Prefix ={NewCell nil}
      YCounter
      Arity = {List.length Args}
   in
      {Show 'Will assign Ys'}
      YCounter={YAssigner Args}
      if YCounter>0 then
         % Use List.forAllInd
         {For 0 YCounter-1 1
            proc {$ I}
              Prefix:={List.append @Prefix [move(x(I) y({{Nth Args I+1}.1 get(yindex $)}) )]}
            end}
      end
      {Show 'Assigned Ys:'#YCounter}
      {Show 'Check if all symbols assigned'}
      {Show 'Will generate opcodes'}
      OpCodes := {List.append @OpCodes {GenCode AST params(prefix:@Prefix procassemble:true currentIndex:{NewCell YCounter} )}}
      {ForAll @OpCodes Show}
      {Show 'will call AssembleAST'}
      {AssembleAST Arity @OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end
end

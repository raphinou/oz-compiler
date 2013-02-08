functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
   % for nested environments debugging
   OS

export
   namer: Namer
   genCode: GenCode
   declsFlattener: DeclsFlattener
   globaliser: Globaliser
   pve: PVE
   pvs: PVS
   unWrapFAnd: UnWrapFAnd
define


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Support classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   class ListBuilder
      attr
         head
         tail

      meth init(Head)
         head:=Head
         tail:=Head
      end

      meth append(V)
         NewTail
      in
        @tail=V|NewTail
        tail:=NewTail
      end

      meth close()
         @tail=nil
      end
   end
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
         gindex
         procId
         %possibilities: localProcId , global, localised (when a global has been replaced by a new local symbol)
         type
         ref
      meth clone(?NewSym)
         NewSym={New Symbol init(@name @pos)}
         {NewSym set(yindex @yindex)}
         {NewSym set(gindex @gindex)}
         {NewSym set(procId @procId)}
         {NewSym set(type @type)}
      end
      meth init(Name Pos)
         name:=Name
         pos:=Pos
         yindex:=nil
         gindex:=nil
         procId:=0
         type:=localProcId
      end
      meth toVS(?R)
         pos(File Line Col _ _ _)=@pos
      in
         %FIXME SCOPENAMEISNAME
         %R="'Sym "#@name#"@"#File#"("#Line#","#Col#") y:"#@yindex#" type: "#@type#"'"
         %for debugging when scope is printable (ie OS.rand and not NewName)
         R="'Sym "#@name#"@"#File#"("#Line#","#Col#") y:"#@yindex#" type: "#@type#" procId "#@procId#"'"
      end
      meth hasYIndex(?B)
         B=(@yindex\=nil)
      end
      meth setProcId(ProcId)

         if @procId\=0 then
            raise tryingToOverrideAssignedProcId end
         end
         procId:=ProcId
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

   % Wrap an instructions list in fAnd
   % Note the first element of the list is the deepest nested.
   % This is usually ok as instructions list are often build by adding the new
   % instruction at the beginning of the list like:
   %   NewInstr|CurrentInstrs
   fun {WrapInFAnd Instrs}
      if {Not {List.is Instrs}}then
         {Show Instrs}
         raise wrapInFAndNeedsAListOfLength2OrMore end
      else
         L = {List.length Instrs}
      in
         if L>1 then
            {List.foldL Instrs.2 fun {$ A I} fAnd(I A) end Instrs.1}
         elseif L==1 then
            Instrs.1
         else
            unit
         end
      end
   end
   fun {UnWrapFAnd AST}
      fun {UnWrapFAndInt AST Terminate}
         case AST
         of fAnd(First Second=fAnd(_ _)) then
       {UnWrapFAndInt First false}|{UnWrapFAndInt Second Terminate}
         [] fAnd(First Second) then
            {UnWrapFAndInt First false} | {UnWrapFAndInt Second Terminate}
         else
            if Terminate then
               AST|nil
            else
               AST
            end
         end
      end
   in
      {UnWrapFAndInt AST true}
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
         case AST
         of pos(_ _ _ _) then
            % Do not go down into position records
            AST
         else
            {Record.map AST fun {$ I} {F I Params} end}
         end
      else
         AST
      end
   end
   % see http://www.mozart-oz.org/documentation/notation/node6.html
   % Pattern Variables from Statements
   fun {PVS AST}
      fun {PVSInt AST}
         case AST
         of fAnd(First Second ) then
            {Record.adjoin {PVSInt First}  {PVSInt Second}}
         [] fVar(Name _) then
            pv(Name:AST)
         [] fLocal(Decls Body _) then
            % {PVS Body} - {PVS Decls}
            {Record.subtractList {PVSInt Body}  {Record.arity {PVSInt Decls}}}
         [] fProc(E _ _ _ _ ) then
            {PVE E}
         [] fEq(LHS RHS _) then
            {PVE LHS}
         else
            pv()
         end
      end
   in
      {Record.toList {PVSInt AST}}
   end


   % Pattern Variables from Expressions
   fun {PVE AST}
      case AST
      of fVar(Name _) then
         pv(Name:AST)
      [] fLocal(Decls Body _) then
         % Get last feature in fAnd hierarchy
         fun {Last AST}
            case AST
            of fAnd(First Second) then
               {Last Second}
            else
               AST
            end
         end
         %Return all but last feature in fAnd hierarchy
         fun {ButLast AST}
            case AST
            of fAnd(V fAnd(First Second)) then
               fAnd(V {ButLast fAnd(First Second)})
            [] fAnd(First _) then
               First
            else
               AST
            end
         end
      in
         % Body2 is the body except the last instruction
         % ({PVS Body2} + {PVE Last}) - {PVS Decls}
         {Record.subtractList{{Record.adjoin {PVS {ButLast Body}} {PVE Last}}  {Record.arity {PVS Decls}}}}
      [] fEq(LHS RHS) then
         {Record.adjoin {PVE LHS} {PVE RHS}}
      else
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
         fun {CodeInDecls AST}
            proc {CodeInDeclsInt AST Acc}
               case AST
               of fAnd(First Second) then
                  CodeInFirst={CodeInDecls First}
                  CodeInSecond={CodeInDecls Second}
               in
                  if CodeInFirst\=nil then
                     Acc:={List.append CodeInFirst @Acc}
                  end
                  if CodeInSecond\=nil then
                     Acc:={List.append CodeInSecond @Acc}
                  end
               [] fVar(_ _) then
                  skip
               else
                 Acc:=AST|@Acc
               end
            end
            Instrs = {NewCell nil}
         in
            {CodeInDeclsInt AST Instrs}
            @Instrs
         end
      in
         case AST
         %------------------------
         of fLocal(Decls Body Pos) then
         %------------------------

            Acc=params(acc:{NewCell nil})
            FinalAcc
            NewDecls
            NewBody
            Res
            Tmp
            CodeInDeclarations
            CodeInDeclatationsAST

            PatternVariables
         in
            CodeInDeclarations={CodeInDecls Decls}
            PatternVariables={PVS Decls}
            NewDecls={WrapInFAnd PatternVariables}


            CodeInDeclatationsAST={WrapInFAnd CodeInDeclarations}
            if CodeInDeclatationsAST==unit then
               NewBody={DeclsFlattener Body}
            else
               NewBody=fAnd({DeclsFlattener CodeInDeclatationsAST} {DeclsFlattener Body})
            end

            % Put all transformed parts in the new fLocal
            Res = fLocal(
               NewDecls
               NewBody
               Pos
            )

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
      fun {NamerForDecls AST Params}
         case AST
         %----------------
         of fVar(Name Pos) then
         %----------------
            Sym
         in
            % in declarations or procedure procdecl, assign symbol to variables
            %if Params.indecls orelse ({HasFeature Params procdecl}  andthen Params.procdecl) then
               % assign symbol in declarations
               Sym={Params.env setSymbol(Name Pos $)}
               fSym( Sym Pos)
         [] fAnd(First Second) then
            fAnd( {NamerForDecls First Params} {NamerForDecls Second Params})
         %---
         else
         %---
            {Show 'AST received by NamerForDecls:'}
            {DumpAST.dumpAST AST}
            raise flattenerLeftOtherThingsThanFVarInDecls end
         end
      end



      fun {NamerForBody AST Params}
         case AST
         %-----------------------
         of fLocal(Decl Body Pos) then
         %-----------------------
            Res
         in
            {Params.env backup()}
            Res=fLocal(
               {NamerForDecls Decl Params}
               {NamerForBody Body  Params}
               Pos
               )
            {Params.env restore()}
            Res

         %---------------------------------
         [] fProc(Name Args Body Flags Pos) then
         %---------------------------------
            fProc(
               % The procedure's variable has to be declared explicitely
               {NamerForBody Name Params}
               {List.map Args fun {$ I} {NamerForDecls I Params} end }
               {NamerForBody Body Params}
               Flags
               Pos
            )


         %----------------
         [] fVar(Name Pos) then
         %----------------
            Sym
         in
            if {Params.env hasSymbol(Name $)} then
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
            {DefaultPass AST NamerForBody Params}
         end
      end

      InitialParams = params(env:{New Environment init()})
   in
      %{Show 'AST received by Namer:'}
      %{DumpAST.dumpAST AST}
      %{Show '--------------------------------------------------------------------------------'}
      {NamerForBody AST InitialParams}
   end

   %###################
   fun {Globaliser AST}
   %###################
      fun {GlobaliserInt AST Params}
         fun {AssignScope AST ProcId}
            case AST
            of fSym(Sym _) then
               {Sym setProcId(ProcId)}
               AST
            [] fEq(fSym(Sym _) RHS _) then
               {Sym setProcId(ProcId)}
               AST
            else
               {DefaultPass AST AssignScope ProcId}
            end
         end

      in
         case AST
         %-------------------
         of fProc(FSym Args ProcBody Flags Pos) then
         %-------------------
            % Identify new scope with a new name
            %FIXME SCOPENAMEISNAME
            %ScopeName = {NewName}
            NewProcId = {OS.rand}
            NewParams={Record.adjoin Params params(currentProcId:NewProcId)}
            DeclaredLocals
            NewBody
            NewLocals
         in
            % Assign scope to the formal parameters (Args)
            {AssignScope Args NewProcId _}
            %Extract the list of variables declared
            DeclaredLocals = {UnWrapFAnd Args}

            %Call recursively on children doing a post-order traversal
            NewBody = {GlobaliserInt ProcBody NewParams}

            %Children Globals are in Params.globals
            %An fProc's globals are the globals of its children minus the variables it declares
            %This fProc's NewLocals are the newlocals of its childrent minus thos it declares
            NewLocals={List.filter @(Params.newlocals) fun {$ I} {Not {List.member {I get(ref $)} DeclaredLocals}} end }
            (Params.newlocals):=NewLocals

            % FIXME: can we do without the Globals list?
            fDefineProc(FSym Args NewBody Flags Pos {List.mapInd NewLocals fun {$ Ind L} {L set(gindex Ind-1)} {L get(ref $)} end } NewLocals)

         [] fLocal(Decls Body Pos) then
            DeclaredLocals
            NewBody
            NewLocals
         in
            {AssignScope Decls Params.currentProcId _}
            DeclaredLocals = {UnWrapFAnd Decls}
            NewBody = {GlobaliserInt Body Params}
            NewLocals={List.filter @(Params.newlocals) fun {$ I} {Not {List.member {I get(ref $)} DeclaredLocals}} end }
            (Params.newlocals):=NewLocals
            fLocal(Decls NewBody Pos)
         %-------------
         []fSym(Sym Pos) then
         %-------------
            NewLocalSymbol
         in
            {Show fSym}
            if {Sym get(procId $)}\=Params.currentProcId then
               NewLocalSymbol = {Sym clone($)}
               {NewLocalSymbol set(procId Params.currentProcId)}
               {NewLocalSymbol set(type global)}
               {NewLocalSymbol set(ref Sym)}

               % collect global and its corresponding new local symbol
               (Params.newlocals):=NewLocalSymbol|@(Params.newlocals)

               %{System.showInfo "replaced "#{Sym toVS($)}#" by "#{NewLocalSymbol toVS($)} }
               % Return the new symbol
               fSym(NewLocalSymbol Pos)
            else
               AST
            end
         %-------------
         [] fVar(_ _) then
         %-------------
            raise namerLeftFVarIntact end

         %-------------
         [] fAtom(_ _) then
         %-------------
            raise namerLeftFAtomIntact end
         %---
         [] fConst(_ _) then
            AST
         [] pos(_ _ _ _ _ ) then
            AST
         else
         %---
            {DefaultPass AST GlobaliserInt Params}
         end
      end
      InitialParams=params(currentProcId: {OS.rand} newlocals:{NewCell nil} )
   in
      try
         {GlobaliserInt AST InitialParams}
      catch E then
         case E
         of 'NamerLeftFVarIntact' then
            {Show 'Namer left fVar intact'}
            {DumpAST.dumpAST AST}
         else
            raise E end
         end
      end
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
         [] fDefineProc(fSym(Sym _) Args Body Flags Pos Globals NewLocals) then
         %---------------------------------

            %FIXME: the name of the proc can not be available, eg in the case { {GetProc 2} arg1 arg2}

            CA
            VS
            NamedBody
            YCount
            OpCodes={NewCell nil}
            NewBody
            % Number of globals, ie variables comint from parent's environment
            GlobalsCount = {List.length Globals}
            ArrayFills
         in
            %
            if {List.length Globals}\={List.length NewLocals} then
               raise incoherenceBetweenGlobalsAndNewLocalsListsSizes end
            end

            {GenAndAssemble Body Args 'test' d(file:Pos.1 line:Pos.2 column:Pos.3) switches ?CA ?VS}


            OpCodes:={List.append @OpCodes  [createAbstractionUnify(k(CA) GlobalsCount  y({Sym get(yindex $)}))]}
            % not needed here, we create it, we do not call it!
            %OpCodes:={List.append @OpCodes {List.mapInd Args fun {$ I fSym(Sym _)} move(x(I-1) y({Sym get(yindex $)})) end }}
            % arrayfill for globals
            ArrayFills = {List.map Globals fun {$ I} arrayFill(y({I get(yindex $)})) end }
            OpCodes:={List.append @OpCodes  ArrayFills }
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
            %CHECKME:  don't we need this global case?
            %elseif {Sym get(type $)}==global then
            %   g({Sym get(gindex $)})
            else
               y({Sym get(yindex $)})
            end

         %---------------
         [] fApply(Sym Args Pos) then
         %---------------
            R={NewCell nil}
         in
            %if {HasFeature Params procassemble} andthen Params.procassemble then
            %L is the arguments list
            % first move arguments in x registers
            % FIXME will need to check from which type of register we need to copy!
            R:={List.mapInd Args fun {$ Index AST}
                                    case AST
                                    of fSym(S _) then
                                       if {S get(type $)}==localProcId then
                                          {List.append @R move(y({S get(yindex $)}) x(Index-1))}
                                       elseif {S get(type $)}==global then
                                          {List.append @R move(g({S get(gindex $)}) x(Index-1))}
                                       else
                                          raise unknownSymbolType end
                                       end
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
      %OpCodes={List.flatten {GenCodeInt AST InitialParams}}

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
      %{Show 'will call NewAssemble on opcodes'}
      %{ForAll OpCodes Show}
      %{Show '..............................'}
      {NewAssembler.assemble Arity OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end

   proc {GenAndAssemble AST Args PrintName DebugData Switches ?CodeArea ?VS}
      %CHECKME
      OpCodes={NewCell nil}
      Prefix ={NewCell nil}
      YCounter
      Arity = {List.length Args}
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
      {Show 'Will generate opcodes'}
      OpCodes := {List.append @OpCodes {GenCode AST params(prefix:@Prefix procassemble:true currentIndex:{NewCell YCounter} )}}
      {ForAll @OpCodes Show}
      {Show 'will call AssembleAST'}
      {AssembleAST Arity @OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end
end

functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at '../lib/DumpAST.ozf'
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
         id
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
         {NewSym set(ref @ref)}
         {NewSym set(type @type)}
      end
      meth init(Name Pos)
         id:={OS.rand} mod 100
         name:=Name
         pos:=Pos
         yindex:=nil
         gindex:=nil
         procId:=0
         type:=localProcId
      end
      meth toVS(?R)
         pos(File Line Col _ _ _)=@pos
         Ref
      in
         if @type==localised then
            Ref = ' refprocid: '#{@ref get(procId $)}#' ref:Sym'#{@ref get(id $)}
         else
            Ref=''
         end
         %FIXME SCOPENAMEISNAME
         %R="'Sym "#@name#"@"#File#"("#Line#","#Col#") y:"#@yindex#" type: "#@type#"'"
         %for debugging when scope is printable (ie OS.rand and not NewName)
         R="'Sym"#@id#" "#@name#"@"#File#"("#Line#","#Col#") y:"#@yindex#"g:"#@gindex#" type: "#@type#" procId "#@procId#Ref#"'"
      end
      meth hasYIndex(?B)
         B=(@yindex\=nil)
      end
      meth setProcId(ProcId)

         if @procId\=0 then
            {Show 'Trying to override procId value to '#ProcId}
            {System.showInfo {self toVS($)}}
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



   % List helper functions
   fun {IndexOf Xs Y}
      fun {IndexOfInt Xs Y I}
         case Xs
         of nil then notFound
         [] X|Xr then
            if X == Y then found(I)
            else {IndexOfInt Xr Y I+1}
            end
         end
      end
   in
      case {IndexOfInt Xs Y 1}
      of found(I) then
         I
      else
         0
      end
   end
   proc {FirstOfP Xs P ?Index ?Item}
      fun {FirstOfPInt Xs P I}
         case Xs
         of nil then notFound
         [] X|Xr then
            if {P X} then found(I X)
            else {FirstOfPInt Xr P I+1}
            end
         end
      end
   in
      case {FirstOfPInt Xs P 1}
      of found(I R) then
         Index=I
         Item=R
      else
         Index=0
         Item=unit
      end
   end

   %###################
   fun {Globaliser AST}
   %###################
      class GlobalsManager
         attr
            globals
            newlocals
         meth init()
            globals:=nil   % array of global variables of the current fProc
            newlocals:=nil  % list of locals created for the global with same index
         end

         meth indexOfGlobal(Global ?Ind)
            Ind={IndexOf @globals Global}
         end
         meth hasLocalForGlobalInProcId(Global ProcId ?B ?Local)
            GlobalInd
            Locals
         in
            % get index of global
            GlobalInd={self indexOfGlobal(Global $)}
            if GlobalInd>0 then
               % the global was found
               % get list of corresponding locals
               Locals = @{Nth @newlocals GlobalInd}
               % return boolean true if found, ie if index received is > 0
               B=({FirstOfP Locals fun {$ I} {I get(procId $)}==ProcId end $ ?Local}>0)
            else
               B=false
               Local=unit
            end
            % search for local with ProcId
         end

         meth createLocalForGlobalInProcId(Global ProcId ?Local)
            GlobalInd
            L
         in
            %FIXME handle position!! Need to get
            Local = {New Symbol init({Global get(name $)} {Global get(pos $)})}
            {Local set(type localised)}
            {Local set(ref Global)}
            {Local set(procId ProcId)}
            {self addPair(Global Local)}
         end

         % Apply P to all pairs (GlobalVariable, CorrespondingLocalsList)
         meth forAllPairs(P)
            {List.forAllInd @globals proc {$ Ind Global} {P Global @{List.nth @newlocals Ind } } end }
         end
         meth addPair(Global Local)
            GlobalInd
         in
            GlobalInd={self indexOfGlobal(Global $)}
            if GlobalInd>0 then
               %this global already had a local created, just append the new local
               % get cell containing the list of locals for the global at index GlobalInd
               LocalsForGlobal = {Nth @newlocals GlobalInd}
            in
               % prepend the new local to the list in the cell
               LocalsForGlobal:=Local|@LocalsForGlobal
            else
               %Global was not yet see, so add it to the list and create a new list for its locals
               globals:=Global|@globals
               newlocals:={NewCell Local|nil}|@newlocals
            end
         end
      end
      fun {GlobaliserInt AST Params}
         fun {AssignScope AST ProcId}
            case AST
            of fSym(Sym _) then
               {Show 'assigning procId '#ProcId#' to '}
               {System.showInfo {Sym toVS($)}}
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
            NewProcId = {OS.rand} mod 1000
            NewParams={Record.adjoin Params params( currentProcId:NewProcId gm:{New GlobalsManager init()})}
            (NewParams.setter):={FSym.1 get(name $)}
            DeclaredLocals
            NewBody
            NewLocals={NewCell nil}
         in
            % Assign scope to the formal parameters (Args)
            {Show 'assign scope to arguments of fProc'}
            {AssignScope Args NewProcId _}
            %Extract the list of variables declared
            DeclaredLocals = {UnWrapFAnd Args}
            NewBody = {GlobaliserInt ProcBody NewParams}

            {NewParams.gm forAllPairs(proc {$ Global Locals}
                                       FoundLocal
                                       NewLocal
                                    in
                                       if {Global get(procId $)}==NewParams.currentProcId then
                                          % this is a variable we declare, don't pass it to the parent
                                          skip
                                       else
                                          if {FirstOfP Locals fun{$ I} {I get(procId $)}==NewParams.currentProcId end $ ?FoundLocal}>0 then
                                             %make all locals reference the local with current procId that was found
                                             {List.forAll Locals  proc {$ I}
                                                                     if I\=FoundLocal then
                                                                     {I set(ref FoundLocal)}
                                                                     end
                                                                  end}
                                             % then push the pair (Global FoundLocal) in parent's global manager
                                             {Params.gm addPair(Global FoundLocal)}
                                             % and collect in the newlocals to pass to fDefineProc
                                             NewLocals:=FoundLocal|@NewLocals
                                          else
                                             NewLocal
                                          in
                                             % No local with current procId was found, need to create one
                                             % we create it at the parent level
                                             {Params.gm createLocalForGlobalInProcId(Global NewParams.currentProcId ?NewLocal)}
                                             % Make all our children's locals for that global point to our newly created local
                                             {List.forAll Locals  proc {$ I}
                                                                     {I set(ref NewLocal)}
                                                                  end}
                                             % And add it to our level
                                             {NewParams.gm addPair(Global NewLocal)}
                                             % and collect in the newlocals to pass to fDefineProc
                                             NewLocals:=NewLocal|@NewLocals
                                          end
                                       end
                                    end)}

            % Set the gindex for the newlocals of this fProc
            {List.forAllInd @NewLocals proc {$ Ind Sym} {Sym set(gindex Ind-1)} end }
            fDefineProc(FSym Args NewBody Flags Pos @NewLocals)

         [] fLocal(Decls Body Pos) then
            DeclaredLocals
            NewBody
            NewLocals
         in
            {Show 'assign scope in fLocal'}
            {AssignScope Decls Params.currentProcId _}
            DeclaredLocals = {UnWrapFAnd Decls}
            NewBody = {GlobaliserInt Body Params}
            fLocal(Decls NewBody Pos)
         %-------------
         []fSym(Sym Pos) then
         %-------------
            LocalSymbolFound
            NewLocalSymbol
            Found
         in
            {Show '------------------------------fSym--------------------------'}
            {System.showInfo {Sym toVS($)}}
            % If the Symbol's procId is different from the current proc's
            % procId, it means that it is a global for the current proc.
            if {Sym get(procId $)}\=Params.currentProcId then
               {Params.gm hasLocalForGlobalInProcId(Sym Params.currentProcId ?Found ?LocalSymbolFound)}
               if Found then
                  if LocalSymbolFound==unit then raise localClaimedToExistNotReturned end end
                  fSym(LocalSymbolFound Pos)
               else
                  {Params.gm createLocalForGlobalInProcId(Sym Params.currentProcId ?NewLocalSymbol)}
                  fSym(NewLocalSymbol Pos)
               end
            else
               % Same procId, so keep it!
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
      InitialParams=params(currentProcId: {OS.rand} mod 1000 gm:{New GlobalsManager init()} setter:{NewCell 'toplevel'})
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
         [] fDefineProc(fSym(Sym _) Args Body Flags Pos NewLocals) then
         %---------------------------------

            %FIXME: the name of the proc can not be available, eg in the case { {GetProc 2} arg1 arg2}

            CA
            VS
            NamedBody
            YCount
            OpCodes={NewCell nil}
            NewBody
            % Number of globals, ie variables comint from parent's environment
            GlobalsCount = {List.length NewLocals}
            ArrayFills
         in
            %
            {Show '##############'}
            {System.showInfo 'Procedure '#{Sym get(name $)} }
            {Show '##############'}
            {GenAndAssemble Body Args 'test' d(file:Pos.1 line:Pos.2 column:Pos.3) switches ?CA ?VS}


            OpCodes:={List.append @OpCodes  [createAbstractionUnify(k(CA) GlobalsCount  y({Sym get(yindex $)}))]}

            % arrayfill for globals
            %----------------------
            {System.showInfo 'Here are the globals for '#{Sym get(name $)}}
            {ForAll NewLocals proc {$ G} {System.showInfo {G get(name $)}#' '#{G get(type $)}#' '#{G get(procId $)}#' ' } end }
            ArrayFills = {List.map NewLocals
                                    fun {$ I}
                                       if {{I get(ref $)} get(type $)}==localised then
                                          %{Show '####################'}
                                          %{Show '----ArrayFill-------'}
                                          %{Show '####################'}
                                          %{Show 'next var is referencing a global (so use g)'}
                                          %{System.showInfo {I toVS($)}}
                                          %{Show 'References:' }
                                          %{System.showInfo {{I get(ref $)}  toVS($)}}
                                          arrayFill(g({{I get(ref $)} get(gindex $)}))
                                       else
                                          %{Show 'next var is referencing a non globali (so use y)'}
                                          %{System.showInfo {I toVS($)}}
                                          %{Show 'References:' }
                                          %{System.showInfo {{I get(ref $)}  toVS($)}}
                                          arrayFill(y({{I get(ref $)} get(yindex $)})) end
                                    end }
            OpCodes:={List.append @OpCodes ArrayFills}

            {System.showInfo {Sym get(name $)}#' Opcode is:'}
            {ForAll @OpCodes Show}
            {System.showInfo 'End Procedure '#{Sym get(name $)} }
            {Show '================================================================================'}

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
            elseif {Sym get(type $)}==localised then
               g({Sym get(gindex $)})
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
            _={List.mapInd Args fun {$ Index AST}
                                    case AST
                                    of fSym(S _) then
                                       if {S get(type $)}==localProcId then
                                          if {S get(yindex $)}==nil then
                                             raise missingNeededYIndex end
                                          end
                                          R:={List.append @R [move(y({S get(yindex $)}) x(Index-1))]}
                                       elseif {S get(type $)}==localised then
                                          if {S get(gindex $)}==nil then
                                             {System.showInfo 'missing gindex for '#{S toVS($)}}
                                             raise missingNeededGIndex end
                                          end
                                          R:={List.append @R [move(g({S get(gindex $)}) x(Index-1))]}
                                       else
                                          raise unknownSymbolType end
                                       end
                                    [] fConst(V _) then
                                       R:={List.append @R [move(k(V) x(Index-1))]}
                                    end
                                 end}
            case Sym
            of fConst(Const _) then
               {List.append @R [call(k(Sym.1) {List.length Args})] }
            % FIXME: find better naming for Sym2 ?
            [] fSym(Sym2 _) then
               {List.append @R [call(y({Sym2 get(yindex $)}) {List.length Args})] }
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
      OpCodes := {List.append @OpCodes {GenCode AST params(prefix:@Prefix procassemble:true currentIndex:{NewCell YCounter} )}}
      {Show 'CodeArea built with this code:'}
      {ForAll @OpCodes Show}
      {AssembleAST Arity @OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end
end

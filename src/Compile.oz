functor

import
   Narrator('class')
   ErrorListener('class')
   System(showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction makeArity ) at 'x-oz://system/CompilerSupport.ozf'
   % Boot_Object provides:
   % attrExchangeFun attrGet attrPut cellOrAttrExchangeFun cellOrAttrGet cellOrAttrPut getClass is new
   %Boot_Object at 'x-oz://boot/Object'
   Boot_CompilerSupport at 'x-oz://boot/CompilerSupport'
   Boot_Record at 'x-oz://boot/Record'
   Boot_Thread at 'x-oz://boot/Thread'
   Boot_Exception at 'x-oz://boot/Exception'
   Boot_Name at 'x-oz://boot/Name'
   Boot_Value at 'x-oz://boot/Value'
   DumpAST at '../lib/DumpAST.ozf'
   % for nested environments debugging
   OS

export
   namer: Namer
   genCode: CodeGen
   declsFlattener: DeclsFlattener
   desugar: Desugar
   unnester: Unnester
   globaliser: Globaliser
   pve: PVE
   pvs: PVS
   unWrapFAnd: UnWrapFAnd
define


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Support classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Left here in case it becomes useful in the future
%   class ListBuilder
%      attr
%         head
%         tail
%
%      meth init(Head)
%         head:=Head
%         tail:=Head
%      end
%
%      meth append(V)
%         NewTail
%      in
%        @tail=V|NewTail
%        tail:=NewTail
%      end
%
%      meth close()
%         @tail=nil
%      end
%   end
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
         xindex
         yindex
         gindex
         procId
         % type possibilities:
         %  -localProcId
         %  -localised (when a global has been replaced by a new local symbol)
         %  -patternmatch
         %  -wildcard (unused symbol)
         type
         ref
      meth clone(?NewSym)
         % Create new symbole with same properties as this one.
         NewSym={New Symbol init(@name @pos)}
         {NewSym set(yindex @yindex)}
         {NewSym set(gindex @gindex)}
         {NewSym set(xindex @xindex)}
         {NewSym set(procId @procId)}
         {NewSym set(ref @ref)}
         {NewSym set(type @type)}
      end
      meth init(Name Pos)
         % Initialises attributes
         id:={OS.rand} mod 100
         name:=Name
         pos:=Pos
         xindex:=nil
         yindex:=nil
         gindex:=nil
         procId:=0
         type:=localProcId
      end
      meth toVS(?R)
         % Gives a string representation of this symbole, including the symbol it references if it's a localised one.
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
         R="'Sym"#@id#" "#@name#"@"#File#"("#Line#","#Col#") x:"#@xindex#"y:"#@yindex#"g:"#@gindex#" type: "#@type#" procId "#@procId#Ref#"'"
      end
      meth hasXIndex(?B)
         B=(@xindex\=nil)
      end
      meth hasYIndex(?B)
         B=(@yindex\=nil)
      end
      meth hasGIndex(?B)
         B=(@gindex\=nil)
      end
      meth setProcId(ProcId)
         % Set the procId, but raises an error if there was already one
         if @procId\=0 then
            {Show 'Trying to override procId value to '#ProcId}
            {System.showInfo {self toVS($)}}
            raise tryingToOverrideAssignedProcId end
         end
         procId:=ProcId
      end
   end
   class SyntheticSymbol from Symbol
      % A synthetic symbol is a symbol introduces by the compiler, eg when unnesting.
      % As such, it has no corresponding variable name, and its type is set to synthetic.
      meth init(Pos)
         Symbol, init('' Pos)
         type:=synthetic
      end
      meth toVS(?R)
         % Override this method to avoid problems with irrelevant attributes
         R="'SSym"#@id#"y:"#@yindex#"g:"#@gindex#" type: "#@type#" procId "#@procId#"'"
      end
   end

   class Environment
      % The class Environment is used by the namer to keep track of variables
      % defined at each step.
      % The environment maps the variable name to its symbol. That way, all
      % occurences of a variable in an environment are replaced by the same
      % symbol.
      % An new environment is created when entering an fProc, but that new
      % environment keeps all definitions of the parent. That's what creates
      % the closure.
      attr
         dict
         backups
      meth init()
         dict:={NewDictionary}
         backups:=nil
      end

      meth addOrGetSymbol(Name Pos ?Res)
         % If the variable Name is already mapped to a symbol, assign it to Res.
         % If not, first create a new symbol, map it to the variable name, and assign it to Res.
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
         % Assigns to R the symbol mapped to variable Name
         R={Dictionary.get @dict Name}
      end
      meth hasSymbol(Name ?R)
        {Dictionary.member @dict Name R}
      end
      meth setSymbol(Name Pos ?NewSymbol)
         % create a new symbol and map it to variable Name.
         % This method is used when we need to override an already defined mapping,
         % eg when a variable local to a proc override a variable captured from the parent environment.
         NewSymbol = { New Symbol init(Name Pos)}
         {Dictionary.put @dict Name NewSymbol}
      end
      meth setSyntheticSymbol(Pos ?NewSymbol)
         % create a new symbol and map it to variable Name.
         % This method is used when we need to override an already defined mapping,
         % eg when a variable local to a proc override a variable captured from the parent environment.
         NewSymbol = { New SyntheticSymbol init(Pos)}
         {Dictionary.put @dict {NewName} NewSymbol}
      end
      meth backup()
         % Backup environment so it can be restored later.
         % Simply push a clone of the dictionary on a list
         backups:={Dictionary.clone @dict}|@backups
      end
      meth restore()
         % Restore a backed up environment. Simply pop from the backups list.
         dict:=@backups.1
         backups:=@backups.2
      end
   end

   % Wrap an instructions list in records with label Label.
   % Mainly used to wrap a list of nodes in fAnd or fAndThen.
   % However, as fAnd has no position feature and fAndThen has,
   % there was a nedd to add the trick of passing the position value false
   % for record without positions...
   % Note the first element of the list is the deepest nested.
   % This is usually ok as instructions list are often build by adding the new
   % instruction at the beginning of the list like:
   %   NewInstr|CurrentInstrs
   fun {WrapIn Label Instrs WithPos}
      if {Not {List.is Instrs}}then
         {Show Instrs}
         raise wrapInNeedsAList end
      else
         L = {List.length Instrs}
      in
         if L>1 then
            if WithPos==false then
               {List.foldL Instrs.2 fun {$ A I} Label(I A) end Instrs.1}
            else
               {List.foldL Instrs.2 fun {$ A I} Label(I A WithPos) end Instrs.1}
            end
         elseif L==1 then
            Instrs.1
         else
            unit
         end
      end
   end
   fun {WrapInFAnd L}
      {WrapIn fAnd L false}
   end

   fun {UnWrapFAnd AST}
      %Get a list of records wrapped in a fAnd hierarchy.
      fun {UnWrapFAndInt AST }
         case AST
         of fAnd(First Second) then
            {UnWrapFAndInt First } | {UnWrapFAndInt Second }
         else
            AST|nil
         end
      end
   in
      {List.flatten {UnWrapFAndInt AST }}
   end

   fun {ListToAST L}
      case L
      of X|Xs then
         fRecord(fConst('|' pos)
                 '|'(X {ListToAST Xs}))
      [] nil then
         nil
      end
   end


   % List helper functions
   %Returns the index of Y in list Xs, 0 if not found
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
   % Gives the index of and the first item itself for which the predicate P is
   % true. If not found, index is 0 and item is unit
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


   % Generate a new label used in opcode generation
   % Using ints for easier debug, but can easily be switched to names
   IntLabel={NewCell 0}
   fun {GenLabel}
      IntLabel:=@IntLabel+1
      @IntLabel
   end


   fun {GetPos AST}
      fun {GetPosInList L}
         case L
         of X|Xs then
            if {Record.label X}==pos then
               X
            else
               {GetPosInList Xs}
            end
         [] nil then
            pos
         end
      end
   in
      %returns the position feature of the AST, pos if nont was found
      {GetPosInList {Record.toList AST}}
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
   fun {DefaultPassNoParams AST F}
      % beware of the order. a record is also a list!
      if {List.is AST} then
         {List.map AST F}
      elseif {Record.is AST} then
         case AST
         of pos(_ _ _ _) then
            % Do not go down into position records
            AST
         else
            {Record.map AST F}
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
         [] fFun(E _ _ _ _ ) then
            {PVE E}
         [] fEq(LHS _ _) then
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
      [] fAnd(S E) then
         {Record.adjoin {PVS S} {PVE E}}
      [] fLocal(Decls Body _) then
         % Get last feature in fAnd hierarchy
         % Body2 is the body except the last instruction
         % ({PVS Body2} + {PVE Last}) - {PVS Decls}
         {Record.subtractList {PVE Body}  {Record.arity {PVS Decls} }}
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
         % Returns the list of instructions found in the declarations passed in AST.
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
            % Extract code from the declarations.
            % Extract variables to be defined from declarations with the pattern variable functions PVS
            % Move the code from the declarations to the body, wrapping all in fAnds
            % Leave only the variables to be declared in the declaration part.

            NewDecls
            NewBody
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
            fLocal(
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

   % FIXME : we add Show manually to the base environment.
   AugmentedBase={AdjoinAt Base 'Show' Show}
   % Key is a name known only by the compiler, and used to protect date, notably in the pattern matching code.
   Key = {NewName}

   % Helper functions to store and access data stored privately by the compiler.
   fun {StoreInSafe X}
      {NewChunk store(Key:X compiler_internal__:true)}
   end
   fun {AccessSafe X}
      X.Key
   end
   fun {IsSafe X}
      {IsChunk X} andthen {HasFeature X Key}
   end

   %##############
   fun {Namer AST}
   %##############
      % The namer replaces variable names with a Symbol instance, all identical
      % variable instances referencing the same symbol.
      % The environment is a dictionary, the keys being variable names, the value
      % being their respective symbol instance
      % AST = the record
      % Params is a record with 1 features:
      %   env = mapping of var names to symbols built in parents
      % Body and declarations are handled by 2 distinct functions: NamerForDecls
      % and NamerForBody.

      fun {NamerForDecls AST Params}
      %-----------------------------
         % In declarations, we create a new symbol.
         case AST
         %----------------
         of fVar(Name Pos) then
         %----------------
            Sym
         in
            % assign symbol in declarations
            Sym={Params.env setSymbol(Name Pos $)}
            fSym( Sym Pos)
         %--------------------
         [] fAnd(First Second) then
         %--------------------
            fAnd( {NamerForDecls First Params} {NamerForDecls Second Params})
         [] fSym(_ _) then
            % Introduced when added support for patterns in function arguments.
            % See explanation in NamerForBody
            AST
         else
         %---
            %{Show 'AST received by NamerForDecls:'}
            {DumpAST.dumpAST AST _}
            raise flattenerLeftOtherThingsThanFVarInDecls end
         end
      end

      fun {NamerForCaptures Pattern Params}
         % This function creates synthetic symbols for captures in the case patterns
         % These new symbols are collected in Params.captures so they can be declared
         % outside the case.
         case Pattern
         of fVar(Name Pos) then
            NewSymbol
         in
            NewSymbol={Params.env setSymbol(Name Pos $)}
            {NewSymbol set(type patternmatch)}
            % Add the fSym record for the new symbol in the captures list, so it can immediately be wrapped in fAnd
            (Params.captures):=fSym(NewSymbol Pos)|@(Params.captures)
            % In the fConst, directly plave the 'safe'
            fConst( {StoreInSafe fSym(NewSymbol Pos)} Pos)
         [] fEscape( Var=fVar(_ _) Pos) then
            NewSymbol={New SyntheticSymbol init(Pos)}
         in
            {NewSymbol set(type patternmatch)}
            (Params.additionalGuards):= fOpApply('==' [fSym(NewSymbol Pos) {NamerForBody Var Params}] Pos)|@(Params.additionalGuards)
            (Params.captures):=fSym(NewSymbol Pos)|@(Params.captures)
            fConst( {StoreInSafe fSym(NewSymbol Pos)} Pos)
         [] fRecord(Label Features) then
            fRecord({NamerForBody Label Params} {List.map Features fun {$ I} {NamerForCaptures I Params} end})
         [] fOpenRecord(Label Features) then
            fOpenRecord({NamerForBody Label Params} {List.map Features fun {$ I} {NamerForCaptures I Params} end})
         [] fColon(Key Val) then
            % Pattern matching on values in records, not on the features
            fColon({NamerForBody Key Params} {NamerForCaptures Val Params} )
         [] fWildcard(Pos) then
            NewSymbol
         in
            NewSymbol={Params.env setSyntheticSymbol(Pos $)}
            {NewSymbol set(type wildcard)}
            % Add the fSym record for the new symbol in the captures list, so it can immediately be wrapped in fAnd
            % Even if it is not used, we need to declare it
            (Params.captures):=fSym(NewSymbol Pos)|@(Params.captures)
            % In the fConst, directly place the 'safe'
            fConst( {StoreInSafe fSym(NewSymbol Pos)} Pos)
         [] fEq(LHS RHS Pos) then
            fPatMatConjunction({NamerForCaptures LHS Params} {NamerForCaptures RHS Params} Pos)
         else
            % FIXME CHECKME : is this ok?
            {NamerForBody Pattern Params}
         end
      end


      fun {NamerForBody AST Params}
      %----------------------------



         fun {WrapInFCases Body SymbolsAndPatterns Pos}
            % Used by HandlePatternArgs
            % Wrap Body in one fCase for each item of the list SymbolsAndPatterns,
            % its items being of the form Symbol#Pattern, where Symbol is the value
            % tested against Pattern.
            if {Not {List.is SymbolsAndPatterns}}then
               raise wrapInFCasesNeedsAList end
            else
               L = {List.length SymbolsAndPatterns}
            in
               if L>0 then
                  {List.foldL SymbolsAndPatterns fun {$ Acc Symbol#Pattern} fCase(Symbol [fCaseClause(Pattern Acc)] fNoElse(Pos) Pos) end Body}
               else
                  Body
               end
            end
         end


         fun {HandlePatternArgs AST Params}
            % Function called by NamerForBody when it encounters a function of
            % procedure with pattern arguments.
            % It first builds 2 lists, which will be used if patterns are present in the arguments
            % (in the other case, the original arguments list is used):
            % - one containing all arguments, including the new symbols replacing the patterns
            % - one containing items NewSymbol#Pattern
            % The first is used to build the new parameters list, the second is used to wrap the
            % function's body in fCases testing the value in NewSymbol against Pattern.
            % After that, it works on the AST of the function of procedure, using the lists if needed.
            Res
            ArgsDesc
            ASTLabel={Record.label AST}
            Name Args Body Flags Pos
         in
            if ASTLabel==fFun then
               fFun(Name Args Body Flags Pos)=AST
            elseif ASTLabel==fProc then
               fProc(Name Args Body Flags Pos)=AST
            else
               raise patternArgumentsOnlySupportedInFunAndProc end
            end
            % Backup environment here because we immediately create new symbols for patterns
            {Params.env backup()}

            %Extract needed lists
            ArgsDesc={List.foldL Args   fun {$ Acc I}
                                          case I
                                          of fRecord(_ _) then
                                             Pos={GetPos I}
                                             NewSymbol={Params.env setSyntheticSymbol(Pos $)}
                                          in
                                             (Acc.args):=fSym(NewSymbol Pos)|@(Acc.args)
                                             (Acc.patterns):=fSym(NewSymbol Pos)#I|@(Acc.patterns)
                                          [] fOpenRecord(_ _) then
                                             Pos={GetPos I}
                                             NewSymbol={Params.env setSyntheticSymbol(Pos $)}
                                          in
                                             (Acc.args):=fSym(NewSymbol Pos)|@(Acc.args)
                                             (Acc.patterns):=fSym(NewSymbol Pos)#I|@(Acc.patterns)
                                          [] fEq(_ _ _) then
                                             Pos={GetPos I}
                                             NewSymbol={Params.env setSyntheticSymbol(Pos $)}
                                          in
                                             (Acc.args):=fSym(NewSymbol Pos)|@(Acc.args)
                                             (Acc.patterns):=fSym(NewSymbol Pos)#I|@(Acc.patterns)
                                          else
                                             (Acc.args):=I|@(Acc.args)
                                          end
                                          Acc
                                       end
                                       acc(patterns:{NewCell nil} args:{NewCell nil})}
            if ArgsDesc\=nil andthen {List.length @(ArgsDesc.patterns)}>0 then
               % If we have patterns, then we replace the pattern arguments by
               % their respective symbol, and inject a case in the body of the function.
               % This required NamerForDecls to handle fSym, as the tested value of the
               % case is the symbol we just placed in the argument list, but all the rest
               % of the fCase has still to be traversed by the namer.
               Res=ASTLabel(
                  % The function's variable has to be declared explicitely in the declaration part.
                  % That's why we call NamerForBody on the Name
                  {NamerForBody Name Params}
                  % Formal parameters are declarations, that's why we call NameForDecls
                  % FIXME: we can replace the 2 list visits by one
                  % Use the new arguments list
                  {List.map {List.reverse @(ArgsDesc.args)} fun {$ I} {NamerForDecls I Params} end }
                  % Wrap body in fCases
                  {NamerForBody {WrapInFCases Body @(ArgsDesc.patterns) Pos} Params}
                  Flags
                  Pos
               )
            else
               Res=ASTLabel(
                  % The function's variable has to be declared explicitely in the declaration part.
                  % That's why we call NamerForBody on the Name
                  {NamerForBody Name Params}
                  % Formal parameters are declarations, that's why we call NameForDecls
                  {List.map Args fun {$ I} {NamerForDecls I Params} end }
                  {NamerForBody Body Params}
                  Flags
                  Pos
               )
            end
            {Params.env restore()}
            Res
         end
      in
         % for fLocal, fFun, fProc, we backup the environment when we enter and
         % restore it when we get out.
         % for fFun and fProc, this is necessary or formal parameters could
         % erase variables with the same name in the parent environment when
         % used after the fun/proc definition (see test 029)
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
         [] fFun(_ _ _ _ _) then
         %---------------------------------
            {HandlePatternArgs AST Params}
         %---------------------------------
         [] fProc(_ _ _ _ _) then
         %---------------------------------
            {HandlePatternArgs AST Params}
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

         [] fCase(Val Clauses Else Pos) then
            NewParams={Record.adjoin Params params( captures:{NewCell nil} guardsSymbols:{NewCell nil} additionalGuards:{NewCell nil})}
            NewClauses
            Decls
            NewCaseAST
            R
         in
            % We first go through all clauses.
            % Each clause can introduce additional guards for escaped variables. Those are collected
            % in NewParams.additionalGuards by NamerForCaptures.
            % Each clause has its own environment, so a clause cannot access a symbol specific to another clause.
            % Expressions with guards in fSideCondition are transformed:
            % - Guards expressions are unified with one symbol, yielding a statement.
            % - it produces a fNamedSideCondition where the feature corresponding to the guard holds this new statement,
            %   and with additional feature the symbol unified with the original guards.
            % This transformation is needed for CodeGen to know which symbol to use for the guards.
            %
            % Additional guards are clause specific, but guards symbols and captures are collected over the whole clauses list.
            % These have to be declared in a local..in..end instruction wrapping the case instruction.
            NewClauses={List.map Clauses  fun{$ I}
                                             NewPattern NewBody
                                          in
                                             % Additional guards are clause specific. Reset them everytime
                                             (NewParams.additionalGuards):=nil
                                             case I
                                             of fCaseClause(fSideCondition(Pattern Decls Guards Pos) Body) then
                                                NewGuardSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
                                                NewGuards
                                             in
                                                {NewParams.env backup}
                                                NewPattern = {NamerForCaptures Pattern NewParams}
                                                % Don't create new symbols in guards, but give access to
                                                % those defined in the pattern by using the same environment
                                                % Also add possible guards introduced by !Vars in pattern
                                                NewGuards=fEq(NewGuardSymbol {WrapIn fAndThen {NamerForBody Guards NewParams}|@(NewParams.additionalGuards) Pos} Pos)
                                                NewBody = {NamerForBody Body NewParams}
                                                (NewParams.guardsSymbols):=NewGuardSymbol|@(NewParams.guardsSymbols)
                                                {NewParams.env restore}
                                                fCaseClause(fNamedSideCondition(NewPattern Decls NewGuards NewGuardSymbol Pos) NewBody)
                                             [] fCaseClause(Pattern Body) then
                                                {NewParams.env backup}
                                                NewPattern = {NamerForCaptures Pattern NewParams}
                                                NewBody = {NamerForBody Body NewParams}
                                                if @(NewParams.additionalGuards)==nil then
                                                   {NewParams.env restore}
                                                   fCaseClause(NewPattern NewBody)
                                                else
                                                   NewGuardSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
                                                   NewGuards
                                                in
                                                   NewGuards=fEq(NewGuardSymbol {WrapIn fAndThen @(NewParams.additionalGuards) Pos} Pos)
                                                   (NewParams.guardsSymbols):=NewGuardSymbol|@(NewParams.guardsSymbols)
                                                   {NewParams.env restore}
                                                   fCaseClause(fNamedSideCondition(NewPattern fSkip(unit)  NewGuards NewGuardSymbol Pos) NewBody)
                                                end
                                             end
                                          end }

            Decls = {List.append @(NewParams.captures) @(NewParams.guardsSymbols)}
            NewCaseAST=fCase({NamerForBody Val Params} NewClauses {NamerForBody Else Params} Pos)
            if {List.length Decls}>0 then
               R=fLocal({WrapInFAnd Decls} NewCaseAST Pos)
            else
               R=NewCaseAST
            end
            R
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

         [] fClass(Var Attributes Methods Pos) then
            fun {NameMethod fMeth(fRecord(Name Args) Body Pos) Params}
               NewArgs NewBody
            in
               % Args are available only to the method, so we backup the environment
               % to be able to restore it after we traversed this method
               {Params.env backup()}
               % Declare all args
               NewArgs={List.map Args  fun{$ I}
                                          case I
                                          of fMethArg(Arg Default) then
                                             fMethArg({NamerForDecls Arg Params} {NamerForBody Default Params})
                                          [] fMethColonArg(F V Default) then
                                             fMethColonArg({NamerForBody F Params} {NamerForDecls V Params} {NamerForBody Default Params})
                                          end
                                       end }
               % Traverse body with args in environment
               NewBody={NamerForBody Body Params}
               {Params.env restore()}
               fMeth(fRecord( {NamerForBody Name Params} NewArgs) NewBody Pos)
            end
            NewMethods
         in
            NewMethods={List.map Methods fun {$ I} {NameMethod I Params} end }
            fClass( {NamerForBody Var Params} {NamerForBody Attributes Params} NewMethods Pos)

         %---
         else
         %---
            {Show 'Default pass for next ast'}
            {Show AST}
            {Show '..............................................'}
            {DefaultPass AST NamerForBody Params}
         end
      end

      InitialParams = params(env:{New Environment init()})
   in
      %{Show 'AST received by Namer:'}
      %{DumpAST.dumpAST AST}
      %{Show '--------------------------------------------------------------------------------'}
      {Show '-------'}
      {Show 'Namer'}
      {Show '-------'}
      {NamerForBody AST InitialParams}
   end


   %################
   fun {Desugar AST}
   %################
      % Simply transform syntactic sugar in its basic form.
      % Expressions and Statements are handled differently.
      % Eg an if as statement has both its branches handled as statement, but
      % when it is an expression, both branches are treated as expressions too.
      fun {DesugarOp Op Args Pos}
      %--------------------------
         case Op
         of '+' then %orelse '-' orelse '/' orelse '*' then
            fConst(Number.Op Pos)
         [] '-' then
            fConst(Number.Op Pos)
         [] '*' then
            fConst(Number.Op Pos)
         [] 'div' then
            fConst(Int.Op Pos)
         [] '/' then
            fConst(Float.Op Pos)
         [] '==' then
            fConst(Value.Op Pos)
         [] '>' then
            fConst(Value.Op Pos)
         [] '<' then
            fConst(Value.Op Pos)
         [] '>=' then
            fConst(Value.Op Pos)
         [] '=<' then
            fConst(Value.Op Pos)
         [] '.' then
            fConst(Value.Op Pos)
         [] '\\=' then
            fConst(Value.Op Pos)
         else
            % Return Op if it shouldn't be desugared because it is not an operator
            Op
         end
      end

      fun {DesugarRecordFeatures Feature Params}
      %-----------------------------------------
         % Assign features if not specified, starting with index 1
         case Feature
         of fColon(F V) then
            fColon({DesugarExpr F Params} {DesugarExpr V Params})
         else
            Pos
         in
            % Feature.2 is most of the time the position, but not alway.
            % We check it
            if {Record.is Feature.2} andthen {Label Feature.2}==pos then
               Pos = Feature.2
            else
               Pos = pos
            end

            (Params.featureIndex):=@(Params.featureIndex)+1
            fColon( fConst(@(Params.featureIndex) Pos) {DesugarExpr Feature Params})
         end
      end
      % Expression/statements:
      % https://github.com/mozart/mozart2-bootcompiler/blob/master/src/main/scala/org/mozartoz/bootcompiler/transform/Transformer.scala#L64

      fun {IsConstantRecord fRecord(L Fs)}
      %-----------------------------------
         % Returns couple of bools indicating if label, features and values of record all are constants or not.
         % This will go through the list once, and set all components to correct boolean value.
         {List.foldL Fs fun {$ FB#VB I} F V in I=fColon(F V) (FB andthen {Label F}==fConst)#(VB andthen {Label V}==fConst) end ({Label L}==fConst)#true}
      end

      fun {TransformRecord AST=fRecord(_ Features)}
      %------------------------------------------------
         % Called to transform record just *after* it has been desugared:
         % - replace constant record by a constant (fConst)
         % - replace records with non const label or feature by a call to Boot_Record.makeDynamic

         fun {Features2Record Features}
            % Builds a record with label #, features are increasing integer
            % values, and the values are alternatively the feature and its
            % corresponding value of the record definition found in the AST.
            % rec(A:1 B:2 C:3)
            % will be transformed in (in AST form):
            % #(1:A 2:1 3:B 4:2 5:C 6:3)
            % This is done because we need records with constant features in CodeGen.
            fun{Features2RecordInt Features I}
               case Features
               of fColon(F V)|Fs then
                  % FIXME: set pos!
                  % FIXME: check if we shouldn't we call TransformRecord or Features2Record on the value if it is a fRecord?
                  fColon(fConst(I pos) F)|fColon(fConst(I+1 pos) V)|{Features2RecordInt Fs I+2}
               [] nil then
                  nil
               end
            end
         in
            %FIXME: set pos!
            fRecord( fConst('#' pos) {Features2RecordInt Features 1} )
         end
      in
         case {IsConstantRecord AST}
         of true#true then
            % All parts of the record are constants. Build the record and put
            % it in the AST as a constant (under a fConst)

            Rec

            fun {ConstantiseRecord AST=fRecord(fConst(RecordLabel _) _ )}
               fun {ConstantiseRecordInt AST Lab}
                  {List.foldL Features fun{$ A I}
                                          case I
                                          of fColon(fConst(L _) fConst(F _)) then
                                             {Record.adjoin A Lab(L:F)}
                                          [] fColon(fConst(L _) R=fRecord(_ _ _)) then
                                             {Record.adjoin A Lab(L:{TransformRecord R})}
                                          else
                                             A
                                          end
                                      end Lab()}
               end
            in
               {ConstantiseRecordInt AST RecordLabel}
            end

         in
            Rec={ConstantiseRecord AST}
            fConst(Rec pos)
         [] true#false then
            % will use makeArity in CodeGen, leave as is
            AST
         [] false#_ then
            %Need to change it in a call to makeDynamic, so that the arity is constant for later phases, including CodeGen
            L Features
         in
            fRecord(L Features)=AST
            % FIXME: set pos!
            fApply( fConst(Boot_Record.makeDynamic pos) [L {Features2Record Features}] pos)
         else
            AST
         end
      end
      fun {HandleLazyFlag ReturnSymbol Body Flags Pos}
         % This function returns the body of a function, modified accordingly
         % if the lazy flag is present.
         % A lazy function has its body unified with the return symbol in a thread,
         % with a waitNeeded on this symbol *before* the unification.
         % An eager function has simply its body unified with the return symbol.
         if {List.member lazy {List.map Flags fun{$ fAtom(Flag _)} Flag end}} then
            fThread( fAnd( fApply(fConst(Boot_Value.waitNeeded Pos) [ReturnSymbol] Pos)
                           fEq(ReturnSymbol Body Pos)
                         )
                     Pos)
         else
            fEq(ReturnSymbol Body Pos)
         end
      end

      fun {DesugarExpr AST Params}
      %---------------------------
      {Show 'DesugarExpr'}
      {DumpAST.dumpAST AST _}
         % Desugar expressions
         case AST
         of fProc(Dollar Args Body Flags Pos) then
         %   DollarSymbol = fSym({New SyntheticSymbol init(DollarPos)} DollarPos)
         %in
            % FIXME: is the recursive call on the top level really useful?
            % Wouldn't is be better to only do the recursive calls on args and body?
            %{DesugarStat fLocal( DollarSymbol fAnd( fProc(DollarSymbol Args Body Flags Pos) DollarSymbol) Pos) Params}
            fProc(Dollar Args {DesugarStat Body Params} Flags Pos)

         [] fFun(Dollar Args Body Flags Pos) then
            ReturnSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
         %   DollarSymbol = fSym({New SyntheticSymbol init(DollarPos)} DollarPos)
         %in
            % The fLocal we introduce is an expression, handle it as such!
            %{DesugarExpr fLocal( DollarSymbol fAnd( fFun(DollarSymbol Args Body Flags Pos) DollarSymbol) Pos) Params}
            fProc(Dollar {List.append Args [ReturnSymbol]} {DesugarStat {HandleLazyFlag ReturnSymbol Body Flags Pos} Params} Flags Pos)

         [] fAndThen(First Second Pos) then
            fBoolCase({DesugarExpr First Params} {DesugarExpr Second Params} fConst(false Pos) Pos)

         [] fOrElse(First Second Pos) then
            fBoolCase({DesugarExpr First Params} fConst(true Pos) {DesugarExpr Second Params} Pos)

         [] fLocal(Decls Body Pos) then
            % for fLocal, declarations are always statements.
            % if the fLocal is a statement, its body must be a statement and is handled as such
            % Do not recursively desugar declarations, as they are all fSym thanks for DeclsFlattener.
            fLocal(Decls {DesugarExpr Body Params} Pos)
         [] fAnd(First Second) then
            % if the fAnd is an expression, only the second part is treated as expression
            fAnd({DesugarStat First Params} {DesugarExpr Second Params})

         [] fAt(Cell Pos) andthen @(Params.'self')==unit then
            % Not in a class because Params.self is unit
            fApply( fConst(Boot_Value.catAccess Pos) [{DesugarExpr Cell Params}] Pos)
         [] fAt(Cell Pos) then
            % When in a class, Params.self has been set to Self. See TransformMethod
            fApply( fConst(Boot_Value.catAccessOO Pos) [@(Params.'self') {DesugarExpr Cell Params}] Pos)

         [] fOpApply(Op Args Pos) then
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fApply(Op Args Pos) then
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fColonEquals(Cell Val Pos) andthen @(Params.'self')==unit then
            fApply( fConst(Boot_Value.catExchange Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fColonEquals(Cell Val Pos) then
            fApply( fConst(Boot_Value.catExchangeOO Pos) [ @(Params.'self') {DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fBoolCase( Cond TrueCode fNoElse(_) Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarExpr TrueCode Params} fNoElse(pos) Pos)

         [] fBoolCase( Cond TrueCode FalseCode Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarExpr TrueCode Params} {DesugarExpr FalseCode Params} Pos)

         [] fRecord( Label Features) then
            NewParams={Record.adjoin Params params( featureIndex:{NewCell 0})}
         in
            {TransformRecord fRecord({DesugarExpr Label Params} {List.map Features fun {$ I} {DesugarRecordFeatures I NewParams} end }) }
         [] fPatMatConjunction(LHS RHS Pos) then
            fConst({StoreInSafe fPatMatConjunction({DesugarExpr LHS Pos} {DesugarExpr RHS Pos} Pos)} Pos)
         [] fOpenRecord(Label Features) then
            NewParams={Record.adjoin Params params( featureIndex:{NewCell 0})}
         in
            fOpenRecord({DesugarExpr Label Params} {List.map Features fun {$ I} {DesugarRecordFeatures I NewParams} end })
         [] fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) then
            fNamedSideCondition({DesugarExpr Pattern Params} Decls {List.map Guards fun {$ I} {DesugarStat I Params} end} GuardSymbol Pos)
         [] fColon(Feature Value) then
            fColon({DesugarExpr Feature Params} {DesugarExpr Value Params})

         [] fThread(Body Pos) then
            % Create synthetic symbol that will take the value of the thread expression,
            % and then desugar the fThread statement left.
            % It is DesugarStat that will replace the fThread by a call to the builtin Thread.create
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewSymbol fAnd( {DesugarStat fThread(fEq(NewSymbol Body Pos) Pos) Params} NewSymbol) Pos)
         [] fWildcard(Pos) then
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal( NewSymbol NewSymbol Pos)
         [] fCase(Val Clauses Else=fNoElse(_) Pos) then
            % As usual: declare a new symbol, unify it with each clause's body, and put it as last expression
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewSymbol fAnd({DesugarStat fCase({DesugarExpr Val Params} {List.map Clauses fun {$ fCaseClause(Pattern Body)} fCaseClause( Pattern fEq(NewSymbol Body Pos) ) end} Else Pos) Params} NewSymbol) Pos)
         [] fCase(Val Clauses Else Pos) then
            % As usual: declare a new symbol, unify it with each clause's body, and put it as last expression
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewSymbol fAnd({DesugarStat fCase({DesugarExpr Val Params} {List.map Clauses fun {$ fCaseClause(Pattern Body)} fCaseClause( Pattern fEq(NewSymbol Body Pos) ) end} fEq(NewSymbol Else Pos) Pos) Params} NewSymbol) Pos)

         [] fSym(_ _) then
            AST
         [] fConst(_ _) then
            AST
         [] fDollar(_) then
            AST
         [] fAtom(_ _) then
            raise namerLeftFAtomIntactAtDesugar end
         else
            {DumpAST.dumpAST AST _}
            raise unhandledRecordInDesugarExpr end
         end
      end


      fun {DesugarStat AST Params}
      %---------------------------
         fun {DesugarCaseClause Clause}
            case Clause
            of fCaseClause(fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) Body) then
               fCaseClause(fNamedSideCondition({DesugarExpr Pattern Params} Decls {DesugarStat Guards Params} GuardSymbol  Pos) {DesugarStat Body Params} )
            [] fCaseClause(Pattern Body) then
               fCaseClause({DesugarExpr Pattern Params} {DesugarStat Body Params})
            end
         end
      in
         % Desugar Statements.
         {Show 'DesugarStat'}
         {DumpAST.dumpAST AST _}
         case AST
         of fAnd(First Second) then
            % if the fAnd is a statement, both parts are treated as statements
            fAnd({DesugarStat First Params} {DesugarStat Second Params})

         [] fLocal(Decls Body Pos) then
            % for fLocal, declarations are always statements.
            % if the fLocal is a statement, its body must be a statement and is handled as such
            % Do not recursively desugar declarations, as there are all fSym thanks for DeclsFlattener.
            fLocal(Decls {DesugarStat Body Params} Pos)

         [] fApply(Op Args Pos) then
            % both Op and Args must be expression and expressions list respectively
            {Show 'Will desugar fApply'}
            {Show 'Op is '#Op}
            {Show 'is Args a list?'#{List.is Args}}
            {Show Args}
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {Show 'Will desugar arg'#I}{DesugarExpr I Params} end } Pos)

         [] fEq(LHS RHS Pos) then
            fEq({DesugarExpr LHS Params} {DesugarExpr RHS Params} Pos)
         [] fProc(FSym Args Body Flags Pos) then
            fProc({DesugarExpr FSym Params} Args {DesugarStat Body Params} Flags Pos)
         [] fFun(FSym Args Body Flags Pos) then
            ReturnSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            % Need to Desugar the top-level fProc, eg in the case of a statement function (fun {$ ..}),
            % so that the $ also gets desugared
            fProc(FSym {List.append Args [ReturnSymbol]} {DesugarStat {HandleLazyFlag ReturnSymbol Body Flags Pos} Params} Flags Pos)
         [] fColonEquals(Cell Val Pos) andthen @(Params.'self')==unit then
            fApply( fConst(Boot_Value.catAssign Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)
         [] fColonEquals(Cell Val Pos) then
            fApply( fConst(Boot_Value.catAssignOO Pos) [@(Params.'self') {DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)
         [] fBoolCase( Cond TrueCode fNoElse(_) Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarStat TrueCode Params} fNoElse(pos) Pos)

         [] fBoolCase( Cond TrueCode FalseCode Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarStat TrueCode Params} {DesugarStat FalseCode Params} Pos)
         [] fThread(Body Pos) then
            % Create a wrapping proc taking no argument, and pass it to the builtin Thread.create.
            NewProcSym=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewProcSym fAnd(fProc(NewProcSym nil {DesugarStat Body Params} nil Pos) fApply(fConst(Boot_Thread.create Pos) [NewProcSym] Pos)) Pos)
         [] fLockThen(Lock Body Pos) then
            % Create a wrapping proc taking no argument, and pass it to the builtin Base.lockIn
            NewProcSym=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewProcSym fAnd(fProc(NewProcSym nil {DesugarStat Body Params} nil Pos) fApply(fConst(LockIn Pos) [Lock NewProcSym] Pos)) Pos)
         [] fCase(Val Clauses fNoElse(_) Pos=pos(File Line _ _ _ _)) then
            fCase({DesugarExpr Val Params} {List.map Clauses DesugarCaseClause} fApply(fConst(Boot_Exception.'raiseError' Pos) [{DesugarExpr fRecord(fConst(kernel pos) [fConst(noElse pos) fConst(File pos) fConst(Line pos) Val]) Params} ] Pos) Pos)
         [] fCase(Val Clauses Else Pos) then
            fCase({DesugarExpr Val Params} {List.map Clauses DesugarCaseClause} {DesugarStat Else Params} Pos)
         [] fClass(FSym AttributesAndProperties Methods Pos) then
            fun {DesugarClass AST Params}
               fun {TransformMethod Ind fMeth(Signature=fRecord(FName=fConst(Name _) Args) Body Pos) Params}
                  % Takes one method signature and an index, and build its element of the '#' record containing
                  % the class' methods.
                  % The value returned is also a '#'-record, the first value being the method's name, the second being a
                  % proc expression taking 2 arguments:
                  % - self
                  % - the message requesting the method invocation
                  % Due to this 'proxy' procedure, the symbols in the body do not correspond to the procedure arguments anymore.
                  % That's why the body is prefixed with unifications of the method's arguments with the proc's message's values
                  % meth foo(X Y)
                  %  {Show X Y}
                  % end
                  % is transformed in
                  % proc {Foo Self M}
                  %    X=M.1
                  %    Y=M.2
                  % in
                  %    {Show X Y}
                  % end
                  % The Body is also desugared with the SelfSymbol passed in Params.
                  % This is needed, eg for desugaring @attribute in catAccessOO rather than in catAccess.
                  % The attributes have to be placed in a record with label 'attr'. The features in that record are the
                  % attributes, the values being their respective initial value, on the result of
                  % a call to {Boot_Name.newUnique 'ooFreeFlag'}
                  NewArgs
                  SelfSymbol=fSym({New Symbol init('self' Pos)} Pos)
                  fun {BuildMessage Args Params}
                     % Build message record corresponding to the call
                     {List.foldL Args fun{$ Acc Arg }
                                         %FIXME handle defaults
                                         case Arg
                                         of fMethArg(Var Default) then
                                            {Record.adjoin Acc Name(Var)}
                                         [] fMethColonArg(fConst(F _) V Default) then
                                            {Record.adjoin Acc Name(F:V)}
                                          end
                                      end
                                      Name()}
                  end
                  Message
                  NewBody
                  Decls
                  R
                  MessageSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
               in
                  Message={BuildMessage Args Params}

                  Decls = {List.mapInd Args  fun {$ Ind I}
                            % If a default value is provided, inject code in the AST to check if a value was provided,
                            % ie if the feature is present in the message.
                            % If it is a fMethArg, the feature is numeric and found in Ind
                            % It is is a fMethColonArg, the feature is found in its first value.
                            % Except for the distinction in the feature, both cases have the same code.
                                                case I
                                                of fMethArg(Sym fNoDefault) then
                                                   Sym#fEq(Sym fOpApply('.' [MessageSymbol fConst(Ind pos)] pos) pos)
                                                [] fMethArg(Sym fDefault(Default _)) then
                                                   Sym#fEq(Sym fBoolCase(fApply(fConst(Value.hasFeature pos) [MessageSymbol fConst(Ind pos)] pos) fOpApply('.' [MessageSymbol fConst(Ind pos) ] pos) Default pos) pos)
                                                [] fMethColonArg(F Sym fNoDefault) then
                                                   Sym#fEq(Sym fOpApply('.' [MessageSymbol F ] pos) pos)
                                                [] fMethColonArg(F Sym fDefault(Default _)) then
                                                   Sym#fEq(Sym fBoolCase(fApply(fConst(Value.hasFeature pos) [MessageSymbol F] pos) fOpApply('.' [MessageSymbol F ] pos) Default pos) pos)

                                                end
                                             end}
                  if {List.length Decls}>0 then
                     NewBody=fLocal( {WrapInFAnd {List.map Decls fun{$ Sym#_} Sym end}} {WrapInFAnd {List.append  [Body] {List.map Decls fun{$ _#Init} Init end }}} Pos)
                  else
                     NewBody=Body
                  end
                  % Desugar the body with the self symbol set.
                  % This will transform @bla in catAccessOO
                  % FIXME: check that we can still use cells
                  (Params.'self'):=SelfSymbol
                  R=fColon(fConst(Ind Pos) fRecord(fConst('#' Pos) [FName fProc(fDollar(Pos) [SelfSymbol MessageSymbol] {DesugarStat NewBody Params} nil Pos)] ))
                  (Params.'self'):=unit

                  {Show 'transformed method:'}
                  {DumpAST.dumpAST R}
               end

               fun {TransformAttribute Rec Params}
                  case Rec
                  of '#'(F V) then
                     fColon(F V)
                  else
                     fColon(Rec fConst({Boot_Name.newUnique 'ooFreeFlag'} pos))
                  end
               end


               Parents NewMeths NewAttrs NewFeats NewProps PrintName
            in
               Parents=fConst(nil pos)
               NewMeths=fRecord(fConst('#' Pos) {List.mapInd Methods fun {$ Ind I} {TransformMethod Ind I Params} end } )
               {Show 'NewMeths = '}
               {DumpAST.dumpAST NewMeths _}
               {List.forAll AttributesAndProperties  proc {$ I}
                                                        case I
                                                        of fAttr(L _) then
                                                           NewAttrs=fRecord(fConst('attr' pos) {List.map L fun {$ Attr} {TransformAttribute Attr Params} end })
                                                        [] fFeat(L _) then
                                                           NewFeats=fRecord(fConst('feat' pos) {List.map L fun {$ Attr} {TransformAttribute Attr Params} end })
                                                        end
                                                     end}
               %NewAttrs=fConst('attr'() pos)
               {Show 'NewFeats:'}
               {DumpAST.dumpAST NewFeats _}
               %NewFeats=fConst('feat'() pos)
               NewProps=fConst(nil pos)
               PrintName=fConst(printname pos)
               %ok:
               {DesugarStat fApply( fConst(OoExtensions.'class' Pos) [Parents NewMeths NewAttrs NewFeats NewProps PrintName FSym] Pos) Params}
            end
         in
            {DesugarClass AST Params}
         [] fSkip(_) then
            AST
         %else
         %   {DefaultPass AST DesugarInt Params}
         end
      end
   in
      {Show '-------'}
      {Show 'Desugar'}
      {Show '-------'}
      {DesugarStat AST params('self':{NewCell unit})}
   end

   %#################
   fun {Unnester AST}
   %#################
      fun {IsElementary AST}
      %---------------------
         % Only fConst and fSym are elementary
         if {Record.is AST} then
            case {Label AST}
            of fConst then
               true
            [] fSym then
               true
            [] fDollar then
               true
            [] fAtom then
               true
            else
               false
            end
         else
            true
         end
      end

      fun {BindVarToExpr FSym AST Params}
      %----------------------------------
         % Handles the binding of a variables to a complex expression
         case AST
         of fProc(fDollar(_) Args Body Flags Pos) then
            {UnnesterInt fProc(FSym Args Body Flags Pos) Params }
         [] fApply(Proc Args Pos) then
             %enters the assignation target in the proc call
             %Res = {P Arg} -> {P Arg Res}
            fun {InjectSym FSym Args HadDollar}
               %This function injects the symbol to assign to in the arguments
               %list, either in the dollar location or as last argument.
               case Args
               of fDollar(_)|Rs then
                  FSym|{InjectSym FSym Rs true}
               [] R|Rs then
                  R|{InjectSym FSym Rs HadDollar}
               [] nil then
                  if HadDollar then
                     nil
                  else
                     FSym|nil
                  end
               end
            end
         in
            {UnnesterInt fApply(Proc {InjectSym FSym Args false} Pos) Params }
         [] fAnd(First Second) then
            % the result of a sequence of instructions is the value of the last one
            % Recursive call to get to the end of the sequence
            %FIXME: set Pos
            %FIXME: place the recursive call depper in the subtree (on the fEq)?
            {UnnesterInt fAnd(First fEq(FSym Second pos)) Params}
         [] fLocal(Decls Body Pos) then
            % the value of a local..in..end is the value of the last expression in the body
            {UnnesterInt fLocal(Decls fEq(FSym Body Pos) Pos) Params}
         [] fBoolCase(Cond TrueCode FalseCode Pos) then
            % A = if Cond then TrueCode else FalseCode end
            % become
            % if Cond then A=TrueCode else A=FalseCode end
            {UnnesterInt fBoolCase(Cond fEq(FSym TrueCode Pos) fEq(FSym FalseCode Pos) Pos) Params}
         [] fRecord(_ _) then
            % record has already been unnested before the call to BindVarToExpr (see call to BindVarToExpr for fRecord), just return the fEq
            %FIXME: set pos!
            fEq(FSym AST pos)
         else
            {DumpAST.dumpAST FSym _}
            {DumpAST.dumpAST AST _}
            raise unexpectedASTForBindVarToExpr end
         end
      end

      fun {UnnestFApply AST=fApply(Op Args Pos) Params}
      %------------------------------------------------
         % Transforms the AST such that fApply args are all elementary (fSym or
         % fConst). Creates new symbols unified with complex arguments. Wraps
         % the fApply in fLocals for each new symbol created.
         % does not use Params
         fun {UnnestFApplyInt FApplyAST NewArgsList ArgsRest}
            % Unnest all arguments one by one.
            % Elementary arguments are left untouched
            % Complex arguments are extracted from the arguments list by:
            % - declaring a new symbol
            % - unifying this new symbol with the argument
            % - replacing the argument by the new symbol in the argument list.

            case ArgsRest
            of X|Xs then
               % The recursive calls haven't reached the end of the argument list.
               % Handle the head of the remaining list, and make a recursive call
               if {IsElementary X} then
                 {UnnestFApplyInt FApplyAST X|NewArgsList Xs}
               else
                  NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
               in
                  %case X
                  %of fDollar(_) then
                  %   % If the argument is $, replace it by the new sym without adding a unification
                  %   {UnnesterInt fLocal( NewSymbol
                  %                        {UnnestFApplyInt FApplyAST NewSymbol|NewArgsList Xs} Pos)
                  %                Params}
                  %else
                     % If the argument is not $, include a unification before
                     {UnnesterInt fLocal(NewSymbol
                                         fAnd( fEq(NewSymbol X Pos)
                                               {UnnestFApplyInt FApplyAST NewSymbol|NewArgsList Xs}) Pos)
                                  Params}
                  %end
               end
            else
               % All unnested arguments are now found in NewArgsList
               % We can now work on the fApply itself
               case Op
               of fApply(_ _ _) then
                  NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
               in
                  % When the proc/fun called is itself the result of a proc/fun call, we need to recursively handle it.
                  {UnnesterInt fLocal( NewSymbol
                                       fAnd( fEq(NewSymbol {UnnestFApply Op Params} Pos)
                                             fApply(NewSymbol {List.reverse NewArgsList} Pos))
                                       Pos)
                               Params}
               else
                  % otherwise no recursive call
                  % all fLocal introduced by complex arguments have been directly place out of fApply when traversing ArgsRest
                  % and all what's left in the argument list are Symbols.
                  fApply(Op {List.reverse NewArgsList} Pos)
               end

            end
         end
      in
         {UnnestFApplyInt AST nil Args}

      end

      fun {UnnestFEq AST=fEq(LHS RHS Pos) Params}
      %------------------------------------------
         case {IsElementary LHS}#{IsElementary RHS}
         of true#true then
            AST
         [] true#false then
            % elementaty passed as first argument
            {BindVarToExpr LHS RHS Params}
         [] false#true then
            % elementaty passed as first argument
            {BindVarToExpr RHS LHS Params}
         else
            % Both sides are complex expressions.
            % declare a new symbol (hence the fLocal) and
            % unify it first with the LHS, then with RHS.
            % Then recursively unnest
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            {UnnesterInt fLocal( NewSymbol
                                 fAnd( fEq(NewSymbol LHS Pos)
                                       fEq(NewSymbol RHS Pos))
                                 Pos)
                         Params}
         end
      end
      fun {UnnestFBoolCase fBoolCase(Cond TrueCode FalseCode Pos) Params}
      %------------------------------------------------------------------
         % if the condition is elementary, simply unnest both branches
         % if the condition is non elementary, create a new symbol to be used as the condition.
         if {IsElementary Cond} then
            fBoolCase(Cond {UnnesterInt TrueCode Params} {UnnesterInt FalseCode Params} Pos)
         else
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            % FIXME: can as well call unnester on body alone, as this is what this call will do
            {UnnesterInt fLocal( NewSymbol
                                 fAnd( fEq( NewSymbol Cond Pos)
                                       fBoolCase(NewSymbol TrueCode FalseCode Pos))
                                 Pos)
                         Params}
         end
      end


      fun {UnnestFRecord AST Params}
      %----------------------------------------------
         % Similar reasoning as UnnestFApply
         % Transforms the AST such that fRecord values are all elementary (fSym or
         % fConst). Creates new symbols unified with complex values. Wraps
         % the fRecord in fLocals for each new symbol created.
         % does not use Params
         fun {UnnestFRecordInt FRecordAST NewArgsList ArgsRest}
            % Unnest all arguments one by one.
            % Elementary arguments are left untouched
            % Complex arguments are extracted from the arguments list by:
            % - declaring a new symbol
            % - unifying this new symbol with the argument
            % - replacing the argument by the new symbol in the argument list.

            % FIXME: set Pos!
            % fRecords do not have a position feature, so currently no position is set!
            % -------------------------------------------------------------------------
            case ArgsRest
            of X|Xs then
               F V
            in
               % The recursive calls haven't reached the end of the argument list.
               % Handle the head of the remaining list, and make a recursive call
               X = fColon(F V)
               if {IsElementary V} then
                 {UnnestFRecordInt FRecordAST X|NewArgsList Xs}
               else
                  case V
                  of fOpenRecord(_ _) then
                     {UnnestFRecordInt FRecordAST fColon(F {UnnestFRecordInt V nil V.2 })|NewArgsList Xs}

                  else
                     NewSymbol={New SyntheticSymbol init(pos)}
                  in
                     {UnnesterInt fLocal(fSym(NewSymbol pos)
                                         fAnd( fEq(fSym(NewSymbol pos) V pos)
                                               {UnnestFRecordInt FRecordAST fColon(F fSym(NewSymbol pos))|NewArgsList Xs}) pos)
                                  Params}
                  end
               end
            else
               % All unnested arguments are now found in NewArgsList
               % We can now work on the fRecord itself
               % otherwise no recursive call
               % all fLocal introduced by complex arguments have been directly place out of fRecord when traversing ArgsRest
               % and all what's left in the argument list are Symbols.
               case FRecordAST
               of fRecord(Op _) then
                  fRecord(Op {List.reverse NewArgsList})
               [] fOpenRecord(Op _) then
                  fOpenRecord(Op {List.reverse NewArgsList})
               end

            end
         end
      in
         case AST
         of fRecord(_ Features) then
            {UnnestFRecordInt AST nil Features}
         [] fOpenRecord(_ Features) then
            {UnnestFRecordInt AST nil Features}
         end
      end




      fun {UnnesterInt AST Params}
      %---------------------------
         %{Show 'UnnesterInt works on:'}
         %{DumpAST.dumpAST AST _}
         case AST
         of fEq(LHS RHS Pos) then
            % Call Unnester on both parts, so that if it is a record, it is unnested.
            % FIXME: This does not feel the cleanest, as it is not the expected flow.
            {UnnestFEq fEq({UnnesterInt LHS Params} {UnnesterInt RHS Params} Pos) Params}
         [] fLocal(Decls Body Pos) then
            fLocal(Decls {UnnesterInt Body Params} Pos)
         [] fApply(_ _ _) then
            {UnnestFApply AST Params}
         [] fBoolCase(_ _ _ _) then
            {UnnestFBoolCase AST Params}
         [] fRecord(_ _) then
            % FIXME:  is this really the place to constantise the record.
            % This requires the unnest function to be called on both sides of fEq before it gets treated....
            {UnnestFRecord AST Params}
         [] fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) then
            fNamedSideCondition({UnnesterInt Pattern Params} Decls {UnnesterInt Guards Params} GuardSymbol Pos)
         else
            {DefaultPass AST UnnesterInt Params}
         end
      end
   in
      {Show '-------'}
      {Show 'Unnester'}
      {Show '-------'}
      {UnnesterInt AST params}
   end



   %###################
   fun {Globaliser AST}
   %###################
      % Class to help manage the globals and their respective locals.
      %-------------------
      class GlobalsManager
      %-------------------
         attr
            globals
            newlocals
         meth init()
            globals:=nil   % array of global variables of the current fProc
            newlocals:=nil  % list of locals created for the global with same index
         end

         % gives the index of the Global variable in the list of globals known
         % by this instance
         meth indexOfGlobal(Global ?Ind)
            Ind={IndexOf @globals Global}
         end

         % if a known local with ProcId references Global, B it set to true and
         % Local is bound to said local
         % else B is false and Local bound to unit
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

         % Creates and new symbol in ProcId referencing Global, and binds Local
         % to it.
         meth createLocalForGlobalInProcId(Global ProcId ?Local)
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

         % Add Local to Global's locals. If global was not yet present,
         % initialise it first with an empty list of locals.
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

      % Internal function for Globaliser.
      % Takes the additional Params argument
      %-----------------------------
      fun {GlobaliserInt AST Params}
      %-----------------------------
         % Assign ProcId to all symbols found in AST
         fun {AssignScope AST ProcId}
            case AST
            of fSym(Sym _) then
               %{Show 'assigning procId '#ProcId#' to '}
               %{System.showInfo {Sym toVS($)}}
               {Sym setProcId(ProcId)}
               AST
            [] fSkip(_) then
               AST
            else
               {DefaultPass AST AssignScope ProcId}
            end
         end


      in
         case AST

         % An fProc creates a new procId, which is assigned to the variables it declares.
         % It recursively goes in its children, from which it receives the new locals they had to create.
         % It then looks if these new locals reference a variable it declares. If yes, do nothing. Else
         % create a new local symbol, which in its turn will be passed the the proc's parent.
         %-------------------
         of fProc(FSym Args ProcBody Flags Pos) then
         %-------------------
            % Identify new scope with a new name, and create NewParams passed to recursive calls done on children
            %FIXME SCOPENAMEISNAME
            %ScopeName = {NewName}
            NewProcId = {OS.rand} mod 1000
            NewParams={Record.adjoin Params params( currentProcId:NewProcId gm:{New GlobalsManager init()})}
            (NewParams.setter):={FSym.1 get(name $)}
            NewBody
            NewLocals={NewCell nil}
         in
            % Assign scope to the formal parameters (Args)
            {AssignScope Args NewProcId _}
            % Recursive call on body
            NewBody = {GlobaliserInt ProcBody NewParams}

            % Work on each global and its respective list of locals.
            % We decide here if
            % - the children procs' globals are variables we define at this level
            % - the children procs' globals are variables that are global at
            %   this level too
            %   - in that case, we look if this level uses the variable directly too
            %     - if yes we already have a new local defined at this level that we need to link the children's new locals to.
            %     - else we need to define a new local at this level, and link the children proc's new locals to it.
            %   - finally, we pass to our parent the list of new locals we
            %     defined at our level, so that the parent can link our new
            %     locals to its variables (which can also be new locals at its
            %     level).
            {NewParams.gm forAllPairs(proc {$ Global Locals}
                                       FoundLocal
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


         %Assign current procId to declared variable
         %Recursively call globaliser on body
         %------------------------
         [] fLocal(Decls Body Pos) then
         %------------------------
            NewBody
         in
            %{Show 'assign scope in fLocal'}
            {AssignScope Decls Params.currentProcId _}
            NewBody = {GlobaliserInt Body Params}
            fLocal(Decls NewBody Pos)

         % if the Symbol has the current procId, do nothing.
         % else it is a global, and we replace it by a localised symbol referencing the global
         %-------------
         []fSym(Sym Pos) then
         %-------------
            LocalSymbolFound
            NewLocalSymbol
            Found
         in
            %{Show '------------------------------fSym--------------------------'}
            %{System.showInfo {Sym toVS($)}}
            % If the Symbol's procId is different from the current proc's
            % procId, it means that it is a global for the current proc.
            if {Sym get(procId $)}\=Params.currentProcId then
               % First check if we already have a local symbol for this global, so we can reuse it.
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
            raise namerLeftFAtomIntactAtGlobaliser end
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
      {Show '-------'}
      {Show 'Globaliser'}
      {Show '-------'}
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

   %###########################
   fun {CodeGen AST CallParams}
   %###########################

      fun {CodeGenDecls AST Params}
      %----------------------------
         % Generate opcodes for declarations.
         case AST
         of fSym(Sym _) then
         %-------------
            % In declarations, assign Y register and issue createVar
            % assign Y register
            if {Sym get(yindex $)}==nil then
               {Sym set(yindex @(Params.currentIndex))}
               (Params.currentIndex):=@(Params.currentIndex)+1
            end
            createVar(y({Sym get(yindex $)}))
         [] fAnd(First Second) then
            [{CodeGenDecls First Params} {CodeGenDecls Second Params}]
         end
      end
      fun {RegForSym AST Params}
      %-------------------------
         % Return register for fSym or fConst
         case AST
         of fConst(K _) then
            k(K)
         [] fSym(Sym _) then
            if {Sym get(type $)}==localised then
               g({Sym get(gindex $)})
            elseif {Sym get(type $)}==patternmatch then
               x({Sym get(xindex $)})
            else
               y({Sym get(yindex $)})
            end
         end

      end

      % FIXME: remove if not needed
      fun {PermRegForSym AST Params}
      %-------------------------
         % Return y,g or kregister for fSym or fConst
         % Does not return X register
         case AST
         of fConst(K _) then
            k(K)
         [] fSym(Sym _) then
            if {Sym get(type $)}==localised then
               g({Sym get(gindex $)})
            else
               YReg={Sym get(yindex $)}
            in
               if YReg==nil then
                  raise nonAssignedYReg end
               else
                  y(YReg)
               end
            end
         end
      end

      fun {CodeGenInt AST Params}
      %--------------------------
         case AST
         %----------------------
         of fLocal(Decls Body _) then
         %----------------------
            % Should create all without recursive call
            [ {CodeGenDecls Decls Params} {CodeGenInt Body Params}]

         [] fSkip(_) then
            nil
         %---------------------------------
         [] fDefineProc(fSym(Sym _) Args Body _     Pos NewLocals) then
         %  fDefineProc(fSym(Sym _) Args Body Flags Pos NewLocals) then
         %---------------------------------

            %FIXME: the name of the proc can not be available, eg in the case { {GetProc 2} arg1 arg2}

            CA
            OpCodes={NewCell nil}
            % Number of globals, ie variables comint from parent's environment
            GlobalsCount = {List.length NewLocals}
            ArrayFills
         in
            %
            {Show '##############'}
            {System.showInfo 'Procedure '#{Sym get(name $)} }
            {Show '##############'}
            {GenAndAssemble Body Args {Sym get(name $)} d(file:Pos.1 line:Pos.2 column:Pos.3) switches ?CA _} %last argument is ?VS, the virtual string is set by GenAndAssemble.


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

         %---------------
         [] fApply(Sym Args _) then
         %---------------
            R={NewCell nil}
         in
            %if {HasFeature Params procassemble} andthen Params.procassemble then
            %L is the arguments list
            % first move arguments in x registers
            _={List.mapInd Args fun {$ Index AST}
                                    case AST
                                    of fSym(S _) then
                                       SymbolType = {S get(type $)}
                                    in
                                       if SymbolType==localProcId orelse SymbolType==synthetic orelse SymbolType==patternmatch then
                                          if {S get(yindex $)}==nil then
                                             raise missingNeededYIndex end
                                          end
                                          R:={List.append @R [move(y({S get(yindex $)}) x(Index-1))]}
                                       elseif SymbolType==localised then
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
                                    else
                                       AST
                                    end
                                 end}
            % When arguments are in X registers, we can do the call
            case Sym
            of fConst(Const _) then
               {List.append @R [call(k(Const) {List.length Args})] }
            % FIXME: find better naming for Sym2 ?
            [] fSym(_ _) then
               {List.append @R [call({RegForSym Sym Params} {List.length Args})] }
            end

         %--------------------
         [] fAnd(First Second) then
         %--------------------
            [{CodeGenInt First  Params} {CodeGenInt Second  Params}]

         %----------------
         [] fEq(LHS fRecord(fConst(Label _)  Features) _) then
         %----------------
            % Due to previous passes, fRecord can only be in RHS, so we only test it.
            % If we have unification with a record, we need to handle it specifically.

            FeaturesList OrderedFeaturesList Arity OpCodes
            Fills
         in
            % Build feature list with elements being pairs feature#value.
            % This will let us order the list according to the features, and
            % then access the values in the same order (for arrayFill)
            FeaturesList = {List.foldL Features fun{$ A I}
                                    case I
                                    of fColon(fConst(L _) V) then
                                       L#V|A
                                    else
                                       raise constantFeatureExpectedforFRecordInCodeGen end
                                    end
                                 end
                                 nil }
            % The features can be compared with the builtin FeatureLess
            OrderedFeaturesList = {List.sort FeaturesList fun {$ L1#_ L2#_} {Boot_CompilerSupport.featureLess L1 L2} end }

            % Try to make an Arity to use with createRecordUnify
            Arity={CompilerSupport.makeArity Label {List.map OrderedFeaturesList fun{$ L#_} L end } }
            if Arity\=false then
               % If makeArity returned a usable result, it means we need to create a record (ie the arity is not numeric only)
               OpCodes = createRecordUnify(k(Arity) {List.length OrderedFeaturesList}  {RegForSym LHS Params})
            else
               if Label=='|' andthen {List.map OrderedFeaturesList fun {$ L#_} L end}==[1 2] then
                  % in this case we need to create a cons
                  OpCodes = createConsUnify({RegForSym LHS Params})
               else
                  % if makeArity returned false, it means we need to create a tuple, because the feature was all numeric
                  OpCodes = createTupleUnify(k(Label) {List.length OrderedFeaturesList}  {RegForSym LHS Params})
               end
            end
            % after create...Unify, we need to pass values through arrayFills, ordered according to the features.
            Fills={List.map OrderedFeaturesList fun{$ _#V} arrayFill({PermRegForSym V Params}) end }
            {List.append [OpCodes] Fills}
         %----------------
         [] fEq(LHS RHS _) then
         %----------------
            [unify({RegForSym LHS  Params} {RegForSym RHS  Params})]

         %--------------------------------------
         [] fBoolCase(FSym TrueCode FalseCode _) then
         %--------------------------------------
            ErrorLabel={GenLabel}
            ElseLabel={GenLabel}
            EndLabel={GenLabel}
         in
            move({RegForSym FSym Params} x(0))|
            condBranch(x(0) ElseLabel ErrorLabel)|
            %---- true ----
            {CodeGenInt TrueCode Params}|
            branch(EndLabel)|
            %---- error ----
            lbl(ErrorLabel)|
            move(k(badBooleanInIf) x(0))|
            tailCall(k(Exception.raiseError) 1)|
            %---- else ----
            lbl(ElseLabel)|
            case FalseCode
            of fNoElse(_) then
               lbl(EndLabel)|nil
            else
               {CodeGenInt FalseCode Params}|
               % ---- end ----
               lbl(EndLabel)|nil
            end

         %-----------------------------
         [] fCase(TestedValue Clauses Else _) then
         %-----------------------------
            % A case statement is testing a value Val against clauses:
            %
            %   case Val
            %                 -------------------+
            %                 ---+               |
            %   of Pattern1 then |->1 clause     |
            %      Body1         |               |
            %                 ---+               |
            %   [] Pattern2 then                 |-> Clauses sequence for Mozart2 VM
            %      Body                          |
            %   ...                              |
            %   [] PatternN then                 |
            %      BodyN                         |
            %                --------------------+
            %   else
            %      ElseBody
            %   end
            %
            % The Mozart1 VM allowed the compiler to group subsequent clauses of
            % constant patterns (records, integers, atoms, floats,...), but open
            % record patterns could only be handled individually.
            % The first version of this code was doing exactly the same.
            % The current code uses the possibilities offered by the new VM, but
            % still uses the same concepts of clause sequences.
            %
            % A sequence of clauses is a group of subsequent clauses of the same type (constant or open record) and with no guards.
            % As sequence of clauses are grouped, the opcode generated will not
            % have one test instruction per clause, but one test instruction
            % per sequence of clause, and the test result will jump to the corresponding clause' body.
            %
            % This will result in opcodes of this form:
            %   patternMatch(x(0) k('#'(Pat1#lbl1 Pat2#lbl2)))
            %   branch(NextTestLabel)
            %   lbl(1)
            %   Code for clause1
            %   branch(EndLabel)
            %   lbl(2)
            %   Code for clause2
            %   lbl(NextTestLabel)
            %
            %   ... Other clauses ...
            %
            %   lbl(EndLabel)
            %
            % The variables in the patterns are replaced by a object obtained with a newPatMatCapture taking
            % as argument the X register in which to place the value assigned to the capture variable.
            %
            % A clause with guards however must be the only clause in the sequence.
            % This is because after testing the pattern, we need code to test the guards,
            % which is not possible in the opcode showed above.
            % The opcode for clauses with guards looks like this:
            %
            %   patternMatch(x(0) k('#'(Pat1#lbl1)))
            %   lbl(1)
            %     Guards code
            %   condBranch(GuardReg NextTestLabel ErrorLabel)
            %   Code for clause1
            %   branch(EndLabel)
            %   lbl(NextTestLabel)
            %   patternMatch(....)
            %   ...
            %   lbl(EndLabel)
            %
            % If the pattern match is successful, the jump target it the code of the guards, and not the code of the clause.
            % The guards code has to be placed exactly there so that it has access to the captures created by the patternMatch!
            %
            % Code-related explanations
            % -------------------------
            % The jump destination of done by the patternMatch (which is the clause' body or the guards code
            % as illustrated above) has to be identified by a label, found in the variable ThisLabel
            %
            % Currently, it is still possible to generate one test per clause.
            % The function InNewSequence just has to return true in all cases.
            %
            % The code needs to be able to perform a test instruction and
            % jump to the next test if unsuccessful. Hence the code keeps track
            % of both the current test label (in ThisTestLabel) and the next test
            % label (NextTestLabel).
            %
            % The cell Code holds the code generated for all completely visited sequences.
            % The cell CodeBuffer holds the code for all handled clauses of the
            % current sequence. This does not include the prefix of the clause' code.
            %
            % The prefix of a clause' code is simply the code performing the
            % tests of the pattern and the guards, which has to pass
            % successfully for the clause' code to be executed.
            %
            % Due to the difference of code structure between clauses with guards and those without,
            % the prefix is constructed differently in these 2 cases. See function PrefixOfSeq.
            % For clauses without guards, the simplest case, the prefix simply consists in the patternMatch instruction.
            % As multiple clauses can be grouped, the clause' code label has to
            % be introduced in the CodeBuffer, it cannot be set by PrefixOfSeq
            % when closing the sequence.
            %
            % It is the opposite situation in clauses with guards, because the patternMatch should not jump
            % to the clause' body, but to the guards' code. That's why in this case the label ThisLabel is not
            % introduced in the CodeBuffer, because it would then make the patternMatch jump to the clause' body,
            % short-circuiting the guards code.
            %
            % The same applies to the call to UsedSymbolsToYReg that inserts code to make used symbols available in Y registers.
            % For clauses without guards, it is added to CodeBuffer, but for clauses with guards,
            % this code has to be inserted before the guards code, so that capture are available.



            % When a new sequence is started (tested with IsNewSequence), the previous sequence is closed:
            % its prefix followed by the CodeBuffer are appended to Code.
            % After execution of the body of a clause, no code of other clauses
            % should be executed, hence the need to have an EndLabel.


            fun {HandleCase Clauses Params}

               fun {UsedSymbolsToYReg UsedSymbols}
                  % Generate opcode to move symbols used in a pattern to Y registers
                  {List.map UsedSymbols fun{$ Sym} move(x({Sym get(xindex $)}) y({Sym get(yindex $)})) end }
               end

               fun {TransformPattern Pattern XIndex UsedSymbols }
                  % This function will return the value corresponding to this clause in the record passed to patternMatch
                  % The XIndex cell contains the last assigned X register
                  % It collects symbols found in the pattern in the UsedSymbols list
                  case Pattern
                  of fConst(Val _) then
                     % Call TransformPattern on the value in fConst, as it could be a safe holding a capture
                     {TransformPattern Val XIndex UsedSymbols }
                  [] fPatMatConjunction(LHS RHS _) then
                     {Boot_CompilerSupport.newPatMatConjunction '#'({TransformPattern LHS XIndex UsedSymbols} {TransformPattern RHS XIndex UsedSymbols} ) }

                  [] fSym(Sym _) then
                     % If this is a symbol, it is a capture in the pattern match
                     % Assign the next X register to that symbol, add it to the
                     % list of used symbols, and replace it with a newPatMatCapture.
                     XIndex:=@XIndex+1
                     {Sym set(xindex @XIndex)}
                     UsedSymbols:=Sym|@UsedSymbols
                     {Boot_CompilerSupport.newPatMatCapture @XIndex}
                  [] fNamedSideCondition(RealPattern _     _      _           _) then
                    %fNamedSideCondition(RealPattern Decls Guards GuardSymbol Pos) then
                     % For a guarded clause, only look at its pattern
                     {TransformPattern RealPattern XIndex UsedSymbols }
                  [] fOpenRecord(fConst(RecordLabel _) Features) then
                     % OpenRecords are replaced by a newPatMatOpenRecord.
                     % Build feature list with elements being pairs feature#value.
                     % This will let us order the list according to the features, and
                     % then access the values in the same order.
                     % Same code as handling of records with createRecordUnify opcode
                     FeaturesList = {List.foldL Features fun{$ A I}
                                            case I
                                             of fColon(fConst(L _) V) then
                                                L#V|A
                                             else
                                                raise constantFeatureExpectedforFOpenRecord end
                                             end
                                          end
                                          nil }
                     OrderedFeaturesList = {List.sort FeaturesList fun {$ L1#_ L2#_} {Boot_CompilerSupport.featureLess L1 L2} end }

                     Arity
                  in
                     % Set force to true as we need the arity even if it is a cons or a tuple
                     % Notice the recursive call on each feature
                     Arity={Boot_CompilerSupport.makeArityDynamic RecordLabel {List.toTuple '#' {List.map OrderedFeaturesList fun{$ L#_} {TransformPattern L  XIndex UsedSymbols } end }} true }
                     {Boot_CompilerSupport.newPatMatOpenRecord Arity {List.toTuple '#' {List.map OrderedFeaturesList fun{$ _#V} {TransformPattern  V XIndex UsedSymbols } end }}}

                  [] fRecord(fConst(RecordLabel _) Features) then
                     %replace the fRecord by the oz value it represents. This mean we build the record from the AST.
                     {List.foldL Features  fun{$ Acc I}
                                                case I
                                                of fColon(fConst(L _) F) then
                                                   {Record.adjoin Acc RecordLabel(L:{TransformPattern F XIndex UsedSymbols })}
                                                   else Acc
                                                end
                                             end RecordLabel()}
                  else
                     % Pattern is a record
                     if {IsSafe Pattern} then
                        % Open safe and transform it.
                        {TransformPattern {AccessSafe Pattern} XIndex UsedSymbols }
                     elseif {Record.is Pattern} then
                        % Recursively transform each value of the record.
                        % FIXME: we could limit the depth of the recursive call as this code even recurses into the position features.
                        {Record.map Pattern fun{$ I} {TransformPattern I XIndex UsedSymbols } end}
                     else
                        % This is a value, simply return it.
                        Pattern
                     end
                  end
               end


               fun {PrefixOfSeq TestedValue Pattern PatternMatchRecord ThisLabel NextTestLabel ErrorLabel UsedSymbols Params}
                  case Pattern
                  of fNamedSideCondition(_           _     Guards GuardSymbol _) then
                    %fNamedSideCondition(RealPattern Decls Guards GuardSymbol _)
                     % This is a clause with guards. The label of the guards code is
                     % ThisLabel, so that the patternmatch jumps to the guards code.

                     % Place TestedValue in x(0) as guards code could have wiped it
                     move({RegForSym TestedValue Params} x(0))|
                     patternMatch(x(0) k(PatternMatchRecord))|
                     branch(NextTestLabel)|
                     lbl(ThisLabel)|
                     % Make captures available to guards code
                     {UsedSymbolsToYReg UsedSymbols}|
                     {CodeGenInt Guards Params}|
                     move({RegForSym GuardSymbol Params} x(1))|
                     condBranch(x(1) NextTestLabel ErrorLabel)|
                     nil
                  else
                     % This is the prefix of a sequence. In this case, the label for
                     % each clause' code is set already in the loop, so we do not
                     % include it here.

                     % Place TestedValue in x(0) as guards code could have wiped it
                     move({RegForSym TestedValue Params} x(0))|
                     patternMatch(x(0) k(PatternMatchRecord))|
                     branch(NextTestLabel)|
                     nil
                  end
               end



               % Initialise variables
               % The current sequence we're in
               SequenceType={NewCell none}
               % The label for this clause' code
               ThisLabel={NewCell {GenLabel}}
               % The label for the possibly grouped clauses test
               ThisTestLabel={NewCell unit}
               % The label for the next possibly grouped clauses test
               NextTestLabel={NewCell {GenLabel}}
               % Code for all completed sequences
               Code={NewCell nil}
               % Code for all clauses part of the current sequence
               CodeBuffer={NewCell nil}
               % The record holding all tests for visited clauses in the current sequence
               PatternMatchRecord={NewCell '#'()}
               % Number of clauses already visited in the current sequence.
               % Used to compute index in the PatternMatchRecord
               SeqLen={NewCell 0}
               % Label at the end of the code for the case
               EndLabel={GenLabel}
               % Label identifying error code
               ErrorLabel={GenLabel}

               % Cell holding the pattern of the last visited clause.
               % Needed for the closing code adding the code for the last sequence
               LastPattern={NewCell unit}

               % Collect symbols used in clause code.
               % Declared here because it needs to be passed to PrefixOfSeq out of the {List.forAllInd Clauses} loop.
               UsedSymbols={NewCell nil}

               fun {IsNewSequence Label Type}
                  % Only clauses with no guards are part of sequences
                  % AS fRecord are stored in a safe in a fConst we can just test if the label is fConst
                  % Return true to debug and see all clauses generated seperately
                  % Seems that by using patMatOpenRecord we can put all in one sequence
                  if Type==none then
                     % First iteration
                     true
                  elseif Label==fNamedSideCondition then
                     % Beginning a clause with guards
                     true
                  elseif Type==fNamedSideCondition then
                     % This clause follows a clause with guards
                     true
                  else
                     false
                  end
               end
            in
               {List.forAllInd Clauses proc{$ Ind fCaseClause(Pattern Body)}
                                          PatternLabel={Record.label Pattern}
                                          XIndex
                                          PatternForRecord
                                          PatternIndex
                                       in
                                          %{Show 'Starting work on pattern'}
                                          %{Show '************************'}
                                          %{DumpAST.dumpAST Pattern _}


                                          % If we start a new sequence, add the code of the previous sequence to Code
                                          if {IsNewSequence PatternLabel @SequenceType} then
                                             % Do not do it at the first iteration
                                             if @SequenceType\=none then
                                                if @ThisTestLabel\=unit then
                                                   % Do not include a label for first test
                                                   Code:=@Code|lbl(@ThisTestLabel)|{UsedSymbolsToYReg @UsedSymbols}|{PrefixOfSeq TestedValue @LastPattern @PatternMatchRecord @ThisLabel @NextTestLabel ErrorLabel @UsedSymbols Params}|@CodeBuffer|nil
                                                else
                                                   Code:=@Code|{PrefixOfSeq TestedValue @LastPattern @PatternMatchRecord @ThisLabel @NextTestLabel ErrorLabel @UsedSymbols Params}|@CodeBuffer|nil
                                                end
                                             end
                                             % Reset loop variables
                                             CodeBuffer:=nil
                                             PatternMatchRecord:='#'()
                                             SequenceType:=PatternLabel
                                             SeqLen:=0
                                             ThisTestLabel:=@NextTestLabel
                                             NextTestLabel:={GenLabel}
                                             SequenceType:=PatternLabel
                                          end

                                          ThisLabel:={GenLabel}


                                          % Collect symbol used, so that they can be stored in Y regs to
                                          % keep them accessible in this branch
                                          % Reset it for each clause
                                          UsedSymbols:=nil

                                          % Index of last assigned X register.
                                          % Initialise it with 0 to keep the value we test against in x(0)
                                          % FIXME: this can certainly by changed as we now put the tested value
                                          % in x(0) at the beginning of each test.
                                          XIndex={NewCell 0}

                                          % Build record used in the pattern matching instruction
                                          PatternForRecord = {TransformPattern Pattern XIndex UsedSymbols}

                                          % Needed to avoid a parse error
                                          % Reminder: SeqLen is the number of clauses completed in the current sequence.
                                          % Hence this this clause' pattern index is one more.
                                          PatternIndex=@SeqLen+1

                                          PatternMatchRecord:={Record.adjoin @PatternMatchRecord '#'(PatternIndex:PatternForRecord#@ThisLabel)}
                                          if @SequenceType==fNamedSideCondition then
                                             CodeBuffer:=@CodeBuffer|{CodeGenInt Body Params}|branch(EndLabel)|nil
                                          else
                                             CodeBuffer:=@CodeBuffer|lbl(@ThisLabel)|{UsedSymbolsToYReg @UsedSymbols}|{CodeGenInt Body Params}|branch(EndLabel)|nil
                                          end
                                          % Update number of clauses visited in current sequence and LastPattern
                                          SeqLen:=@SeqLen+1
                                          LastPattern:=Pattern
                                       end}
               % Add the code for the last clause, for error handling, and the end label
               Code:=@Code|
                     % FIXME: this if expression in the list might not be the clearest code
                     if @ThisTestLabel\=unit then
                        lbl(@ThisTestLabel)|{PrefixOfSeq TestedValue @LastPattern @PatternMatchRecord @ThisLabel @NextTestLabel ErrorLabel @UsedSymbols Params}
                     else
                        {PrefixOfSeq TestedValue @LastPattern @PatternMatchRecord @ThisLabel @NextTestLabel ErrorLabel @UsedSymbols Params}
                     end|
                     @CodeBuffer|
                     %---- error ----
                     lbl(ErrorLabel)|
                     move(k(errorInCase) x(0))|
                     tailCall(k(Exception.raiseError) 1)|
                     % else branch
                     lbl(@NextTestLabel)|
                     case Else
                     of fNoElse(_) then
                        lbl(EndLabel)|nil
                     else
                        {CodeGenInt Else Params}|
                        lbl(EndLabel)|nil
                     end
               {List.flatten @Code}
            end
         in
            {HandleCase Clauses Params}|
            nil

         [] fRecord(fConst(_ _) _) then
            raise unhandledRecordType end
         end
      end
      InitialParams = {Record.adjoin params(indecls:false opCodes:{NewCell nil} currentIndex:{NewCell 0}) CallParams}
      OpCodes
   in
      % append return
      OpCodes={List.append {List.flatten {CodeGenInt AST InitialParams} } ['return'()]}

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
      fun { YAssigner Xs}
         Counter={NewCell 0}
         proc {YAssignerInt X}
            if {List.is X} then
               {ForAll X YAssignerInt}
            else
               case X
               of fSym(Sym _) then
                  if {Sym get(yindex $)}==nil then
                     {Sym set(yindex @Counter)}
                     Counter:=@Counter+1
                  end
               else
                  {DefaultPassNoParams X YAssigner _}
               end
            end
         end
      in
         {YAssignerInt Xs}
         @Counter
      end
   in
      {Show 'Will assign Ys'}
      YCounter={YAssigner Args}
      {Show 'Done Assigning Y'}
      if YCounter>0 then
         % Use List.forAllInd
         {For 0 YCounter-1 1
            proc {$ I}
            {Show I}
            {Show {Nth Args I+1}}
              Prefix:={List.append @Prefix [move(x(I) y({{Nth Args I+1}.1 get(yindex $)}) )]}
            end}
      end
      {Show 'Assigned Ys:'#YCounter}
      OpCodes := {List.append @OpCodes {CodeGen AST params(prefix:@Prefix procassemble:true currentIndex:{NewCell YCounter} )}}
      {Show 'CodeArea for '#PrintName#'built with this code:'}
      {ForAll @OpCodes Show}
      {AssembleAST Arity @OpCodes PrintName DebugData Switches ?CodeArea ?VS}
   end
end

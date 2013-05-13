functor
   import
      System(showInfo show:Show)
      Module
      OS
      DumpAST at '../lib/DumpAST.ozf'
      Helpers( symbol:Symbol
               syntheticSymbol:SyntheticSymbol
               wrapInFAnd:WrapInFAnd
               wrapIn:WrapIn
               unWrapFAnd:UnWrapFAnd
               listToAST:ListToAST
               indexOf:IndexOf
               firstOfP:FirstOfP
               genLabel:GenLabel
               getPos:GetPos
               defaultPass:DefaultPass
               defaultPassNoParams:DefaultPassNoParams
               storeInSafe:StoreInSafe
               accessSafe:AccessSafe
               isSafe:IsSafe
               extractFunctorSpecs:ExtractFunctorSpecs
               ) at '../lib/Helpers.ozf'
   export
      Namer
   define

   AugmentedBase={AdjoinAt {AdjoinAt {AdjoinAt Base 'Show' Show} 'OS' OS} 'Module' Module}
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
      fun {NamerForMethDecls AST Params}
         % Namer for class method headers
         % Only accepts fDollar and fWildcard in addition to what NamerForDecls does
         case AST
         of fDollar(_) then
            AST
         [] fWildcard(Pos) then
            NewSym=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            NewSym
         else
            {NamerForDecls AST Params}
         end
      end


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
         [] fDollar(_) then
            % Leave netsing marker in declarations of functional procedures
            AST
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
      %------------------------------------
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
            % FIXME: change it to one fCase with pattern being a list, as described
            % at http://www.mozart-oz.org/documentation/notation/node6.html#label22
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
            % Function called by NamerForBody when it encounters a function or
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
            % Acc.args accumulates effective parameters to be present in transformed declaration
            % Acc.patterns accumulates value#pattern pairs that will be used to wrap the body in case tests
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
                  % FIXME: we can replace the 2 list visits by one
                  % Use the new arguments list
                  {List.map {List.reverse @(ArgsDesc.args)} fun {$ I} {NamerForMethDecls I Params} end }
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
                  {List.map Args fun {$ I} {NamerForMethDecls I Params} end }
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
               {DumpAST.dumpAST AST _}
               raise unnamedVariable end
               AST
            end

         %-----------------------------
         [] fCase(Val Clauses Else Pos) then
         %-----------------------------
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

         %-------------------------------
         [] fClass(Var Specs Methods Pos) then
         %-------------------------------
            fun {NameMethodLabel fMeth(M Body Pos) Params}
               % Function naming method labels
               % Declarations to wrap the class in are accumulated in Params.decls
               % Initialisation code to be put before the class code, but inside the declarations, is accumulated in Params.init
               %
               % fVar labels are replaced by a new symbol, which will be declared and initialised to a new name value
               % fEscape labels use the environment to find the symbol to use
               fun {NameMethodLabelInt Name Params}
                  case Name
                  of fAtom(L Pos) then
                     fConst(L Pos)
                  [] fVar(V Pos) then
                     NewSym=fSym({Params.env setSymbol(V Pos $)} Pos)
                  in
                     (Params.decls):=NewSym|@(Params.decls)
                     (Params.init):=fApply( fConst(NewName Pos) [NewSym] Pos)|@(Params.init)
                     NewSym
                  [] fEscape(V _) then
                     {NamerForBody V Params}
                  [] fRecord(Name Args) then
                     fRecord( {NameMethodLabelInt Name Params} Args)
                  [] fOpenRecord(Name Args) then
                     fOpenRecord( {NameMethodLabelInt Name Params} Args)
                  [] fEq(M V Pos) then
                     % Do not name V here, only name it when naming the body as this has to be available in the body on this method only
                     fEq({NameMethodLabelInt M Params} V Pos)
                  end
               end
            in
               fMeth({NameMethodLabelInt M Params} Body Pos)
            end

            fun {NameMethod Method Params}
               NewArgs NewBody
               fun {NameMethodWithArgs Args Body Params}
                  % Method with arguments
                  % Args are available only to the method, so we backup the environment
                  % to be able to restore it after we traversed this method
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
                  %fMeth(Type(Name NewArgs) NewBody Pos)
                  NewArgs#NewBody
               end
            in
               %{Show 'Mthod to be named:'}
               %{DumpAST.dumpAST Method _}
               % the env backup and restore is at this level to be able to name the variable capturin the method head
               case Method
               of fMeth(fRecord(Name Args) Body Pos) then
                  NewArgs
                  NewBody
                  R
               in
                  {Params.env backup()}
                  NewArgs#NewBody={NameMethodWithArgs Args Body Params}
                  R=fMeth(fRecord(Name NewArgs) NewBody Pos)
                  {Params.env restore()}
                  R
               [] fMeth(fOpenRecord(Name Args) Body Pos) then
                  NewArgs
                  NewBody
                  R
               in
                  {Params.env backup()}
                  NewArgs#NewBody={NameMethodWithArgs Args Body Params}
                  R=fMeth(fOpenRecord(Name NewArgs) NewBody Pos)
                  {Params.env restore()}
                  R
               [] fMeth(fEq(fRecord(Name Args) H Pos) Body MPos) then
                  NewArgs
                  NewBody
                  NamedH
                  R
               in
                  {Params.env backup()}
                  NamedH={NamerForDecls H Params}
                  NewArgs#NewBody={NameMethodWithArgs Args Body Params}
                  R=fMeth(fEq(fRecord(Name NewArgs) NamedH Pos) NewBody MPos)
                  {Params.env restore()}
                  R
               [] fMeth(fEq(fOpenRecord(Name Args) H Pos) Body MPos) then
                  NewArgs
                  NewBody
                  NamedH
                  R
               in
                  {Params.env backup()}
                  NamedH={NamerForDecls H Params}
                  NewArgs#NewBody={NameMethodWithArgs Args Body Params}
                  R=fMeth(fEq(fOpenRecord(Name NewArgs) NamedH Pos) NewBody MPos)
                  {Params.env restore()}
                  R
               [] fMeth(fEq(L H Pos) Body MPos) then
                  NewBody
                  NamedH
                  R
               in
                  {Params.env backup()}
                  NamedH={NamerForDecls H Params}
                  NewBody={NamerForBody Body Params}
                  R=fMeth(fEq(L NamedH Pos) NewBody MPos)
                  {Params.env restore()}
                  R
               else
                  Name Body MPos
               in
                  % Method without arguments
                  % It is transformed in the same form as methods with arguments, but with an empty arguments list
                  % Args are available only to the method, so we backup the environment
                  fMeth(Name Body MPos)=Method
                  % this is a method without argument
                  fMeth(fRecord( Name nil) {NamerForBody Body Params} MPos)
               end
            end
            NewMethods
            NamedLabelsMethods
            NewSpecs
            NewParams={Record.adjoin Params params(decls:{NewCell nil} init:{NewCell nil})}
            NamedClass
            ClassWithInit
            ClassWithDecls
         in
            % First name the class, then backup the env, and then name the methods.
            % This is needed for private methods indicated by the same variable name as the
            % class. See test 321
            NamedClass={NamerForBody Var Params}
            {NewParams.env backup()}
            % Name method first, so the private methods are available in the methods bodies
            NamedLabelsMethods={List.map Methods fun {$ I} {NameMethodLabel I NewParams} end }
            NewMethods={List.map NamedLabelsMethods fun {$ I} {NameMethod I NewParams} end }
            NewSpecs={List.map Specs fun {$ I} {NamerForBody I NewParams} end }
            if @(NewParams.init)\=nil then
               ClassWithInit={WrapInFAnd {List.append [fClass(NamedClass NewSpecs NewMethods Pos)] @(NewParams.init)}}
            else
               ClassWithInit=fClass( NamedClass NewSpecs NewMethods Pos)
            end
            if @(NewParams.decls)\=nil then
               ClassWithDecls=fLocal({WrapInFAnd @(NewParams.decls)} ClassWithInit Pos)
            else
               ClassWithDecls=ClassWithInit
            end
            {NewParams.env restore()}
            ClassWithDecls

         %-------------------------
         [] fFOR(Patterns Body Pos) then
         %-------------------------
            fun {TransformForPatterns Args Body}
               % FIXME: can this desugar be moved to desugar?
               case Args
               of forPattern(V forGeneratorList(L))|Ps then
                  {NamerForBody fApply(fConst(ForAll Pos) [L fProc(fDollar(Pos) [V] {TransformForPatterns Ps Body} nil  Pos)] Pos) Params}
               [] forPattern(V forGeneratorInt(Start End Step))|Ps then
                  fun {ExplicitStep Step}
                     case Step
                     of unit then
                        % Start is a global
                        fConst(1 {GetPos Start})
                     else
                        Step
                     end
                  end
               in
                  {NamerForBody fApply(fConst(For Pos) [Start End {ExplicitStep Step} fProc(fDollar(Pos) [V] {TransformForPatterns Ps Body} nil  Pos)] Pos) Params}
               % FIXME C generator
               % and break/continue http://www.mozart-oz.org/home/doc/loop/node1.html#chapter.official
               %
               %[] forPattern(V forGeneratorC(Start Cond Next))|Ps then
               [] nil then
                  Body
               end
            end
         in
            {NamerForBody {TransformForPatterns Patterns Body} Params}

         %-------------------------------------------------
         [] fTry(Body fCatch(Clauses CatchPos) Finally Pos) then
         %-------------------------------------------------
            % User NamerForCaptures for the pattern in Clause,
            % as this will be desugared in a case instruction using the same clauses
            NewParams={Record.adjoin Params params(captures:{NewCell nil})}
            NamedTry
         in
            NamedTry=fTry( {NamerForBody Body Params}
                  fCatch( {List.map Clauses  fun {$ fCaseClause(Val Body)}
                     NewVal NewBody
                  in
                     {NewParams.env backup()}
                     NewVal={NamerForCaptures Val NewParams}
                     NewBody={NamerForBody Body NewParams}
                     {NewParams.env restore()}
                     fCaseClause(NewVal NewBody)
                  end}
                          CatchPos)
                  {NamerForBody Finally Params}
                  Pos)
            if {List.length @(NewParams.captures)}>0 then
               fLocal( {WrapInFAnd @(NewParams.captures)}
                        NamedTry
                        Pos
                     )
            else
               NamedTry
            end

         %-------------------------------------------------
         [] fFunctor(Id ExportImportPrepareDefine Pos) then
         %-------------------------------------------------
            % imports => namerfordecls on modules imported
            % aliases => TODO
            %
            Requires
            Imports
            Prepare
            Define
            Exports
            {ExtractFunctorSpecs ExportImportPrepareDefine ?Requires ?RequireItems ?Imports ?ImportItems ?Prepare ?Define ?Exports ?ExportItems}
            ImportItems ExportItems RequireItems

            NewId NewExports NewImports NewPrepare NewDefine
            fun {NameToAtom N}
               Head Tail
            in
               {List.takeDrop {VirtualString.toString N} 1 Head Tail}
               {String.toAtom {Char.toLower Head.1}|Tail}
            end
         in

            {Params.env backup}
            NewPrepare = {List.map Prepare   fun {$ fPrepare(Decls Body Pos)}
                                                fPrepare({NamerForDecls Decls Params} {NamerForBody Body Params} Pos)
                                             end}
            NewImports = fImport({List.map ImportItems   fun {$ fImportItem(Id Aliases At)}
                                                            NewAliases = {List.map Aliases fun {$ '#'(A F)} '#'({NamerForDecls A Params} {NamerForBody F Params}) end}
                                                         in
                                                            fImportItem({NamerForDecls Id Params} NewAliases {NamerForBody At Params})
                                                         end} pos)
            NewDefine  = {List.map Define    fun {$ fDefine(Decls Body Pos)}
                                                fDefine({NamerForDecls Decls Params} {NamerForBody Body Params} Pos)
                                             end}
            NewExports = fExport({List.map ExportItems   fun {$ fExportItem(I)}
                                                            case I
                                                            of fColon(_ _) then
                                                               fExportItem({NamerForBody I Params})
                                                            else
                                                               fVar(Name Pos)=I
                                                            in
                                                               fExportItem(fColon(fConst({NameToAtom Name} Pos) {NamerForBody I Params}))
                                                            end
                                                         end} pos)
            NewId={NamerForBody Id Params}
            {Params.env restore}

            % We reuse the order of the list's items later
            fFunctor(NewId  [NewPrepare.1 NewImports NewDefine.1 NewExports] Pos)
         %-----------------------
         [] fOpApply(Op Args Pos) then
         %-----------------------
            fOpApply(Op {List.map Args fun {$ I} {NamerForBody I Params} end} Pos)
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
      {Show '-------'}
      {Show 'Namer'}
      {Show '-------'}
      {NamerForBody AST InitialParams}
   end

end

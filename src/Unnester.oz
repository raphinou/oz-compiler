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
   Unnester
define
   %#################
   fun {Unnester AST}
   %#################
      % Functions to update Params.tail
      % If a node is not in tail position, its childen are not in tail position
      % if a node is in tail position, only its last child is in tail position.
      % So one way to do it is reuse the parent's tail flag for the last child,
      % and set it to false for all others
      % It is just when entering a proc that we need to override the value with
      % true for the body
      fun {InTail Params}
         {Record.adjoin Params params(tail:true)}
      end
      fun {NoTail Params}
         {Record.adjoin Params params(tail:false)}
      end


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
         ClassExt=OoExtensions.'class'
         Raise=Exception.'raise'
      in
         % Handles the binding of a variables to a complex expression
         case AST
         of fProc(fDollar(_) Args Body Flags Pos) then
            {UnnesterInt fProc(FSym Args Body Flags Pos) Params }
         [] fApply(fConst(!Raise _) _ _) then
            % simply keep the raise, cf http://www.mozart-oz.org/documentation/notation/node6.html#label27
            AST
         [] fApply( fConst(!ClassExt Pos) L Pos) then
            % See T defined earlier. Done to pass parser
            fApply( fConst(OoExtensions.'class' Pos) {List.append {List.take L 6} [FSym]}  Pos)
         [] fApply(Proc Args Pos) then
             % All argument are all symbols when we get here
             % Simply injects the unified symbol FSym in the fApply arguments, either in
             % the location of the fDollar, or as the last argument.
             % After that it call the unnester again to unnest complext arguments of fapply
            fun {InjectSymInRecord AST DollarSym Found}
               % Looks in AST for fDollar.
               % If found, creates a new symbol which replaces the fDollar and is place in DollarSym
               case AST
               of fRecord(L Features) then
                  fRecord(L {List.map Features fun {$ I} {InjectSymInRecord I DollarSym Found} end })
               [] fColon(F V) then
                  fColon(F {InjectSymInRecord V DollarSym ?Found})
               [] fDollar(_) then
                  Found:=true
                  DollarSym
               else
                  AST
               end
            end
            fun {InjectSym FSym Args HadDollar}
               %This function injects the symbol to assign to in the arguments
               %list, either in the dollar location or as last argument.
               case Args
               of fDollar(_)|Rs then
                  if HadDollar then
                     raise multipleNestingMarkersInCallArguments end
                  end
                  FSym|{InjectSym FSym Rs true}
               [] R|Rs then
                  Res
                  Found={NewCell false}
               in
                  Res={InjectSymInRecord R FSym Found}
                  if @Found andthen HadDollar then
                     raise multipleNestingMarkersInArgs end
                  end
                  Res|{InjectSym FSym Rs (@Found orelse HadDollar)}
               [] nil then
                  if HadDollar then
                     nil
                  else
                     FSym|nil
                  end
               end
            end
            R
         in
            {UnnesterInt fApply(Proc {InjectSym FSym Args false} Pos) Params }
         [] fAnd(First Second) then
            % the result of a sequence of instructions is the value of the last one
            % Recursive call to get to the end of the sequence
            %FIXME: set Pos
            %FIXME: place the recursive call depper in the subtree (on the fEq)?
            {UnnesterInt fAnd(First fEq(FSym Second {GetPos Second})) Params}
         [] fLocal(Decls Body Pos) then
            % the value of a local..in..end is the value of the last expression in the body
            {UnnesterInt fLocal(Decls fEq(FSym Body Pos) Pos) Params}
         [] fBoolCase(Cond TrueCode FalseCode Pos) then
            % A = if Cond then TrueCode else FalseCode end
            % become
            % if Cond then A=TrueCode else A=FalseCode end
            {UnnesterInt fBoolCase(Cond fEq(FSym TrueCode Pos) fEq(FSym FalseCode Pos) Pos) Params}
         [] fCase(Val Clauses Else=fNoElse(_) Pos) then
            {UnnesterInt fCase(Val {List.map Clauses fun {$ fCaseClause(Pattern Body)} fCaseClause( Pattern fEq(FSym Body Pos) ) end} Else Pos) Params}
         [] fCase(Val Clauses Else Pos) then
            {UnnesterInt fCase(Val {List.map Clauses fun {$ fCaseClause(Pattern Body)} fCaseClause( Pattern fEq(FSym Body Pos) ) end} fEq(FSym Else Pos) Pos) Params}
         [] fRecord(_ _) then
            % record has already been unnested before the call to BindVarToExpr (see call to BindVarToExpr for fRecord), just return the fEq
            %FIXME: set pos!
            %fEq(FSym AST {GetPos AST})
            {UnnestFRecord FSym AST Params}
         [] fTry(Body fCatch([fCaseClause(E Case)] CatchPos) Finally Pos) then
            % Introduce new symbol to avoid corner case of test 372
            NewSym=fSym({New SyntheticSymbol init(Pos)} Pos)
            BodyPos={GetPos Body}
         in
            % Inject unification in the code tried and in the catch clauses
            {UnnesterInt fTry(fLocal( NewSym fAnd(fEq(NewSym Body BodyPos) fEq(FSym NewSym BodyPos)) BodyPos) fCatch([fCaseClause(E fEq(FSym Case Pos))] CatchPos) Finally Pos) Params}
         else
            {Show "FSym:"}
            {DumpAST.dumpAST FSym _}
            {Show "AST:"}
            {DumpAST.dumpAST AST _}
            raise unexpectedASTForBindVarToExpr end
         end
      end

      fun {UnnestFApply AST=fApply(Op Args Pos) Params}
      %------------------------------------------------
         % Transforms the AST such that fApply args are all elementary (fSym or
         % fConst). Creates new symbols unified with complex arguments. Wraps
         % the fApply in fLocals for each new symbol created.
         % To handle fDollars in record arguments, the transformation is done in 2 steps.
         % 1) unnest arguments and replace fDollar by a new sym DollarSym if it is encountered
         % 2) if a fDollar was found, wrap all the Code obtained in step 1 in a 'local DollarSym in Code DollarSym end'
         % The two steps are necessary because:
         % - if no fDollar is found, no DollarSym should be declared
         % - if a fDollar is found, the DollarSym should be declared outside of the record.
         % does not use Params
         fun {CheckDollarInArg AST DollarSym}
            % Looks in AST for fDollar.
            % If found, creates a new symbol which replaces the fDollar and is place in DollarSym
            case AST
            of fRecord(L Features) then
               fRecord(L {List.map Features fun {$ I} {CheckDollarInArg I DollarSym} end })
            [] fColon(F V) then
               fColon(F {CheckDollarInArg V DollarSym})
            [] fDollar(_) then
               NewSym=fSym({New SyntheticSymbol init(Pos)} Pos)
            in
               if @DollarSym\=unit then
                  raise multipleNestingMarker  end
               end
               % FSym is a global, argument of UnnestFApply
               DollarSym:=NewSym
               @DollarSym
            else
               AST
            end
         end
         fun {UnnestFApplyInt FApplyAST NewArgsList ArgsRest DollarSym}
            % When we get here, the FApplyAST cannot be part of a unification anymore.
            % If it was, the unified symbol has already been injected in the args (in the dollar location
            % or as last argument) by BindVarToExpr.
            % It still has to check the presence of fDollar though with CheckDollarInArg
            % for example to cover this case: {Show {GetVal rec($) }} See test 068.
            %
            % Unnest all arguments one by one. This is Step 1
            % Elementary arguments are left untouched
            % Complex arguments are extracted from the arguments list by:
            % - declaring a new symbol
            % - unifying this new symbol with the argument
            % - replacing the argument by the new symbol in the argument list.
            % If a fDollar is found in a record, this function simply extracts the
            % whole record from the argument list, with the fDollar replaced by a new DollarSym.
            % The declaration of the DollarSym is done in step 2

            case ArgsRest
            of X|Xs then
               % The recursive calls haven't reached the end of the argument list.
               % Handle the head of the remaining list, and make a recursive call
               if {IsElementary X} then
                 {UnnestFApplyInt FApplyAST X#unit#unit|NewArgsList Xs DollarSym}
               else
                  NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
                  DollarResult={CheckDollarInArg X DollarSym}
               in
                  % If the argument is not $, include a unification before
                  % fLocal(NewSymbol
                  %        fAnd( {UnnesterInt fEq(NewSymbol {CheckDollarInArg X DollarSym}  Pos) {NoTail Params}}
                  %              {UnnestFApplyInt FApplyAST NewSymbol|NewArgsList Xs DollarSym}) Pos)
                  % FIXME: with this test we can have NewSym=ArgSym as unification which is not useful, it should
                  % keep ArgSym and not use NewSym in that case
                  %if DollarResult==X then
                  %   {UnnestFApplyInt FApplyAST NewSymbol#NewSymbol#unit|NewArgsList Xs DollarSym}
                  %else
                     {UnnestFApplyInt FApplyAST NewSymbol#NewSymbol#fEq(NewSymbol DollarResult Pos)|NewArgsList Xs DollarSym}
                  %end
               end
            else
               Unifications
               Decls
               FinalArgs
               ResultApply
               UniRes
               DeclsRes
            in
               Unifications={List.map {List.filter NewArgsList fun{$ _#_#Uni} Uni\=unit end}
                                      fun {$ _#_#Uni} Uni end}
               Decls = {List.map  {List.filter NewArgsList fun{$ _#Sym#_} Sym\=unit end}
                                  fun {$ _#Sym#_} Sym end}
               FinalArgs = {List.reverse {List.mapInd  NewArgsList fun {$ Ind Arg#_#_} Arg end} }

               ResultApply=case Op
               of fApply(_ _ _) then
                  NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
               in
                  % When the proc/fun called is itself the result of a proc/fun call, we need to recursively handle it.
                  {UnnesterInt fLocal( NewSymbol
                                       fAnd(fEq(NewSymbol Op Pos)
                                            fApply(NewSymbol FinalArgs Pos))
                                       Pos)
                               Params}
               else
                  fApply(Op FinalArgs Pos)
               end

               if {List.length Unifications}>0 then
                  UniRes=fAnd({UnnesterInt {WrapInFAnd Unifications} {NoTail Params}} ResultApply)
               else
                  %{Show 'debug no unifications needed'}
                  UniRes=ResultApply
               end


               if {List.length Decls }>0 then
                  DeclsRes = fLocal({WrapInFAnd Decls} UniRes Pos)
               else
                  DeclsRes=UniRes
               end
               if {List.length Unifications}>0 then
                  {UnnesterInt DeclsRes Params}
               else
                  DeclsRes
               end

            end
         end
         DollarSym={NewCell unit}
         Tmp
      in
         Tmp={UnnestFApplyInt AST nil Args DollarSym}
         if @DollarSym==unit then
            % No fDollar was found, so no new symbol was created, and this is a statement
            Tmp
         else
            % A new symbol was created to replace a fDollar.
            % This means it is an expression, transform it as such
            {UnnesterInt fLocal(@DollarSym fAnd(Tmp @DollarSym) pos) Params}
         end
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
            fLocal( NewSymbol
                    {UnnesterInt fAnd( fEq(NewSymbol LHS Pos)
                                       fEq(NewSymbol RHS Pos)) Params}
                    Pos)
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
            fLocal( NewSymbol
                    {UnnesterInt fAnd( fEq( NewSymbol Cond Pos)
                                       fBoolCase(NewSymbol TrueCode FalseCode Pos)) Params}
                                       Pos)
         end
      end


      fun {UnnestFCase fCase(TestedValue Clauses Else Pos) Params}
      %------------------------------------------------------------------
         % if the condition is elementary, simply unnest Clauses
         % do not change the tail flag in Params, as all Clauses and Else code are in tail position if the
         % case is in tail position
         % if the condition is non elementary, create a new symbol to be used as the condition.
         if {IsElementary TestedValue} then
            fCase(TestedValue {List.map Clauses fun{$ I} {UnnesterInt I Params} end } {UnnesterInt Else Params}  Pos)
         else
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal( NewSymbol
                    {UnnesterInt fAnd(fEq( NewSymbol TestedValue Pos)
                                      fCase(NewSymbol Clauses Else Pos)) Params}
                                      Pos)
         end
      end


      fun {UnnestFRecord FSym AST Params}
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
            % To declare all symbols with one fLocal, we collect all info in a list
            % of items FV#Decls#Uni with three components:
            % - FV is the Feature-Value pair to be put in the record definition
            % - Decls is the symbol to be declared, replacing the complex value in FV if any. unit if none
            % - Uni is the unification needed to unnest this pair, unit if none.

            % FIXME: set Pos!
            % fRecords do not have a position feature, so get position from AST
            % -------------------------------------------------------------------------
            Pos={GetPos AST}
         in

            case ArgsRest
            of X|Xs then
               F V
            in
               % The recursive calls haven't reached the end of the argument list.
               % Handle the head of the remaining list, and make a recursive call
               X = fColon(F V)
               if {IsElementary V} then
                 {UnnestFRecordInt FRecordAST X#unit#unit|NewArgsList Xs}
               else
                  case V
                  of fOpenRecord(_ _) then
                     {UnnestFRecordInt FRecordAST fColon(F {UnnestFRecordInt V nil V.2 })#unit#unit|NewArgsList Xs}

                  else
                     NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
                  in
                     {UnnestFRecordInt FRecordAST fColon(F NewSymbol)#NewSymbol#{UnnesterInt fEq(NewSymbol V Pos) {NoTail Params}}|NewArgsList Xs}
                  end
               end
            else
               Unifications
               Decls
               FinalArgs
               ResultRecord
               UniRes
               DeclsRes
            in
               % All unnested arguments are now found in NewArgsList
               % We can now work on the fRecord itself
               % otherwise no recursive call
               % all fLocal introduced by complex arguments have been directly place out of fRecord when traversing ArgsRest
               % and all what's left in the argument list are Symbols.

               Unifications={List.map {List.filter NewArgsList fun{$ _#_#Uni} Uni\=unit end}
                                      fun {$ _#_#Uni} Uni end}
               Decls = {List.map  {List.filter NewArgsList fun{$ _#Sym#_} Sym\=unit end}
                                  fun {$ _#Sym#_} Sym end}
               FinalArgs = {List.reverse {List.mapInd  NewArgsList fun {$ Ind Arg#_#_} Arg end} }

               ResultRecord = case FRecordAST
                        of fRecord(Op _) then
                           fRecord(Op FinalArgs)
                        [] fOpenRecord(Op _) then
                           fOpenRecord(Op FinalArgs)
                        end
               if {List.length Unifications}>0 then
                  if Params.tail then
                     %{Show 'debug is tail record'#Pos}
                     %{DumpAST.dumpAST AST _}
                     %{Show '---'}
                     UniRes=fAnd(fEq(FSym ResultRecord Pos) {UnnesterInt {WrapInFAnd Unifications} Params} )
                  else
                     %{Show 'debug is not tail record'#Pos}
                     %{DumpAST.dumpAST AST _}
                     %{Show '---'}
                     UniRes=fAnd({UnnesterInt {WrapInFAnd Unifications} {NoTail Params}} fEq(FSym ResultRecord Pos))
                  end
               else
                  UniRes=fEq(FSym ResultRecord Pos)
               end


               if {List.length Decls }>0 then
                  DeclsRes = fLocal({WrapInFAnd Decls} UniRes Pos)
               else
                  DeclsRes=UniRes
               end
               DeclsRes
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
         %{Show 'debug unnester for:'#Params.tail}
         %{DumpAST.dumpAST AST _}
         %{Show '----'}
         case AST
         of fEq(LHS RHS Pos) then
            % Call Unnester on both parts, so that if it is a record, it is unnested.
            % FIXME: This does not feel the cleanest, as it is not the expected flow.
            {UnnestFEq fEq({UnnesterInt LHS Params} {UnnesterInt RHS Params} Pos) Params}
            %{UnnestFEq AST Params}
         [] fLocal(Decls Body Pos) then
            fLocal(Decls {UnnesterInt Body Params} Pos)
         [] fApply(_ _ _) then
            {UnnestFApply AST Params}
         [] fBoolCase(_ _ _ _) then
            {UnnestFBoolCase AST Params}
         [] fCase(_ _ _ _) then
            {UnnestFCase AST Params}
         %[] fRecord(_ _) then
         %   % FIXME:  is this really the place to constantise the record.
         %   % This requires the unnest function to be called on both sides of fEq before it gets treated....
         %   {UnnestFRecord unit AST Params}
         [] fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) then
            fNamedSideCondition({UnnesterInt Pattern Params} Decls {UnnesterInt Guards Params} GuardSymbol Pos)
         [] fAnd(First Second) then
            fAnd({UnnesterInt First {NoTail Params}} {UnnesterInt Second Params})
         [] fProc(FSym Args Body Flags Pos) then
            fProc(FSym Args {UnnesterInt Body {InTail Params}} Flags Pos)
         [] fConst(_ _) then
            AST
         [] fSym(_ _) then
            AST
         [] pos(_ _ _ _ _ _) then
            AST

         else
            {DefaultPass AST UnnesterInt Params}
         end
      end
   in
      {Show '-------'}
      {Show 'Unnester'}
      {Show '-------'}
      {UnnesterInt AST params(tail:true)}
   end


end

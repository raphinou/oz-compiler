functor
import
   Boot_CompilerSupport at 'x-oz://boot/CompilerSupport'
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction makeArity ) at 'x-oz://system/CompilerSupport.ozf'
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
   CodeGen
define
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
               {List.append @R [call({PermRegForSym Sym Params} {List.length Args})] }
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
               % VM bug workaround
               if {Record.label {PermRegForSym LHS Params}}==k then
                  OpCodes = move({PermRegForSym LHS Params} x(0))|createRecordUnify(k(Arity) {List.length OrderedFeaturesList} x(0))|nil
               else
                  OpCodes = createRecordUnify(k(Arity) {List.length OrderedFeaturesList}  {PermRegForSym LHS Params})
               end
            else
               if Label=='|' andthen {List.map OrderedFeaturesList fun {$ L#_} L end}==[1 2] then
                  % in this case we need to create a cons
                  % VM bug workaround
                  if {Record.label {PermRegForSym LHS Params}}==k then
                     OpCodes = move({PermRegForSym LHS Params} x(0))|createConsUnify(k(Label) {List.length OrderedFeaturesList} x(0))|nil
                  else
                     OpCodes = createConsUnify({PermRegForSym LHS Params})
                  end
               else
                  % if makeArity returned false, it means we need to create a tuple, because the feature was all numeric
                  % VM bug workaround
                  if {Record.label {PermRegForSym LHS Params}}==k then
                     OpCodes = move({PermRegForSym LHS Params} x(0))|createTupleUnify(k(Label) {List.length OrderedFeaturesList} x(0))|nil
                  else
                     OpCodes = createTupleUnify(k(Label) {List.length OrderedFeaturesList}  {PermRegForSym LHS Params})
                  end
               end
            end
            % after create...Unify, we need to pass values through arrayFills, ordered according to the features.
            Fills={List.map OrderedFeaturesList fun{$ _#V} arrayFill({PermRegForSym V Params}) end }
            {List.append [OpCodes] Fills}
         %----------------
         [] fEq(LHS RHS _) then
         %----------------
            % Use permanent register to avoid strange errors, eg in test 378
            [unify({PermRegForSym LHS  Params} {PermRegForSym RHS  Params})]

         %--------------------------------------
         [] fBoolCase(FSym TrueCode FalseCode _) then
         %--------------------------------------
            ErrorLabel={GenLabel}
            ElseLabel={GenLabel}
            EndLabel={GenLabel}
         in
            move({PermRegForSym FSym Params} x(0))|
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
                     move({PermRegForSym TestedValue Params} x(0))|
                     patternMatch(x(0) k(PatternMatchRecord))|
                     branch(NextTestLabel)|
                     lbl(ThisLabel)|
                     % Make captures available to guards code
                     {UsedSymbolsToYReg UsedSymbols}|
                     {CodeGenInt Guards Params}|
                     move({PermRegForSym GuardSymbol Params} x(1))|
                     condBranch(x(1) NextTestLabel ErrorLabel)|
                     nil
                  else
                     % This is the prefix of a sequence. In this case, the label for
                     % each clause' code is set already in the loop, so we do not
                     % include it here.

                     % Place TestedValue in x(0) as guards code could have wiped it
                     move({PermRegForSym TestedValue Params} x(0))|
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

         [] fTry(Body fCatch([fCaseClause(E Case)] _) fNoFinally _) then
            TryLabel={GenLabel}
            EndLabel={GenLabel}
         in
            setupExceptionHandler(TryLabel)|
            %FIXME
            %Ugly: we move the exception in x(0) to its permanent register
            % and the Case opcodes will move it from its permanent register
            % back to x(0).
            % The case opcodes do that because guards could have overriden x(0).
            % There should be a better solution...
            move(x(0) {PermRegForSym E Params})|
            {CodeGenInt Case Params}|
            branch(EndLabel)|
            lbl(TryLabel)|
            {CodeGenInt Body Params}|
            popExceptionHandler|
            lbl(EndLabel)|
            nil
         [] fRecord(fConst(_ _) _) then
            {Show '---'}
            {DumpAST.dumpAST AST _}
            raise unhandledRecordType end
         else
            {Show '---'}
            {DumpAST.dumpAST AST _}
            raise unexpectedASTInCodeGenInt end
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
                     {Show yAssigned}
                     {DumpAST.dumpAST X _}
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

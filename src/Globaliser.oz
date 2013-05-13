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
   Globaliser
define
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


end

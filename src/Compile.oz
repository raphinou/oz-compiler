functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at '../lib/DumpAST.ozf'
   BootValue at 'x-oz://boot/Value'
   % for nested environments debugging
   OS

export
   namer: Namer
   genCode: GenCode
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
   class SyntheticSymbol from Symbol
      meth init(Pos)
         Symbol, init('' Pos)
         type:=synthetic
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

   fun {Desugar AST}
      fun {DesugarOp Op Args Pos}
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
            Op
         end
      end

      fun {DesugarRecordFeatures Feature Params}
         % Assign features if not specified, starting with index 1
         case Feature
         of fColon(F V) then
            fColon({DesugarExpr F Params} {DesugarExpr V Params})
         else
            % Feature.1 is position. Check this is always the case!
            (Params.featureIndex):=@(Params.featureIndex)+1
            fColon( fConst(@(Params.featureIndex) Feature.1) {DesugarExpr Feature Params})
         end
      end
      % Expression/statements:
      % https://github.com/mozart/mozart2-bootcompiler/blob/master/src/main/scala/org/mozartoz/bootcompiler/transform/Transformer.scala#L64
      fun {DesugarExpr AST Params}
         % Desugar expressions
         case AST
         of fProc(fDollar(DollarPos) Args Body Flags Pos) then
            DollarSymbol = fSym({New SyntheticSymbol init(DollarPos)} DollarPos)
         in
            % FIXME: is the recursive call on the top level really useful?
            % Wouldn't is be better to only do the recursive calls on args and body?
            {DesugarStat fLocal( DollarSymbol fAnd( fProc(DollarSymbol Args Body Flags Pos) DollarSymbol) Pos) Params}

         [] fFun(fDollar(DollarPos) Args Body Flags Pos) then
            DollarSymbol = fSym({New SyntheticSymbol init(DollarPos)} DollarPos)
         in
            % The fLocal we introduce is an expression, handle it as such!
            {DesugarExpr fLocal( DollarSymbol fAnd( fFun(DollarSymbol Args Body Flags Pos) DollarSymbol) Pos) Params}
         [] fLocal(Decls Body Pos) then
            % for fLocal, declarations are always statements.
            % if the fLocal is a statement, its body must be a statement and is handled as such
            % Do not recursively desugar declarations, as there are all fSym thanks for DeclsFlattener.
            fLocal(Decls {DesugarExpr Body Params} Pos)
         [] fAnd(First Second) then
            % if the fAnd is an expression, only the second part is treated as expression
            fAnd({DesugarStat First Params} {DesugarExpr Second Params})
         [] fAt(Cell Pos) then
            fApply( fConst(BootValue.catAccess Pos) [{DesugarExpr Cell Params}] Pos)

         [] fOpApply(Op Args Pos) then
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fApply(Op Args Pos) then
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fColonEquals(Cell Val Pos) then
            fApply( fConst(BootValue.catExchange Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fBoolCase( Cond TrueCode FalseCode Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarExpr TrueCode Params} {DesugarExpr FalseCode Params} Pos)

         [] fRecord( Label Features) then
            NewParams={Record.adjoin Params params( featureIndex:{NewCell 0})}
         in
            fRecord({DesugarExpr Label Params} {List.map Features fun {$ I} {DesugarRecordFeatures I NewParams} end })

         [] fColon(Feature Value) then
            fColon({DesugarExpr Feature Params} {DesugarExpr Value Params})

         [] fSym(_ _) then
            AST
         [] fConst(_ _) then
            AST
         end
      end

      fun {DesugarStat AST Params}
         % Desugar function. Direct translation from sugared to desugared.
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
            fApply({DesugarOp Op Args Pos} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fEq(LHS RHS Pos) then
            fEq({DesugarExpr LHS Params} {DesugarExpr RHS Params} Pos)

         [] fProc(FSym Args Body Flags Pos) then
            fProc({DesugarExpr FSym Params} {List.map Args fun {$ I} {DesugarExpr I Params} end } {DesugarStat Body Params} Flags Pos)
         [] fFun(FSym Args Body Flags Pos) then
            ReturnSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            % Need to Desugar the top-level fProc, eg in the case of a statement function (fun {$ ..}),
            % so that the $ also gets desugared
            {DesugarStat fProc(FSym {List.append Args [ReturnSymbol]} fEq(ReturnSymbol Body Pos) Flags Pos) Params}
         [] fColonEquals(Cell Val Pos) then
            fApply( fConst(BootValue.catAssign Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)
         [] fBoolCase( Cond TrueCode FalseCode Pos) then
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarStat TrueCode Params} {DesugarStat FalseCode Params} Pos)
         %else
         %   {DefaultPass AST DesugarInt Params}
         end
      end
   in
      {DesugarStat AST params}
   end

   fun {Unnester AST}
      fun {IsElementary AST}
         % Records are elementary if of the form fConst or fSym.
         % All other types are elementary
         if {Record.is AST} then
            case {Label AST}
            of fConst then
               true
            [] fSym then
               true
            % FIXME RECORD
            [] fRecord then
               true
            else
               false
            end
         else
            true
         end
      end

      fun {BindVarToExpr FSym AST Params}
         % Handles the binding of a variables to a complex expression
         case AST
         %of fApply( fConst(BootValue.catAssign ConstPos) Args Pos) then
         %   {UnnesterInt fApply( fConst(BootValue.catExchangeFun ConstPos) {List.append Args [FSym]} Pos) Params}
         of fApply(Proc Args Pos) then
            % enters the assignation target in the proc call
            % Res = {P Arg} -> {P Arg Res}
            {UnnesterInt fApply(Proc {List.append Args [FSym]} Pos) Params }
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
         else
            {DumpAST.dumpAST AST _}
            raise unexpectedASTForBindVarToExpr end
         end
      end

      fun {UnnestFApply AST=fApply(Op Args Pos) Params}
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
                  NewSymbol={New SyntheticSymbol init(Pos)}
               in
                  {UnnesterInt fLocal(fSym(NewSymbol Pos)
                                      fAnd( fEq(fSym(NewSymbol Pos) X Pos)
                                            {UnnestFApplyInt FApplyAST fSym(NewSymbol Pos)|NewArgsList Xs}) Pos)
                               Params}
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

      fun {UnnesterInt AST Params}
         %{Show 'UnnesterInt works on:'}
         %{DumpAST.dumpAST AST _}
         case AST
         of fEq(_ _ _) then
            {UnnestFEq AST Params}
         [] fLocal(Decls Body Pos) then
            fLocal(Decls {UnnesterInt Body Params} Pos)
         [] fApply(_ _ _) then
            {UnnestFApply AST Params}
         [] fBoolCase(_ _ _ _) then
            {UnnestFBoolCase AST Params}
         else
            {DefaultPass AST UnnesterInt Params}
         end
      end
   in
      {UnnesterInt AST params}
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
            %{Show 'AST received by NamerForDecls:'}
            %{DumpAST.dumpAST AST _}
            raise flattenerLeftOtherThingsThanFVarInDecls end
         end
      end



      fun {NamerForBody AST Params}
         %for fLocal, fFun, fProc, we backup the environment when we enter and
         %restore it when we get out.
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
         [] fFun(Name Args Body Flags Pos) then
         %---------------------------------
            Res
         in
            {Params.env backup()}
            Res=fFun(
               % The procedure's variable has to be declared explicitely
               {NamerForBody Name Params}
               {List.map Args fun {$ I} {NamerForDecls I Params} end }
               {NamerForBody Body Params}
               Flags
               Pos
            )
            {Params.env restore()}
            Res

         %---------------------------------
         [] fProc(Name Args Body Flags Pos) then
         %---------------------------------
            Res
         in
            {Params.env backup()}
            Res=fProc(
               % The procedure's variable has to be declared explicitely
               {NamerForBody Name Params}
               {List.map Args fun {$ I} {NamerForDecls I Params} end }
               {NamerForBody Body Params}
               Flags
               Pos
            )
            {Params.env restore()}
            Res


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
            {Show 'assign scope in fLocal'}
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
            OpCodes={NewCell nil}
            % Number of globals, ie variables comint from parent's environment
            GlobalsCount = {List.length NewLocals}
            ArrayFills
         in
            %
            {Show '##############'}
            {System.showInfo 'Procedure '#{Sym get(name $)} }
            {Show '##############'}
            {GenAndAssemble Body Args 'test' d(file:Pos.1 line:Pos.2 column:Pos.3) switches ?CA _} %last argument is ?VS, the virtual string is set by GenAndAssemble.


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
                                       if SymbolType==localProcId orelse SymbolType==synthetic then
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
            case Sym
            of fConst(Const _) then
               {List.append @R [call(k(Const) {List.length Args})] }
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
            unify({F LHS  Params} {F RHS  Params})

         [] fBoolCase(FSym TrueCode FalseCode _) then
            ErrorLabel={NewName}
            ElseLabel={NewName}
            EndLabel={NewName}
         in
            move({GenCodeInt FSym Params} x(0))|
            condBranch(x(0) ElseLabel ErrorLabel)|
            %---- true ----
            {GenCodeInt TrueCode Params}|
            branch(EndLabel)|
            %---- error ----
            lbl(ErrorLabel)|
            move(k(badBooleanInIf) x(0))|
            tailCall(k(Exception.raiseError) 1)|
            %---- else ----
            lbl(ElseLabel)|
            {GenCodeInt FalseCode Params}|
            % ---- end ----
            lbl(EndLabel)|nil
         [] fRecord(fConst(Label _) Features) then
            Rec
         in
            Rec={List.foldL Features fun{$ A I}
                                       case I
                                       of fColon(fConst(L _) fConst(F _)) then
                                          {Record.adjoin A Label(L:F)}
                                       %[] fColon(fSym(FSym Val _)  fConst(F _)) then
                                       %   {Record.adjoin A {GenCodeInt FSym Params}(L:F)}
                                       else
                                          A
                                       end
                                   end Label()}
            k(Rec)



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
      % append return
      OpCodes={List.append {List.flatten {GenCodeInt AST InitialParams}} ['return'()]}
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

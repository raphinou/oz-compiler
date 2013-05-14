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
      % Boot_Object provides:
      % attrExchangeFun attrGet attrPut cellOrAttrExchangeFun cellOrAttrGet cellOrAttrPut getClass is new
      Boot_Object at 'x-oz://boot/Object'
      Boot_Record at 'x-oz://boot/Record'
      Boot_Thread at 'x-oz://boot/Thread'
      Boot_Exception at 'x-oz://boot/Exception'
      Boot_Name at 'x-oz://boot/Name'
      Boot_Value at 'x-oz://boot/Value'
export
   Desugar
define
   %################
   fun {Desugar AST}
   %################
      % Simply transform syntactic sugar in its basic form.
      % Expressions and Statements are handled differently.
      % Eg an if as statement has both its branches handled as statement, but
      % when it is an expression, both branches are treated as expressions too.
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarOp Op Args Pos Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
         [] 'mod' then
            fConst(Int.Op Pos)
         [] ':=' then
            fConst(Value.catExchange Pos)
         [] '~' then
            fConst(Number.Op Pos)
         else
            {DesugarExpr Op Params}
         end
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {HandleDollarArg AST DollarSym}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Replace nesting marker argument by a new symbol
         % Used for methods and procs
         case AST
         of fDollar(_) then
            Pos = {GetPos AST}
         in
            if @DollarSym\=unit then
               raise multipleNestingMarkersInFunctionalMethodDefinition end
            end
            DollarSym:=fSym({New SyntheticSymbol init(Pos)} Pos)
            @DollarSym
         else
            AST
         end
      end
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {InjectDollarIfNeeded Body DollarSym}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Unify the symbol put in place of the nesting marker argument with the body.
         % Used for methods and procs
         if DollarSym==unit then
            Body
         else
            fEq(DollarSym Body {GetPos Body})
         end
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarRecordFeatures Feature Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Assign features if not specified, starting with index 1
         case Feature
         of fColon(F V) then
            fColon({DesugarExpr F Params} {DesugarExpr V Params})
         else
            Pos
         in
            % Feature.2 is most of the time the position, but not alway.
            % We check it
            if {Record.width Feature}>1 andthen {Record.is Feature.2} andthen {Label Feature.2}==pos then
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

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {IsConstantRecord fRecord(L Fs)}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Returns couple of bools indicating if label, features and values of record all are constants or not.
         % This will go through the list once, and set all components to correct boolean value.
         {List.foldL Fs fun {$ FB#VB I} F V in I=fColon(F V) (FB andthen {Label F}==fConst)#(VB andthen {Label V}==fConst) end ({Label L}==fConst)#true}
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {TransformRecord AST=fRecord(_ Features)}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Called to transform record just *after* it has been desugared:
         % - replace constant record by a constant (fConst)
         % - replace records with non const label or feature by a call to Boot_Record.makeDynamic

         %-----------------------------
         fun {Features2Record Features}
         %-----------------------------
            % Builds a record with label #, features are increasing integer
            % values, and the values are alternatively the feature and its
            % corresponding value of the record definition found in the AST.
            % rec(A:1 B:2 C:3)
            % will be transformed in (in AST form):
            % #(1:A 2:1 3:B 4:2 5:C 6:3)
            % This is done because we need records with constant features in CodeGen.
            %.................................
            fun{Features2RecordInt Features I}
            %.................................
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
         %----------------
            % All parts of the record are constants. Build the record and put
            % it in the AST as a constant (under a fConst)
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
            Rec
         in
            Rec={ConstantiseRecord AST}
            fConst(Rec pos)
         [] true#false then
         %-----------------
            % will use makeArity in CodeGen, leave as is
            AST
         [] false#_ then
         %--------------
            %Need to change it in a call to makeDynamic, so that the arity is constant for later phases, including CodeGen
            L Features
            Pos={GetPos AST}
         in
            fRecord(L Features)=AST
            fApply( fConst(Boot_Record.makeDynamic Pos) [L {Features2Record Features}] Pos)
         else
         %---
            AST
         end
      end
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {HandleLazyFlag ReturnSymbol Body Flags Pos}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarClass fClass(FSym AttributesAndProperties Methods Pos) Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %---------------------------------------------------------
         fun {TransformMethod Ind fMeth(Signature Body Pos) Params}
         %---------------------------------------------------------
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
            % Note that it is possible to define functional methods by putting a nesting marker
            % in the arguments list in the definition of the method
            % These methods' bodies are expressions.
            % This is handled by functions
            % - HandleDollarArg which replaces the dollard arg by a symbol
            % - InjectDollarIfNeeded which add unification of the dollar symbol with the method body if there was
            %   indeed a nesting marker in the arguments list



            %..........................
            fun {NameAndArgs Signature}
            %..........................
               % Extract Identifier and Arguments of the method from its signature
               case Signature
               of fRecord(InFName InArgs) then
                  InFName#InArgs
               [] fOpenRecord(InFName InArgs) then
                  % FIXME: in this case, we should inject code to check that the features we get in the message
                  % cover the required features found in the definition
                  InFName#InArgs
               [] fEq(M _ _) then
                  {NameAndArgs M}
               else
                  Signature#nil
               end
            end

            %..............................
            fun {GetHeaderSymbol Signature}
            %..............................
               % Returns Symbol which will hold a reference to the method head
               case Signature
               of fEq(_ V _) then
                  V
               else
                  unit
               end
            end

            NewBody
            Decls
            R
            SelfSymbol=fSym({New Symbol init('self' Pos)} Pos)
            MessageSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
            HeaderSymbol
            DollarSym={NewCell unit}
            FName
            Args
            DeclsWithMethHeader
         in
            FName#Args={NameAndArgs Signature}
            HeaderSymbol={GetHeaderSymbol Signature}

            Decls = {List.mapInd Args  fun {$ Ind I}
                      % If a default value is provided, inject code in the AST to check if a value was provided,
                      % ie if the feature is present in the message.
                      % If it is a fMethArg, the feature is numeric and found in Ind
                      % It is is a fMethColonArg, the feature is found in its first value.
                      % Except for the distinction in the feature, both cases have the same code.
                                             NewSym
                                             Pos
                                          in
                                             case I
                                             of fMethArg(Sym fNoDefault) then
                                                Pos={GetPos Sym}
                                                NewSym={HandleDollarArg Sym DollarSym}
                                                NewSym#fEq(NewSym fOpApply('.' [MessageSymbol fConst(Ind Pos)] Pos) Pos)
                                             [] fMethArg(Sym fDefault(Default _)) then
                                                Pos={GetPos Sym}
                                                NewSym={HandleDollarArg Sym DollarSym}
                                                NewSym#fEq(NewSym fBoolCase(fApply(fConst(Value.hasFeature Pos) [MessageSymbol fConst(Ind Pos)] Pos) fOpApply('.' [MessageSymbol fConst(Ind Pos) ] Pos) Default Pos) Pos)
                                             [] fMethColonArg(F Sym fNoDefault) then
                                                NewSym={HandleDollarArg Sym DollarSym}
                                                Pos={GetPos Sym}
                                                NewSym#fEq(NewSym fOpApply('.' [MessageSymbol F ] Pos) Pos)
                                             [] fMethColonArg(F Sym fDefault(Default _)) then
                                                Pos={GetPos Sym}
                                                NewSym={HandleDollarArg Sym DollarSym}
                                                NewSym#fEq(NewSym fBoolCase(fApply(fConst(Value.hasFeature Pos) [MessageSymbol F] Pos) fOpApply('.' [MessageSymbol F ] Pos) Default Pos) Pos)

                                             end
                                          end}
            % Add method head capture if present
            if HeaderSymbol\=unit then
               DeclsWithMethHeader={List.append [ HeaderSymbol#fEq(HeaderSymbol MessageSymbol {GetPos HeaderSymbol})] Decls }
            else
               DeclsWithMethHeader=Decls
            end

            % Declare needed symbols, if any
            if {List.length DeclsWithMethHeader}>0 then
               NewBody=fLocal( {WrapInFAnd {List.map DeclsWithMethHeader fun{$ Sym#_} Sym end}} {WrapInFAnd {List.append  [{InjectDollarIfNeeded Body @DollarSym}] {List.map DeclsWithMethHeader fun{$ _#Init} Init end }}} {GetPos Body})
            else
               NewBody={InjectDollarIfNeeded Body @DollarSym}
            end

            % Desugar the body with the self symbol set.
            % This will transform @bla in catAccessOO
            (Params.'self'):=SelfSymbol
            R=fColon(fConst(Ind Pos) fRecord(fConst('#' Pos) [FName fProc(fDollar(Pos) [SelfSymbol MessageSymbol] {DesugarStat NewBody Params} nil Pos)] ))
            (Params.'self'):=unit
            R

         end

         %----------------------------------
         fun {TransformAttribute Rec Params}
         %----------------------------------
            % Builds the feature-value pair for the attr and feat record
            case Rec
            of '#'(F V) then
               fColon(F V)
            else
               fColon(Rec fConst({Boot_Name.newUnique 'ooFreeFlag'} {GetPos Rec}))
            end
         end


         Parents NewParents NewMeths NewAttrs NewFeats Props NewProps PrintName
         ClassSym
      in
         NewMeths=fRecord(fConst('#' Pos) {List.mapInd Methods fun {$ Ind I} {TransformMethod Ind I Params} end } )

         % Collect attributes, features, and attributes
         {List.forAll AttributesAndProperties  proc {$ I}
                                                  case I
                                                  of fAttr(L _) then
                                                     NewAttrs=fRecord(fConst('attr' Pos) {List.map L fun {$ Attr} {TransformAttribute Attr Params} end })
                                                  [] fFeat(L _) then
                                                     NewFeats=fRecord(fConst('feat' Pos) {List.map L fun {$ Attr} {TransformAttribute Attr Params} end })
                                                  [] fFrom(L _) then
                                                     Parents=L
                                                  [] fProp(L _) then
                                                     Props=L
                                                  end
                                               end}
         % Set default values if no attribute, feature, parent was collected in previous step
         if {Not {IsDet NewAttrs}} then
            NewAttrs=fConst('attr'() Pos)
         end
         if {Not {IsDet NewFeats}} then
            NewFeats=fConst('feat'() Pos)
         end
         if {Not {IsDet Parents}} then
               NewParents=fConst(nil Pos)
            else
               NewParents={ListToAST {List.map Parents fun{$ I}{DesugarExpr I Params} end}}
         end
         if {Not {IsDet Props}} then
               NewProps=fConst(nil Pos)
            else
               NewProps={ListToAST {List.map Props fun{$ I}{DesugarExpr I Params} end}}
         end

         % Set PrintName
         case FSym
         of fDollar(_) then
            PrintName=fConst('' Pos)
         else
            fSym(ClassSym _)=FSym
            PrintName=fConst({ClassSym get(name $)} Pos)
         end
         {DesugarStat fApply( fConst(OoExtensions.'class' Pos) [ NewParents NewMeths NewAttrs NewFeats NewProps PrintName FSym] Pos) Params}
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarCaseClause Clause DesugarInstr Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         case Clause
         of fCaseClause(fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) Body) then
            fCaseClause(fNamedSideCondition({DesugarExpr Pattern Params} Decls {DesugarStat Guards Params} GuardSymbol  Pos) {DesugarInstr Body Params} )
         [] fCaseClause(Pattern Body) then
            fCaseClause({DesugarExpr Pattern Params} {DesugarInstr Body Params})
         end
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarFunctor AST=fFunctor(Id SpecsList Pos)  Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         %-----------------------------------------------------------------------------
         fun {DesugarFunctorWithRequire fFunctor(Id SpecsList Pos) FunctorSpecs Params}
         %-----------------------------------------------------------------------------
            % If there is a require, we desugar the functor in 2 nested functors
            % Previous compiler code: https://github.com/mozart/mozart2-compiler/blob/master/Unnester.oz#L840
            %A functor of this form
            %   functor
            %   require <req>
            %   prepare <prep>
            %   import <imp>
            %   export <exp>
            %   define <def>
            %   end
            %is transformed in:
            %
            %   local
            %      functor Outer
            %      import <req>
            %      export inner:Inner
            %      define
            %         functor Inner
            %         import <imp>
            %         export <exp>
            %         define
            %            <defDecls><prepareDecls>
            %         in
            %            <defStats>
            %            <prepareStats>
            %         end
            %      end
            %   in
            %      {ApplyFunctor BaseURL Outer}.inner
            %   end
            %ApplyFunctor is a constant function defined in the compiler, and as it is
            %constant, it can be placed in the AST under a fConst!
            % BaseUrl is the path to the currently being compiled file



            %............................
            fun {ApplyFunctor FileName F}
            %............................
               ModMan = {New Module.manager init()}
            in
               {ModMan apply(url: FileName F $)}
            end

            OuterFunctor = fSym({New SyntheticSymbol init(Pos)} Pos)
            InnerFunctor = fSym({New SyntheticSymbol init(Pos)} Pos)
            FilePath=fConst({GetPos AST}.1 Pos)
            RequirePos={GetPos FunctorSpecs.'require'}
            ExportPos={GetPos FunctorSpecs.'export'}

         in
            fLocal(  OuterFunctor
                   fAnd( fFunctor( OuterFunctor
                                   [ fImport(FunctorSpecs.requireItems RequirePos)
                                     fExport([fExportItem(fColon(fConst('inner' Pos) InnerFunctor))] Pos)
                                     fDefine(fAnd(InnerFunctor FunctorSpecs.'prepareDecls')
                                             fAnd( fFunctor(InnerFunctor [FunctorSpecs.'import' FunctorSpecs.'export' FunctorSpecs.'define'] Pos)
                                                   FunctorSpecs.'prepareStats')
                                             Pos)
                                   ]
                                   Pos )
                         fApply( fConst(Value.'.' Pos)
                                 [ fApply(fConst(ApplyFunctor Pos) [ FilePath OuterFunctor] Pos) fConst('inner' Pos) Id] Pos)) Pos)
         end

         %................................................................................
         fun {DesugarFunctorWithoutRequire fFunctor(Id SpecsList Pos) FunctorSpecs Params}
         %................................................................................
            % Desugar it as a call to Feature.new with 3 arguments:
            % - ImportRecord
            % - ExportRecord
            % - ApplyFunction
            %
            % ImportRecord is of the form import( ModName(info(type:AliasesAtomsList from:"x-oz://system/"#ModName#".ozf"))) where ModName
            % For this import:
            %    import
            %       DumpAST(dumpAST PPAST) at '../lib/DumpAST.ozf'
            % the resulting record is import('DumpAST'(info(type:[dumpAST] from:'../lib/DumpAST.ozf')))
            %
            % ExportRecord is simply of the form export with features the atoms exported, and as corresponding value the atom 'value'.
            % For this export:
            %    export
            %       dumpAST:DumpAST
            % the resulting record is export(dumpAST:value)
            %
            % The ApplyFun takes one parameter and looks like:
            % fun {$ ImportParamSym}
            %    local
            %       <importedSymbolsDeclarations>
            %       <defineDeclarations>
            %       <prepareDeclarations>
            %    in
            %       <importBinds>
            %       <defineStats>
            %       <prepareStats>
            %       'export'( dumpAST:DumpAST ...)
            %    end
            %
            % end
            % <importBinds> are simply unification statement binding an imported symbol with its value.
            % The value is obtained with ImportParamSym.ImportedSymbolName

            % Collect all declarations and statements in list held in a cell.
            Decls={NewCell nil}
            StatsList={NewCell nil}
            % build symbold for the ApplyFunction's argument
            ImportParamSym = fSym({New SyntheticSymbol init(Pos)} Pos)
            TypeRec
            FromRec
            Info
            ImportRecordFields
            ImportRecord
            ExportRecordFields
            ExportRecord
            ImportBinds
            ApplyFun
            ApplyFunBody
         in
            % If there is no require, we simply desugar the functor as a call to NewFunctor
            % Build import record
            ImportRecordFields={List.map FunctorSpecs.'importItems'   fun {$ fImportItem(fSym(Sym _) Aliases At)}
                                                      TypeField=fColon(fConst('type' Pos) {ListToAST {List.map Aliases fun {$ '#'(A F)} F end}})
                                                      Location
                                                      LocationField
                                                      ModName={Sym get(name $)}
                                                   in
                                                      Location=case At
                                                               of fNoImportAt then
                                                                  {VirtualString.toString "x-oz://system/"#ModName#".ozf"}
                                                               [] fImportAt(fConst(Loc _)) then
                                                                  Loc
                                                               end
                                                      LocationField=fColon(fConst('from' Pos) fConst(Location Pos))
                                                      fColon(fConst(ModName Pos) fRecord(fConst('info' Pos) [TypeField LocationField]))

                                                   end }
            ImportRecord=fRecord(fConst('import' Pos) ImportRecordFields)

            % Build the export record
            ExportRecordFields={List.map FunctorSpecs.'exportItems'   fun {$ fExportItem(fColon(F V))}
                                                fColon(F fConst('value' Pos))
                                             end}
            ExportRecord=fRecord(fConst('export' Pos) ExportRecordFields)


            % Collect declarations
            {List.forAll FunctorSpecs.'importItems' proc {$ fImportItem(Id Aliases At)}
                                    Decls:=Id|@Decls
                                    {List.forAll Aliases proc{$ '#'(A F)} Decls:=A|@Decls end}
                                 end}
            if FunctorSpecs.defineDecls\=nil then
               Decls:=FunctorSpecs.defineDecls|@Decls
            end

            if FunctorSpecs.prepareDecls\=nil then
               Decls:=FunctorSpecs.prepareDecls|@Decls
            end


            % bind import variables and aliases
            ImportBinds = {NewCell nil}
            {List.forAll FunctorSpecs.'importItems' proc {$ fImportItem(I=fSym(Sym _) Aliases _)}
                                    ImportBinds:=fEq(I fApply(fConst(Value.'.' Pos) [ImportParamSym fConst({Sym get(name $)} Pos)] Pos) Pos)|@ImportBinds
                                    {List.forAll Aliases proc {$ '#'(A F)}
                                                         ImportBinds:=fEq(A fApply(fConst(Value.byNeedDot Pos) [I F] Pos) Pos)|@ImportBinds
                                                      end}
                                 end}
            StatsList:= fRecord(fConst('export' Pos) {List.map FunctorSpecs.'exportItems' fun{$ fExportItem(I)} I end})|
                        nil
            if FunctorSpecs.prepareStats\=nil then
               StatsList := FunctorSpecs.prepareStats| @StatsList
            end
            if FunctorSpecs.defineStats\=nil then
               StatsList := FunctorSpecs.defineStats|@StatsList
            end

            if @ImportBinds\=nil then
               StatsList :=  @ImportBinds|@StatsList
            end

            % FIXME: could be improved to remove double reverse
            % We put the StatsList in the order we want them to be executed. WrapInFAnd reverse the order, so we reverse it ourself first
            ApplyFunBody = fLocal( {WrapInFAnd @Decls} {WrapInFAnd {List.reverse {List.flatten @StatsList}} } Pos)

            ApplyFun = fFun(fDollar(Pos) [ImportParamSym] ApplyFunBody nil Pos)

            case Id
            of fDollar(_) then
               fApply(fConst(Functor.new Pos) [ImportRecord ExportRecord ApplyFun]  Pos)
            else
               fApply(fConst(Functor.new Pos) [ImportRecord ExportRecord ApplyFun Id]  Pos)
            end
         end

         FunctorSpecs={ExtractFunctorSpecs SpecsList}

      in
         if FunctorSpecs.'require'==nil then
            {DesugarFunctorWithoutRequire AST FunctorSpecs Params}
         else
            {DesugarFunctorWithRequire AST FunctorSpecs Params}
         end
      end

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarExpr AST Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Desugar expressions
         %{Show 'debug DesugarExpr'}
         %{DumpAST.dumpAST AST _}
         case AST
         of fProc(Dollar Args Body Flags Pos) then
         %----------------------------------------
            fProc(Dollar Args {DesugarStat Body Params} Flags Pos)

         [] fFun(Dollar Args Body Flags Pos) then
         %---------------------------------------
            ReturnSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fProc(Dollar {List.append Args [ReturnSymbol]} {DesugarStat {HandleLazyFlag ReturnSymbol Body Flags Pos} Params} Flags Pos)

         [] fAndThen(First Second Pos) then
         %---------------------------------
            fBoolCase({DesugarExpr First Params} {DesugarExpr Second Params} fConst(false Pos) Pos)

         [] fOrElse(First Second Pos) then
         %--------------------------------
            fBoolCase({DesugarExpr First Params} fConst(true Pos) {DesugarExpr Second Params} Pos)

         [] fLocal(Decls Body Pos) then
         %-----------------------------
            % for fLocal, declarations are always statements.
            % if the fLocal is a statement, its body must be a statement and is handled as such
            % Do not recursively desugar declarations, as they are all fSym thanks for DeclsFlattener.
            fLocal(Decls {DesugarExpr Body Params} Pos)
         [] fAnd(First Second) then
         %-------------------------
            % if the fAnd is an expression, only the second part is treated as expression
            fAnd({DesugarStat First Params} {DesugarExpr Second Params})

         [] fAt(Cell Pos) andthen @(Params.'self')==unit then
         %---------------------------------------------------
            % Not in a class because Params.self is unit
            fApply( fConst(Boot_Value.catAccess Pos) [{DesugarExpr Cell Params}] Pos)

         [] fAt(Cell Pos) then
         %--------------------
            % When in a class, Params.self has been set to Self. See TransformMethod
            fApply( fConst(Boot_Value.catAccessOO Pos) [@(Params.'self') {DesugarExpr Cell Params}] Pos)

         [] fOpApply(Op Args Pos) then
         %----------------------------
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos Params} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fApply(Op Args Pos) then
         %--------------------------
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos Params} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fObjApply(LHS RHS Pos) then
         %-----------------------------
            {DesugarExpr
               fApply(
                  fApply(fConst(Value.'.' Pos)
                     [ fApply(fConst(Value.'.' Pos) [LHS fConst({Boot_Name.newUnique 'ooFallback'} Pos)] Pos)
                       fConst(apply Pos) ] Pos)
                  [RHS @(Params.'self') LHS] Pos)
               Params}

         [] fColonEquals(Cell Val Pos) andthen @(Params.'self')==unit then
         %-----------------------------------------------------------------
            fApply( fConst(Boot_Value.catExchange Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fColonEquals(Cell Val Pos) then
         %---------------------------------
            fApply( fConst(Boot_Value.catExchangeOO Pos) [ @(Params.'self') {DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fBoolCase( Cond TrueCode Else=fNoElse(_) Pos) then
         %----------------------------------------------------
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarExpr TrueCode Params} Else Pos)

         [] fBoolCase( Cond TrueCode FalseCode Pos) then
         %----------------------------------------------
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarExpr TrueCode Params} {DesugarExpr FalseCode Params} Pos)

         [] fRecord( Label Features) then
         %-------------------------------
            NewParams={Record.adjoin Params params( featureIndex:{NewCell 0})}
         in
            {TransformRecord fRecord({DesugarExpr Label Params} {List.map Features fun {$ I} {DesugarRecordFeatures I NewParams} end }) }

         [] fPatMatConjunction(LHS RHS Pos) then
         %--------------------------------------
            fConst({StoreInSafe fPatMatConjunction({DesugarExpr LHS Pos} {DesugarExpr RHS Pos} Pos)} Pos)

         [] fOpenRecord(Label Features) then
         %----------------------------------
            NewParams={Record.adjoin Params params( featureIndex:{NewCell 0})}
         in
            fOpenRecord({DesugarExpr Label Params} {List.map Features fun {$ I} {DesugarRecordFeatures I NewParams} end })

         [] fNamedSideCondition(Pattern Decls Guards GuardSymbol Pos) then
         %-----------------------------------------------------------------
            fNamedSideCondition({DesugarExpr Pattern Params} Decls {List.map Guards fun {$ I} {DesugarStat I Params} end} GuardSymbol Pos)

         [] fColon(Feature Value) then
         %----------------------------
            fColon({DesugarExpr Feature Params} {DesugarExpr Value Params})

         [] fThread(Body Pos) then
         %------------------------
            % Create synthetic symbol that will take the value of the thread expression,
            % and then desugar the fThread statement left.
            % It is DesugarStat that will replace the fThread by a call to the builtin Thread.create
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewSymbol fAnd( {DesugarStat fThread(fEq(NewSymbol Body Pos) Pos) Params} NewSymbol) Pos)

         [] fWildcard(Pos) then
         %---------------------
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal( NewSymbol NewSymbol Pos)

         [] fCase(Val Clauses Else=fNoElse(_) Pos=pos(File Line _ _ _ _)) then
         %--------------------------------------------------------------------
            % As usual: declare a new symbol, unify it with each clause's body, and put it as last expression
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            %fLocal(NewSymbol fAnd({DesugarStat fCase({DesugarExpr Val Params} {List.map Clauses fun {$ fCaseClause(Pattern Body)} fCaseClause( Pattern fEq(NewSymbol Body Pos) ) end} Else Pos) Params} NewSymbol) Pos)
            fCase({DesugarExpr Val Params} {List.map Clauses fun{$ I} {DesugarCaseClause I DesugarExpr Params} end} fApply(fConst(Boot_Exception.'raiseError' Pos) [{DesugarExpr fRecord(fConst(kernel pos) [fConst(noElse pos) fConst(File pos) fConst(Line pos) Val]) Params} ] Pos) Pos)

         [] fCase(Val Clauses Else Pos) then
         %----------------------------------
            % As usual: declare a new symbol, unify it with each clause's body, and put it as last expression
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fCase({DesugarExpr Val Params} {List.map Clauses fun{$ I} {DesugarCaseClause I DesugarExpr Params} end} {DesugarExpr Else Params} Pos)

         [] fClass(_ _ _ _) then
         %----------------------
            % fClass(FSym AttributesAndProperties Methods Pos)
            {DesugarClass AST Params}

         [] fTry(Body fCatch(Clauses CatchPos) fNoFinally Pos) then
         %---------------------------------------------------------
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(  NewSymbol
                     % add a else banch to the case, which will re-raise the exception when it was not catched by a pattern
                     fTry({DesugarExpr Body Params} fCatch([fCaseClause(NewSymbol {DesugarExpr fCase(NewSymbol Clauses fRaise(NewSymbol Pos) CatchPos) Params} )] CatchPos) fNoFinally Pos)
                     Pos)
         [] fTry(Body fNoCatch Finally Pos) then
         %--------------------------------------
            % From http://www.mozart-oz.org/documentation/notation/node6.html#label27
            % put the try-finally expression, without its Finally code, in a new try expression
            % with no finally code catching all exceptions.
            % After executing this new try expression, execute the Finally
            % then check if the finally was successful by looking at the value of the try expression.
            % The new try expression will have a record with label ok and value of the try-finally as
            % first value if the execution of the original try was successful.
            % If success, return value of the try-finally expression. Else, re-raise the exception.
            Temp1=fSym({New SyntheticSymbol init(Pos)} Pos)
            Temp2=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            {DesugarExpr
                        fLocal(Temp1
                           fAnd(
                              fLocal(Temp2
                                     fEq(Temp1
                                         fTry(fRecord(fConst(ok Pos) [fColon(fConst(1 Pos) Body)])
                                              fCatch([fCaseClause(Temp2 fRecord(fConst(ex Pos) [fColon(fConst(1 Pos) Temp2)]))] Pos)
                                              fNoFinally
                                              Pos)
                                         Pos)
                                     Pos)
                              fAnd(Finally
                                   fBoolCase( fOpApply('==' [fApply(fConst(Record.label Pos) [Temp1] Pos)  fConst(ok Pos)] Pos )
                                              fOpApply('.' [Temp1 fConst(1 Pos)] Pos)
                                              fRaise(fOpApply('.' [Temp1 fConst(1 Pos)] Pos) Pos)
                                              Pos)))
                           Pos)
                        Params}
         [] fTry(Body fCatch(Clauses CatchPos) Finally Pos) then
         %------------------------------------------------------
            % From http://www.mozart-oz.org/documentation/notation/node6.html#label27
            % put the try-finally expression, without its Finally code, in a new try expression
            % with no finally code catching all exceptions.
            % After executing this new try expression, execute the Finally
            % then check if the finally was successful by looking at the value of the try expression.
            % The new try expression will have a record with label ok and value of the try-finally as
            % first value if the execution of the original try was successful.
            % If success, return value of the try-finally expression. Else, re-raise the exception.
            Temp1=fSym({New SyntheticSymbol init(CatchPos)} CatchPos)
            Temp2=fSym({New SyntheticSymbol init(CatchPos)} CatchPos)
         in
            {DesugarExpr
                        fLocal(Temp1
                           fAnd(
                              fLocal(Temp2
                                     fEq(Temp1
                                         fTry(fRecord(fConst(ok Pos) [fColon(fConst(1 Pos) fTry(Body fCatch(Clauses CatchPos) fNoFinally Pos))])
                                              fCatch([fCaseClause(Temp2 fRecord(fConst(ex Pos) [fColon(fConst(1 CatchPos) Temp2)]))] CatchPos)
                                              fNoFinally
                                              Pos)
                                         Pos)
                                     Pos)
                              fAnd(Finally
                                   fBoolCase( fOpApply('==' [fApply(fConst(Record.label Pos) [Temp1] Pos)  fConst(ok Pos)] Pos )
                                              fOpApply('.' [Temp1 fConst(1 Pos)] Pos)
                                              fRaise(fOpApply('.' [Temp1 fConst(1 Pos)] Pos) Pos)
                                              Pos)))
                           Pos)
                        Params}

         [] fSelf(_) andthen @(Params.'self')==unit then
         %----------------------------------------------
            raise selfUsedOutsideMethod end

         [] fSelf(_)  then
         %----------------
            @(Params.'self')

         [] fRaise(E Pos) then
         %--------------------
            fApply(fConst(Exception.'raise' Pos) [ {DesugarExpr E Params} ] Pos)

         [] fDotAssign(fOpApply('.' [LHS CHS] Pos1) RHS Pos2) then
         %--------------------------------------------------------
            NewSymbol=fSym({New SyntheticSymbol init(Pos1)} Pos1)
         in
            {DesugarExpr fApply(fConst(Boot_Value.'dotExchange' Pos1) [LHS CHS RHS] Pos2) Params}

         [] fSym(_ _) then
         %----------------
            AST

         [] fConst(_ _) then
         %------------------
            AST

         [] fDollar(_) then
         %-----------------
            AST

         [] fAtom(_ _) then
         %-----------------
            raise namerLeftFAtomIntactAtDesugar end

         [] fEq(LHS _ _) then
         %-------------------
            fAnd(AST LHS)

         [] fFunctor(Id SpecsList Pos) then
         %---------------------------------
            {DesugarExpr {DesugarFunctor AST Params} Params}
         else
            {Show '---'}
            {DumpAST.dumpAST AST _}
            raise unhandledRecordInDesugarExpr end
         end
      end


      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      fun {DesugarStat AST Params}
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         % Desugar Statements.
         %{Show 'debug DesugarStat'}
         %{DumpAST.dumpAST AST _}
         case AST
         of fAnd(First Second) then
            % if the fAnd is a statement, both parts are treated as statements
            fAnd({DesugarStat First Params} {DesugarStat Second Params})


         [] fLocal(Decls Body Pos) then
         %-----------------------------
            % for fLocal, declarations are always statements.
            % if the fLocal is a statement, its body must be a statement and is handled as such
            % Do not recursively desugar declarations, as there are all fSym thanks for DeclsFlattener.
            fLocal(Decls {DesugarStat Body Params} Pos)

         [] fApply(Op Args Pos) then
         %-----------------------------
            % both Op and Args must be expression and expressions list respectively
            fApply({DesugarOp Op Args Pos Params} {List.map Args fun {$ I} {DesugarExpr I Params} end } Pos)

         [] fEq(LHS RHS Pos) then
         %-----------------------------
            fEq({DesugarExpr LHS Params} {DesugarExpr RHS Params} Pos)

         [] fProc(FSym Args Body Flags Pos) then
         %--------------------------------------
            % Only special thing is to handle the possible nesting marker in the arguments list.
            % In that case the nesting marker is replaced by a new symbol, and the body is unified with that symbol.
            % We use functions HandleDollarArg and InjectDollarIfNeeded, which are also used for functional methods
            DollarSym={NewCell unit}
            NewArgs
         in
            NewArgs={List.map Args fun{$ I} {HandleDollarArg I DollarSym} end}
            fProc({DesugarExpr FSym Params} NewArgs {DesugarStat {InjectDollarIfNeeded Body @DollarSym}  Params} Flags Pos)

         [] fFun(FSym Args Body Flags Pos) then
         %--------------------------------------
            ReturnSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            % Need to Desugar the top-level fProc, eg in the case of a statement function (fun {$ ..}),
            % so that the $ also gets desugared
            {DesugarStat fProc(FSym {List.append Args [ReturnSymbol]} {HandleLazyFlag ReturnSymbol Body Flags Pos} Flags Pos) Params}

         [] fColonEquals(Cell Val Pos) andthen @(Params.'self')==unit then
         %--------------------------------------
            fApply( fConst(Boot_Value.catAssign Pos) [{DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fColonEquals(Cell Val Pos) then
         %--------------------------------------
            fApply( fConst(Boot_Value.catAssignOO Pos) [@(Params.'self') {DesugarExpr Cell Params} {DesugarExpr Val Params}] Pos)

         [] fBoolCase( Cond TrueCode Else=fNoElse(_) Pos) then
         %--------------------------------------
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarStat TrueCode Params} Else Pos)

         [] fBoolCase( Cond TrueCode FalseCode Pos) then
         %--------------------------------------
            % Cond is a value, hence an expression.
            % Both branches are statements because the if itself is a statement
            fBoolCase( {DesugarExpr Cond Params} {DesugarStat TrueCode Params} {DesugarStat FalseCode Params} Pos)

         [] fThread(Body Pos) then
         %--------------------------------------
            % Create a wrapping proc taking no argument, and pass it to the builtin Thread.create.
            NewProcSym=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewProcSym fAnd(fProc(NewProcSym nil {DesugarStat Body Params} nil Pos) fApply(fConst(Boot_Thread.create Pos) [NewProcSym] Pos)) Pos)

         [] fLock(Body Pos) andthen @(Params.'self')\=unit then
         %--------------------------------------
            {DesugarStat fLockThen(fApply(fConst(OoExtensions.'getObjLock' Pos) [@(Params.'self')] Pos) Body Pos) Params}

         [] fLock(_ _) andthen @(Params.'self')==unit then
         %--------------------------------------
            raise lockNeedsSelfInDesugarStat end

         [] fLockThen(Lock Body Pos) then
         %--------------------------------------
            % Create a wrapping proc taking no argument, and pass it to the builtin Base.lockIn
            NewProcSym=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewProcSym fAnd(fProc(NewProcSym nil {DesugarStat Body Params} nil Pos) fApply(fConst(LockIn Pos) [Lock NewProcSym] Pos)) Pos)

         [] fCase(Val Clauses fNoElse(_) Pos=pos(File Line _ _ _ _)) then
         %--------------------------------------
            fCase({DesugarExpr Val Params} {List.map Clauses fun{$ I} {DesugarCaseClause I DesugarStat Params} end} fApply(fConst(Boot_Exception.'raiseError' Pos) [{DesugarExpr fRecord(fConst(kernel pos) [fConst(noElse pos) fConst(File pos) fConst(Line pos) Val]) Params} ] Pos) Pos)

         [] fCase(Val Clauses Else Pos) then
         %--------------------------------------
            fCase({DesugarExpr Val Params} {List.map Clauses fun {$ I} {DesugarCaseClause I DesugarStat Params} end} {DesugarStat Else Params} Pos)

         [] fClass(_ _ _ _) then
         %--------------------------------------
            % fClass(FSym AttributesAndProperties Methods Pos)
            {DesugarClass AST Params}

         [] fAssign(_ _ _) andthen @(Params.'self')==unit then
         %--------------------------------------
            raise assignAttributeNeedsSelf end

         [] fAssign(LHS RHS Pos) then
         %--------------------------------------
            fApply( fConst(Boot_Object.attrPut Pos) [@(Params.'self') {DesugarExpr LHS Params} {DesugarExpr RHS Params}] Pos)

         [] fObjApply(_ _ _) andthen @(Params.'self')==unit then
         %--------------------------------------
               raise staticCallNeedsSelf end

         [] fObjApply(LHS RHS Pos) then
         %--------------------------------------
            {DesugarStat
               fApply(
                  fApply(fConst(Value.'.' Pos)
                     [ fApply(fConst(Value.'.' Pos) [LHS fConst({Boot_Name.newUnique 'ooFallback'} Pos)] Pos)
                       fConst(apply Pos) ] Pos)
                  [RHS @(Params.'self') LHS] Pos)
               Params}

         [] fTry(Body fCatch(Clauses CatchPos) fNoFinally Pos) then
         %--------------------------------------
            %FIXME: handle multiple caseclauses
            NewSymbol=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            fLocal(NewSymbol
                   fTry({DesugarStat Body Params}
                        fCatch( [fCaseClause(NewSymbol {DesugarStat fCase(NewSymbol Clauses fNoElse(CatchPos) CatchPos) Params} )]
                                CatchPos )
                        fNoFinally
                        Pos)
                   Pos)

         [] fTry(Body fNoCatch Finally Pos) then
         %--------------------------------------
            Temp1=fSym({New SyntheticSymbol init(Pos)} Pos)
            Temp2=fSym({New SyntheticSymbol init(Pos)} Pos)
         in
            {DesugarStat
                        fLocal(Temp1
                           fAnd(
                              fLocal(Temp2
                                     fEq(Temp1
                                         fTry(fAnd(Body fConst(unit Pos))
                                              fCatch([fCaseClause(Temp2 fRecord(fConst(ex Pos) [fColon(fConst(1 Pos) Temp2)]))] Pos)
                                              fNoFinally
                                              Pos)
                                         Pos)
                                     Pos)
                              fAnd(Finally
                                   fBoolCase( fOpApply('==' [Temp1 fConst(unit Pos)] Pos )
                                              fSkip(unit)
                                              fRaise(fOpApply('.' [Temp1 fConst(1 Pos)] Pos) Pos)
                                              Pos)))
                           Pos)
                        Params}

         [] fTry(Body fCatch(Clauses CatchPos) Finally Pos) then
         %--------------------------------------
            % From http://www.mozart-oz.org/documentation/notation/node6.html#label27
            % put the try-finally statement, without its Finally code, in a try expression
            % with no finally code catching all exceptions. The try expression will have a value of unit
            % if the execution of the original try was successful.
            % After executing this new try expression, execute the Finally
            % then check if the finally was successful by looking at the value of the try expression.
            % If success, do nothing. Else, re-raise the exception.
            Temp1=fSym({New SyntheticSymbol init(CatchPos)} CatchPos)
            Temp2=fSym({New SyntheticSymbol init(CatchPos)} CatchPos)
         in
            {DesugarStat
                        fLocal(Temp1
                           fAnd(
                              fLocal(Temp2
                                     fEq(Temp1
                                         fTry(fAnd(fTry(Body fCatch(Clauses CatchPos) fNoFinally Pos)  fConst(unit Pos))
                                              fCatch([fCaseClause(Temp2 fRecord(fConst(ex Pos) [fColon(fConst(1 CatchPos) Temp2)]))] CatchPos)
                                              fNoFinally
                                              Pos)
                                         Pos)
                                     Pos)
                              fAnd(Finally
                                   fBoolCase( fOpApply('==' [Temp1 fConst(unit Pos)] Pos )
                                              fSkip(unit)
                                              fRaise(fOpApply('.' [Temp1 fConst(1 Pos)] Pos) Pos)
                                              Pos)))
                           Pos)
                        Params}

         [] fFunctor(Id SpecsList Pos) then
         %--------------------------------------
            {DesugarExpr {DesugarFunctor AST Params} Params}

         [] fRaise(E Pos) then
         %--------------------------------------
            fApply(fConst(Exception.'raise' Pos) [ {DesugarExpr E Params} ] Pos)

         [] fDotAssign(fOpApply('.' [LHS CHS] Pos1) RHS Pos2) then
         %--------------------------------------
            {DesugarStat fApply(fConst(Boot_Value.'dotAssign' Pos1) [LHS CHS RHS] Pos2) Params}

         [] fSkip(_) then
         %--------------------------------------
            AST

         [] fNoFinally then
         %--------------------------------------
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
end

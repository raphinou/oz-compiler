functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
export
   pass:Pass
   namer: Namer
   yAssigner: YAssigner
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

   % Will apply the function F under the feature {Label AST} in FsR. FsR has label fs.
   % This function F should take as arguments:
   %  - the record it will work on (AST)
   %  - a parameters record
   %  - a function, which will be Pass itself. The goal is that Pass is called on the features of AST by F.
   % FsR also has a feature params, which is the initial parameters to use
   fun {Pass AST FsR Params}
      L
   in
      if {Record.is AST} andthen {List.member L={Label AST} {Record.arity FsR}} then
         {FsR.L  AST Params Pass}
      elseif {Record.is AST} then
         {Record.map AST fun {$ I} {Pass I FsR Params} end}
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
   DeclsFlattenerRecord =  fs(
                        fLocal:  fun {$ fLocal(Decl Body Pos) Params F}
                                    Acc=params(acc:{NewCell nil})
                                    FinalAcc
                                    NewDecls
                                    NewBody
                                    Res
                                 in
                                    % this call will collect in Acc.acc all code to add to the body
                                    NewDecls={F Decl DeclsFlattenerRecord Acc}
                                    % list from which we'll build the fAnds.
                                    % The original Body is the last part of the body's code
                                    FinalAcc=Body|@(Acc.acc)
                                    % buidl the fAnd records
                                    NewBody={List.foldL FinalAcc.2 fun {$ Acc I} fAnd(I Acc) end FinalAcc.1}
                                    % Put all transformed parts in the new fLocal
                                    Res = fLocal(
                                       NewDecls
                                       NewBody
                                       Pos
                                    )
                                 end
                        fEq:     fun {$ AST=fEq(LHS RHS Pos) Params F}
                                    (Params.acc):= AST|@(Params.acc)
                                    {F LHS DeclsFlattenerRecord Params}
                                 end
                     )
   DeclsFlattenerParams = nil
   fun {DeclsFlattener AST}
      {Pass AST DeclsFlattenerRecord DeclsFlattenerParams}
   end

   % The namer replaces variable names with a Symbol instance, all identical
   % variable instances referencing the same symbol.
   % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
   % AST = the record
   % Params is a record with 2 features:
   %   env = mapping of var names to symbols built in parents
   %   indecls = should new vars be mapped to new symbols, ie are we in declarations?

   % Declare function to change fInt, fFloat, fAtom to fConst
   ToConst
   NamerRecord=fs(
                  fLocal:  fun {$ fLocal(Decl Body Pos) Params F}
                              Res
                           in
                              {Params.env backup()}
                              Res=fLocal(
                                 {F Decl NamerRecord {Record.adjoin Params params(indecls:true)}}
                                 {F Body NamerRecord {Record.adjoin Params params(indecls:false)}}
                                 Pos
                                 )
                              {Params.env restore()}
                              Res
                           end
                  fEq:  fun {$ fEq(LHS RHS Pos) Params F}
                           if Params.indecls then
                              % in declarations, only decend in the LHS because only the LHS variables are declared
                              fEq(
                                 {F LHS NamerRecord Params}
                                 RHS
                                 Pos
                              )
                           else
                              fEq(
                                 {F LHS NamerRecord Params}
                                 {F RHS NamerRecord Params}
                                 Pos
                              )
                           end
                        end
                  fVar: fun {$ AST=fVar(Name Pos) Params F}
                           Sym
                        in
                           if Params.indecls then
                              % assign symbol in declarations
                              Sym={Params.env setSymbol(Name Pos $)}
                              fSym( Sym Pos)
                           elseif {Params.env hasSymbol(Name $)} then
                              % if a symbol exists for this variable, use it as
                              % is it a local variable
                              Sym={Params.env getSymbol(Name $)}
                              fSym( Sym Pos)
                           else
                              % this variable has no symbol associated, it must be a global
                              AST
                           end
                        end
                  fInt: ToConst=fun {$ AST Params F}
                           % no pattern matching in formal argument to be able to reuse this function with multiple record labels
                           fConst(AST.1 AST.2)
                        end
                  fFloat: ToConst
                  fAtom:  ToConst

                  )

   NamerParams =  params(env:{New Environment init()} indecls:false)

   fun {Namer AST}
     {Pass AST NamerRecord NamerParams}
   end

   % traverses the tree and assigns Y registers to variables with no Y index yet
   YAssignerRecord = fs(
                           fSym : fun {$ AST=fSym(Sym _) Params F}
                                    if {Sym get(yindex $)}==nil then
                                       {Sym set(yindex @(Params.currentIndex))}
                                       (Params.currentIndex):=@(Params.currentIndex)+1
                                    end
                                    AST
                                 end
                        )
   YAssignerParams = params(currentIndex:{NewCell 0})

   fun {YAssigner AST}
      {Pass AST YAssignerRecord YAssignerParams}
   end

   GenCodeRecord = fs(
                        fLocal:  fun {$ fLocal(Decls Body _) Params F}
                                    [ {F Decls GenCodeRecord {Record.adjoin Params params(indecls: true)}} {F Body GenCodeRecord Params}]
                                 end
                        fSym:    fun {$ fSym(Sym _) Params F}
                                    if Params.indecls then
                                       createVar(y({Sym get(yindex $)}))
                                    else
                                       y({Sym get(yindex $)})
                                    end
                                 end
                        fVar:    fun {$ fVar(Name _) Params F}
                                 % all fVar we get here are globals, as the YAssigner should have replaced locals with fSym
                                    g(unknown)
                                 end
                        fAnd:    fun {$ fAnd(First Second) Params F}
                                    [{F First GenCodeRecord Params} {F Second GenCodeRecord Params}]
                                 end
                        fEq:     fun {$ fEq(LHS RHS _) Params F}
                                    unify({F LHS GenCodeRecord Params} {F RHS GenCodeRecord Params})
                                 end
                        fConst:    fun {$ fConst(Value _) Params F}
                                    k(Value)
                                 end
                     )
   GenCodeParams = params(indecls:false opCodes:{NewCell nil})

   fun {GenCode AST}
      {List.flatten {Pass AST GenCodeRecord GenCodeParams}}
   end


end

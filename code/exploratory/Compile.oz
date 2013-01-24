functor

import
   Narrator('class')
   ErrorListener('class')
   Compiler(parseOzVirtualString)
   System(printInfo showInfo show:Show)
   NewAssembler(assemble) at 'x-oz://system/NewAssembler.ozf'
   CompilerSupport(newAbstraction) at 'x-oz://system/CompilerSupport.ozf'
   DumpAST at './DumpAST.ozf'
   Debug at 'x-oz://boot/Debug'
define
   {Debug.setRaiseOnBlock {Thread.this} true}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Boilerplate code for the parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   PrivateNarratorO
   NarratorO = {New Narrator.'class' init(?PrivateNarratorO)}
   ListenerO = {New ErrorListener.'class' init(NarratorO)}

   fun {GetSwitch Switch}
      false
   end

   EnvDictionary = {NewDictionary}
   {Dictionary.put EnvDictionary 'Show' Show}

  %--------------------------------------------------------------------------------
  % The code we work on
  %--------------------------------------------------------------------------------
  %Code = 'local A = 5 B = 3 in {System.showInfo A + B} end'
   Code = 'local  A C=D in A=3.2   local A in A=6 end  A=7 end'


   AST = {Compiler.parseOzVirtualString Code PrivateNarratorO
          GetSwitch EnvDictionary}

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

  % This is the function to use for default handling of an AST.
  % Eg, the namer only has to do specific work on fLocal and fVar,
  % for which it has specific code. But for all other labels, it
  % just needs to recursively call itself on all features, which
  % is easily done with this function.
   fun {DefaultPass F AST Params}
      if {Record.is AST} then
         {Record.map AST fun {$ I} {F I Params} end}
      else
         AST
      end
   end

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

   % The namer replaces variable names with a Symbol instance, all identical
   % variable instances referencing the same symbol.
   % The environment is a dictionary, the keys being variable names, the value being their respective symbol instance
   % AST = the record
   % Params is a record with 2 features:
   %   env = mapping of var names to symbols built in parents
   %   indecls = should new vars be mapped to new symbols, ie are we in declarations?

   % Declare function to change fInt, fFloat, fAtom to fConst
   ToConst
   NamerRecord=fs(fLocal:  fun {$ AST=fLocal(Decl Body Pos) Params F}
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
                  fEq:  fun {$ AST=fEq(LHS RHS Pos) Params F}
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
                              Sym={Params.env setSymbol(Name Pos $)}
                           else
                              Sym={Params.env getSymbol(Name $)}
                           end
                           fSym( Sym Pos)
                        end
                  fInt: ToConst=fun {$ AST Params F}
                           % no pattern matching in formal argument to be able to reuse this function with multiple record labels
                           fConst(AST.1 AST.2)
                        end
                  fFloat: ToConst
                  fAtom:  ToConst

                  )

   NamerParams =  params(env:{New Environment init()} indecls:false)

   % traverses the tree and assigns Y registers to variables with no Y index yet
   YAssignerRecord = fs(
                           fSym : fun {$ AST=fSym(Sym Pos) Params F}
                                    if {Sym get(yindex $)}==nil then
                                       {Sym set(yindex @(Params.currentIndex))}
                                       (Params.currentIndex):=@(Params.currentIndex)+1
                                    end
                                    AST
                                 end
                        )
   YAssignerParams = params(currentIndex:{NewCell 0})

   GenCodeRecord = fs(  fLocal:  fun {$ AST=fLocal(Decls Body Pos) Params F}
                                    {F Decls GenCodeRecord {Record.adjoin Params params(indecls: true)}}#' '#{F Body GenCodeRecord Params}
                                 end
                        fSym:    fun {$ AST=fSym(Sym Pos) Params F}
                                    if Params.indecls then
                                       'createVar(y('#{Sym get(yindex $)}#'))\n'
                                    else
                                       'y('#{Sym get(yindex $)}#')'
                                    end
                                 end
                        fVar:    fun {$ AST=fVar(Name Pos) Params F}
                                 % all fVar we get here are globals, as the YAssigner should have replaced locals with fSym
                                    'g(??)'
                                 end
                        fAnd:    fun {$ AST=fAnd(First Second) Params F}
                                    {F First GenCodeRecord Params}#'\n'#{F Second GenCodeRecord Params}
                                 end
                        fEq:     fun {$ AST=fEq(LHS RHS _) Params F}
                                    'unify('#{F LHS GenCodeRecord Params}#' '#{F RHS GenCodeRecord Params}#')\n'
                                 end
                        fConst:    fun {$ AST=fConst(Value _) Params F}
                                    'k('#Value#')'
                                 end
                     )
   GenCodeParams = params(indecls:false)

   {System.showInfo '################################################################################'}
   {DumpAST.dumpAST AST}
   {System.showInfo '--------------------------------------------------------------------------------'}
   %{DumpAST.dumpAST {YAssigner {Namer AST.1}}}
   {DumpAST.dumpAST {Pass AST.1 NamerRecord NamerParams}}
   {System.showInfo '--------------------------------------------------------------------------------'}
   {System.showInfo {Pass {Pass {Pass AST.1 NamerRecord NamerParams} YAssignerRecord YAssignerParams } GenCodeRecord GenCodeParams}}
   {System.showInfo '################################################################################'}

end

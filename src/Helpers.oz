functor
import
   System(showInfo show:Show)
   OS
export
   Symbol
   SyntheticSymbol
   WrapIn
   WrapInFAnd
   UnWrapFAnd
   ListToAST
   IndexOf
   FirstOfP
   GenLabel
   GetPos
   DefaultPass
   DefaultPassNoParams
   StoreInSafe
   AccessSafe
   IsSafe
   ExtractFunctorSpecs

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
         name:=''
      end
      meth toVS(?R)
         % Override this method to avoid problems with irrelevant attributes
         R="'SSym"#@id#"y:"#@yindex#"g:"#@gindex#" type: "#@type#" procId "#@procId#"'"
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
      if {List.is L} then
         case L
         of X|Xs then
            Pos={GetPos X}
         in
            fRecord(fConst('|' Pos)
                      [fColon(fConst(1 Pos) X)
                      fColon(fConst(2 Pos) {ListToAST Xs})] )
         [] nil then
            % end of list
            fConst(nil pos)
         end
      else
         L
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
            if {Record.is X} andthen {Record.label X}==pos then
               X
       elseif {Record.is X} then
          Tmp
       in
          Tmp={GetPosInList {Record.toList X}}
          if Tmp==pos then
        {GetPosInList Xs}
          else
        Tmp
          end
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

   fun {DefaultPass AST F Params}
      % beware of the order. a record is also a list!
      if {List.is AST} then
         {List.map AST fun {$ I} {F I Params} end}
      elseif {Record.is AST} then
         case AST
         of pos(_ _ _ _ _ _) then
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

   % Helper functions to store and access data stored privately by the compiler.
   % FIXME: improve
   % Key is a name known only by the compiler, and used to protect data, notably in the pattern matching code.

   % FIXME : we add Show manually to the base environment.
   Key = {NewName}
   fun {StoreInSafe X}
      {NewChunk store(Key:X compiler_internal__:true)}
   end
   fun {AccessSafe X}
      X.Key
   end
   fun {IsSafe X}
      {IsChunk X} andthen {HasFeature X Key}
   end

   fun {ExtractFunctorSpecs ExportImportPrepareDefine }
      % Some names are plurals because they hold a list. Singular names don't hold a list.
      Require RequireItems Import ImportItems Prepare Define Export ExportItems
      PrepareDecls PrepareStats DefineDecls DefineStats
   in
      Require = {List.filter ExportImportPrepareDefine fun {$ I} {Record.label I}==fRequire end}
      Import = {List.filter ExportImportPrepareDefine fun {$ I} {Record.label I}==fImport end}
      Prepare = {List.filter ExportImportPrepareDefine fun {$ I} {Record.label I}==fPrepare end}
      Define = {List.filter ExportImportPrepareDefine fun {$ I} {Record.label I}==fDefine end}
      Export = {List.filter ExportImportPrepareDefine fun {$ I} {Record.label I}==fExport end}
      if Require==nil then
         RequireItems=nil
      else
         [fRequire(RequireItems _)]=Require
      end
      if Import==nil then
         ImportItems=nil
      else
         [fImport(ImportItems _)]=Import
      end
      if Export==nil then
         ExportItems=nil
      else
         [fExport(ExportItems _)]=Export
      end
      if Prepare==nil then
         PrepareDecls=nil
         PrepareStats=nil
      else
         [fPrepare(PrepareDecls PrepareStats _)]=Prepare
      end
      if Define==nil then
         DefineDecls=nil
         DefineStats=nil
      else
         [fDefine(DefineDecls DefineStats _)]=Define
      end
      f( 'require':Require
         'import':Import
         'prepare':Prepare
         'define':Define
         'export':Export
         requireItems:RequireItems
         importItems:ImportItems
         exportItems:ExportItems
         prepareDecls:PrepareDecls
         prepareStats:PrepareStats
         defineDecls:DefineDecls
         defineStats:DefineStats)
   end

end

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
   PVE
   PVS
   DeclsFlattener
define
  % This is the function to use for default handling of an AST.
  % Eg, the namer only has to do specific work on fLocal and fVar,
  % for which it has specific code. But for all other labels, it
  % just needs to recursively call itself on all features, which
  % is easily done with this function.
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
         [] fClass(E _ _ _) then
            {PVE E}
         [] fEq(LHS _ _) then
            {PVE LHS}
         [] fFunctor(Id _ _) then
            {PVE Id}
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
         % FIXME: see Sebastien's tip
         {Record.subtractList {PVE Body}  {Record.arity {PVS Decls} }}
      [] fEq(LHS RHS) then
         {Record.adjoin {PVE LHS} {PVE RHS}}
      [] fRecord(_ Features) then
         {List.foldL Features  fun {$ Acc I}
                                    case I
                                    of fColon(_ V) then
                                       {Record.adjoin {PVE V} Acc}
                                    else
                                       {Record.adjoin {PVE I} Acc}
                                    end
                                 end pv()}
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
         proc {NewDeclsAndBody Decls Body ?NewDecls ?NewBody}
            % Extract variables to be defined from declarations with the pattern variable functions PVS
            % Move the code from the declarations to the body, wrapping all in fAnds
            % Leave only the variables to be declared in the declaration part.

            CodeInDeclarations
            CodeInDeclatationsAST

            PatternVariables
         in
            CodeInDeclarations={CodeInDecls Decls}
            PatternVariables={PVS Decls}
            NewDecls={WrapInFAnd PatternVariables}


            CodeInDeclatationsAST={WrapInFAnd CodeInDeclarations}
            if CodeInDeclatationsAST==unit then
               NewBody={DeclsFlattenerInt Body Params}
            else
               NewBody=fAnd({DeclsFlattenerInt CodeInDeclatationsAST Params} {DeclsFlattenerInt Body Params})
            end

         end
      in
         case AST
         %------------------------
         of fLocal(Decls Body Pos) then
         %------------------------
            NewDecls
            NewBody
         in
            {NewDeclsAndBody Decls Body NewDecls NewBody}
            % Put all transformed parts in the new fLocal
            fLocal(
               NewDecls
               NewBody
               Pos
            )
         % FIXME: check the second feature, usually fSkip
         [] fDefine(Decls Body Pos) then
            NewDecls
            NewBody
         in
            {NewDeclsAndBody Decls Body NewDecls NewBody}
            % Put all transformed parts in the new fLocal
            fDefine(
               NewDecls
               NewBody
               Pos
            )

         [] fPrepare(Decls Body Pos) then
            NewDecls
            NewBody
         in
            {NewDeclsAndBody Decls Body NewDecls NewBody}
            % Put all transformed parts in the new fLocal
            fPrepare(
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
      {Show '---------------'}
      {Show 'DeclsFlattener'}
      {Show '---------------'}
      {DeclsFlattenerInt AST unit}
   end

end

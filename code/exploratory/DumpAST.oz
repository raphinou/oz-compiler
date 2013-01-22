functor 
export 
   dumpAST:DumpAST
import
   System
define
   fun {IsCoord V}
      case V
      of pos(_ _ _) then true
      [] pos(_ _ _ _ _ _) then true
      [] unit then true
      else false
      end
   end
   
   fun {IsTrivial V}
      {IsInt V} orelse
      {IsLiteral V} orelse
      {IsFloat V} orelse
      {IsCoord V}
   end

   fun {IsOneLiner AST}
      {IsTrivial AST} orelse
      {Record.all AST IsTrivial}
   end

   fun {TrivialToVS V}
      case V
      of pos(A B C) then
         'pos('#A#' '#B#' '#C#')'
      [] pos(A B C D E F) then
         'pos('#A#' '#B#' '#C#' '#D#' '#E#' '#F#')'
      [] unit then "unit"
      [] true then "true"
      [] false then "false"
      [] 'unit' then "'unit'"
      [] 'true' then "'true'"
      [] 'false' then "'false'"
      elseif {IsAtom V} then
         {AtomToString V}
      else
         V
      end
   end

   fun {OneLinerToVS AST}
      if {IsTrivial AST} then
         {TrivialToVS AST}
      else
         {Record.foldL AST
          fun {$ Prev Field}
             Prev#{TrivialToVS Field}#' '
          end
          {Label AST}#'('}#')'
      end
   end
   
   proc {DumpASTEx AST Indent}
      if {IsOneLiner AST} then
         {System.showInfo Indent#{OneLinerToVS AST}}
      else
         NewIndent = Indent#'   '
      in
         {System.showInfo Indent#{Label AST}#'('}
         {Record.forAll AST proc {$ X} {DumpASTEx X NewIndent} end}
         {System.showInfo Indent#')'}
      end
   end

   proc {DumpAST AST}
      {DumpASTEx AST ''}
   end
%% in
%   {DumpAST fAnd(fRecord(fVar('Hello' pos(1 2 3))
%                         fInt(5 pos(4 5 6))
%                         pos('Hello' 42 3))
%                 fAtom(unit))}
%%    skip
end

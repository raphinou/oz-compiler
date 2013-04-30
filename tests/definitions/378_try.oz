% Try with multiple catch clauses
local
   F R
in
   fun {F E}
      if {Record.width E}>0 then
      {Show raising#E}
         raise E end
      else
         raise error(E) end
      end
      ok
   end
  {Show try Res={F halt}
         in commit(Res)
         catch error(halt) then
            abort(halt completeMatch)
         [] error(E) then
            abort(E otherValueOfExc)
         [] failure(E) then
            {Show 'Failure'}
            E
        end }
  {Show try Res={F weeez}
         in commit(Res)
         catch error(halt) then
            abort(halt completeMatch)
         [] error(E) then
            abort(E otherValueOfExc)
         [] failure(E) then
            {Show 'Failure'}
            E
        end }
  {Show try Res={F failure(myError)}
         in commit(Res)
         catch error(halt) then
            abort(halt completeMatch)
         [] error(E) then
            abort(E otherValueOfExc)
         [] error2(E) then
            abort(E otherValueOfExc2)
         [] failure(E) then
            E
        end }
  {Show try Res={F failure(myError)}
         in commit(Res)
         catch error(halt) then
            abort(halt completeMatch)
         [] error(E) then
            abort(E otherValueOfExc)
         [] error2(E) then
            abort(E otherValueOfExc2)
         [] failure(E) then
            E
        end }
  {Show try Res={F failure(myError)}
         in commit(Res)
         catch error(halt) then
            abort(halt completeMatch)
         [] error(E) then
            abort(E otherValueOfExc)
         [] failure(E) then
            T
         in
            {Show 'Failure'}
            E
        end }
end

local
   F R
in
   fun {F E}
      raise E end
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
         [] failure(E) then
            {Show 'Failure'}
            E
        end }
end

% Pattern matching in function and proc arguments
local
   F P
in
   fun {F rec(A b)}
      A
   end
   proc {P rec(A b)}
      {Show A}
   end
   {P rec(withproc b)}
   {Show {F rec(withfun b)}}
end


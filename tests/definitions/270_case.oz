% Pattern arguments, proc and fun
local
   F P
in
   fun {F A1 rec(A b)}
      A
   end
   proc {P A1 rec(A b)}
      {Show A}
   end
   {P nil rec2(a b)}
   {Show {F nil rec(a b)}}
end

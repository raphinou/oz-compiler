local
   F P
in
   fun {F rec(A b)}
      A
   end
   proc {P rec(A b)}
      {Show A}
   end
   {P rec2(a b)}
   {Show {F rec(a b)}}
end

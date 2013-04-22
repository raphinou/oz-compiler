% dollar present in a record argument of a procedure
local
   P
in
   proc {P R}
      R=rec(inner(4 5))
   end
   {Show {P rec(inner(_ $))}}
end


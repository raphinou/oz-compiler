% Netsted locals, with one unification done in the declaration of B.
local
   A
   B=3
in
   A=3.2
   {Show A}
   local
      A
   in
      A=6
      {Show A}
   end
   {Show A}
end

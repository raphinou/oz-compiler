% Global in proc
 % Procedure definition and call, with unification done in declaration of A used
 % as argument of the call.

local
   A=5 P B C
in
   proc {P V}
      {Show B}
      {Show C}
      {Show V}
   end
   B = 7
   C = 9
   {P A}
   {For 1 5 1 P}
end

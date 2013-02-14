% global A is redeclared by T, and the argument passed to the call of T is that global.
local
   A P B
in
   proc {P V}
      T W
   in
      proc {T U}
         A=44
      in
         {Show A}
         {Show U}
      end
      proc {W}
         {Show A}
      end
      {W}
      {T A}
   end
   A = 5
   B = 7
   {P A}
end

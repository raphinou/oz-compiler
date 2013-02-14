% This code failed in an ealier version, left here for safety.
local
   A P B
in
   proc {P V}
      T
   in
      proc {T U}
         Text=44
      in
         {Show Text}
         {Show U}
         {Show A}
      end
      {T V}
   end
   A = 5
   B = 7
   {P A}
   {For 1 5 1 P}
end

% Three levels deep nested locals, with redeclaration and new variable at each level.
local
   A=1
in
   local
      A=2
      B=3
   in
      local
         B=4
         C=5
      in
         {Show A}
         {Show B}
         {Show C}
      end
      {Show A}
      {Show B}
   end
   {Show A}
end

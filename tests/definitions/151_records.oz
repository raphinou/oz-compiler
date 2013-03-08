% auto number features of records if necessary (desugar step)
local
   R = rec(f1 b:f2 c:f3)
   R2= rec(a b c)
in
   {Show R.1}
   {Show R.b}
   {Show R2.1}
   {Show R2.2}
   {Show R2.3}
end


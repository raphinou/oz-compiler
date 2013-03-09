% constant records
local
   R = rec(f1 b:f2 c:f3 r:rec(1 2 3))
in
   {Show R}
   {Show R.1}
   {Show R.r}
   {Show R.r.1}
end


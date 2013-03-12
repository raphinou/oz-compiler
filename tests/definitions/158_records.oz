% Record with value result of an if expression
local
   R V
in
   V=2
   R=rec(a:if V==2 then 2 else 0 end b:V*2)
   {Show R}
end

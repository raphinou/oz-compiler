% Records where values are results of function call
local
   A B R R2
   fun{F I}
      I*2
   end
in
   A=1
   B=2
   R=rec(a:A b:B)
   R2=rec(a r:R f:{F 2})
   {Show R2}
end


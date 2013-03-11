% Records where values are not constants
local
   R A B R2
in
   A=1
   B=2
   R=rec(a:A b:B )
   R2=rec(a r:R)
   {Show R}
   {Show R2}
end

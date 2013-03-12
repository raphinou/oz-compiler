% Unification of record to assign value by 'pattern matching'
local
   R V
in
   R=rec(a:1)
   rec(a:V)=R
   {Show V}
end

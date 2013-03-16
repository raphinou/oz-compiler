% Thread as expression, at the 2 sides of unification
local
   A A2 B C
in
   A=thread B+C end
   thread B-C end=A2
   B=1
   C=2
   {Show A+0}
   {Show A2+0}
end

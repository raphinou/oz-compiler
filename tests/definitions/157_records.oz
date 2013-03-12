% Records on LHS of unification, both const and not const
local
   R A B C L FA FB
   R2 R3
in
   FA=a
   A=1
   FB=b
   B=2
   L=lab
   L(FA:A FB:B )=R

   {Show R}

   C=3
   rec(a FB:b c:C)=R2
   {Show R2}

   rec(a:1 b:2)=R3
   {Show R3}
end

% Pattern matchin, recursive calls, guards,...
local
   ExprCode
in

proc {ExprCode Expr Cin Cout Nin Nout}
   case Expr
   of [E1 plus E2] then C1 N1 C2 N2 in
     C1=plus|Cin
     N1=Nin+1
     {ExprCode E2 C1 C2    N1 N2}
     {ExprCode E1 C2 Cout  N2 Nout}
   [] I andthen {IsInt I} then
     Cout=push(I)|Cin
     Nout=Nin+1
   end
end

local C N in
   {ExprCode [[3 plus 3] plus 2] nil C 0 N}
   {Show C}
   {Show N}
end
end

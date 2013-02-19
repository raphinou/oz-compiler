% Unnesting of variable assignment with variable being the RHS and in proc argument
local
   ShowPlusOne
   A
   B
in
   5-2+(8+4)*2 = A
   A*2+4=B
   proc {ShowPlusOne Arg}
     {Show Arg+1}
   end
   {ShowPlusOne A}
   {ShowPlusOne B}
   {ShowPlusOne 2*A+3*B}
end

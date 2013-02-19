% Unnesting of variable assignment in declarations of a local and in proc argument
local
   ShowPlusOne
   A = 5-2+(8+4)*2
   B = A*2+4
in
   proc {ShowPlusOne Arg}
     {Show Arg+1}
   end
   {ShowPlusOne A}
   {ShowPlusOne B}
   {ShowPlusOne 2*A+3*B}
end

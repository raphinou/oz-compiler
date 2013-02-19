% Unnesting of variable assignment in declarations of a local and in proc argument
local
   A = 5-2+(8+4)*2
   B = A*2+4
in
   {Show A}
   {Show B}
   {Show 2*A+3*B}
end

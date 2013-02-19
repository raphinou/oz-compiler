% Unnesting of variable assignment with variable being the RHS and in proc argument
local
   ShowAllFour
   A
   B
in
   5-2+(8+4)*2 = A    %27
   A*2+4=B            %58
   proc {ShowAllFour A1 A2 A3 A4}
     {Show A1}
     {Show A2}
     {Show A3}
     {Show A4}
   end
   {ShowAllFour 1 2 3 4}
   {ShowAllFour 1 (5*15-70)*2-8 2*A+3*B-225 4}
   {ShowAllFour 123*2-245 (5*15-70)*2-8 2*A+3*B-225 4*4-3*4}
end

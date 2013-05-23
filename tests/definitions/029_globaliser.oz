% Is environment restored avec proc definition?
  % The argument of a proc has the same name as a locally declared variable.
  % This tests if the namer restores the environment after a proc. If it does
  % not, the symbol used for A in the 2 last instructions would be the symbol
  % corresponding to the formal parameter of ShowPlusOne
local
   A = 5
   ShowPlusOne
in
   proc {ShowPlusOne A}
     {Show A+1}
   end
   {Show A}
   {ShowPlusOne A}
end


% The argument of a proc has the same name as a locally declared variable.
% --SKIP TEST--
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


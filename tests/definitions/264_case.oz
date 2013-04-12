% case with only constant tests, i.e. no capture

local
   A=2
in
   % Third test matches
   case A
   of 3 then
      {Show 3}
   [] rec(a b c) then
      {Show rec}
   [] 2 then
      {Show 2}
   else
      {Show elseBranch}
   end
   {Show '---'}
   % Else is executed
   case A
   of 3 then
      {Show 3}
   [] rec(a b c) then
      {Show rec}
   [] 4 then
      {Show 4}
   else
      {Show elseBranch}
   end
   {Show '---'}
end

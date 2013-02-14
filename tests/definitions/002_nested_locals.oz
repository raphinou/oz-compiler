% The top local declared and initialises variables A and B.
% The nested local redeclares B and initialises it with another value.
% In the nested local, we then show both A and B.
% In the outer local, we show A and B to check it has the top level value.
local
   A=5
   B=6
in
   local
      B=7
   in
      {Show A}
      {Show B}
   end
   {Show A}
   {Show B}
end

% Simple desugar of + - * / and unnesting of that operation's result assignment
local
   A = 8
   B = 2
   Add Sub Mult Div
in
   Add = A + B
   Sub = A - B
   Mult= A * B
   Div = A div B
   {Show Add}
   {Show Sub}
   {Show Mult}
   {Show Div}
end

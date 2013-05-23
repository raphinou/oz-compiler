% Statement 'for' over lists with pattern matching
local
   L=[ rec(a 1) rec(b 2) rec(c 3)]
in
   for rec(F V) in L do
      {Show F#V}
   end
end

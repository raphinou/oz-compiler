% Declarations in records (PVS and PVE)
local
   rec(A B C)=rec(1 2 3)
   rec(inner(D E) F)=rec(inner(3 4) 6)
   '|'(G H)='|'(7 8)
   I|J|K|nil=9|10|11|nil
   rec(l:L m:M n:N)=rec(l:12 m:13 n:14)
in
   {Show A#B#C#D#E#F#G#H#I#J#K#L#M#N}
end


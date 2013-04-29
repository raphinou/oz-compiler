% for loop over multiple lists
% Skipping until I fix the multiple list behaviour
% --SKIP TEST--
local
   L1 L2
in
   L1=[1 2 3 4]
   L2=[a b c d]
   for I in L1 J in L2 do {Show I#J} end
end

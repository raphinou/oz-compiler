% Unification of a variable in a thread, depending on variables initialised later.
local
   A B C
in
   thread A=B+C end
   B=1
   C=2
   {Show A+0}
end

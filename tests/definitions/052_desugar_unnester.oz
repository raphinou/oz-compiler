% Desugar of functions, some arguments are call functions results
% --SKIP TEST--
local
   Add Mul Sub
   One=1
   Two=2
   Three=3
in
   fun {Add A B}
      fun {AddInt First Second}
         First
      end
      {AddInt A B}+B
   end
   fun {Mul A B}
      A*B
   end
   fun {Sub A B}
      R=A-B
   in
      R
   end
   {Show {Add 2 3}}
   {Show {Add {Add 1 1} {Sub 4 1}}}
   {Show {Add {Mul 3 Two} One}}
end

% Two nested proc definitions (T and W)  at the same level use the same variable (A) which is
% also a new local to the outer proc (P), which is used after the nested procs definitions.
local
    A P B
 in
    proc {P V}
       T W
    in
       proc {T U}
         {Show A}
         {Show A}
         {Show U}
       end
       proc {W}
          {Show A}
       end
       {Show A}
       {W}
       {T B}
    end
    A = 5
    B = 7
    {P A}
 end

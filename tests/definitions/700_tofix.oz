% Pattern matching on procedure arguments when record is handled by Boot_Record.makeDynamic
% --SKIP TEST--
local
   A P
in
   A=a
   proc {P ?R}
      R=rec(a:1 b:2)
   end
   {Show {P rec(A:$ b:2)}}
end


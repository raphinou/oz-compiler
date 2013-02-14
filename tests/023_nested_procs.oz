% A new local variables is used by P *before* it is also used by a nested proc definition.
% We need to be sure that the new local in T references the new local in P.
local
   A P B
in
   proc {P V}
      T
   in
      {Show A}   % use before a nested proc request a new local creation for it
      proc {T U}
        A=44
      in
        {Show A}
        {Show U}
        {Show V}
      end
      {T A}
   end
   A = 5
   B = 7
   {P A}
   {For 1 B 1 P}
end

% P uses a global variable after a child proc (T) definition used it.
% In this case, the new local for P was triggered by a child proc definition
% before a direct use of the variable by the procedure.
% We need to check that the symbol for A in the call {T A} is the one
% referenced by the symbol for A in the definition of T
% The inner proc also uses the outer proc's argument directly: the outer proc's
% argument is a global to the inner proc
local
   A P B
in
   proc {P V}
      T
   in
      proc {T U}
        A=44
      in
        {Show A}
        {Show U}
        {Show V}
      end
      {T A} % use it after a nested proc requested a new local for it
   end
   A = 5
   B = 7
   {P A}
   {For 1 B 1 P}
end

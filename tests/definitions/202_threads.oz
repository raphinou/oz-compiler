% Function calls in thread expressions
% Needs recursive functions support
local
   P1 P2
   R
in
   fun {P1 N}
      if N>0 then
         N+{P1 N-1}
      else
         N
      end
   end
   fun {P2 N}
      if N>0 then
         N*{P1 N-1}
      else
         N
      end
   end

   R=thread {P1 1000} end  + thread {P2 1000} end
   {Show R+0}
end

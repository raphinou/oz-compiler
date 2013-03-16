% Lazy functions. Result variables used in opposite order than unification, and one result unused.
local
   Inc Dec Identity
   R1 R2 R3
in
   fun lazy {Inc N}
      {Show 'Running lazy function Inc with argument:'}
      {Show N}
      N+1
   end
   fun lazy {Dec N}
      {Show 'Running lazy function Dec with argument:'}
      {Show N}
      N-1
   end
   fun lazy {Identity N}
      {Show 'Running lazy function Indentity with argument:'}
      {Show N}
      N
   end

   R1={Inc 100}
   R2={Dec 200}
   R3={Identity 300}
   {Show R2+0}
   {Show R1+0}
end

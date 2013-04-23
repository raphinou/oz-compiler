% Open record method definitions
local
   C I
in
   class C
      meth init skip end
      meth echo(a:A b:B ...)
         {Show A}
         {Show B}
      end
   end
   I={New C init}
   {I echo(a:1 b:2 3 4)}
end

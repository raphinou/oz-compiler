% Method head reference
local
 C L A I
in
   class C
      meth init skip end
      meth echo(M)=H
         {Show H}
         {Show M}
      end
      meth normal(M)
         {Show M}
      end
   end
   I={New C init}
   {I echo(test)}
   {I normal(ntest)}
end

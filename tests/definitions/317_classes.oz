% Test of pattern variable function for classes
local
   class C
      attr
         val
      meth init(V)
         val<-V
      end
      meth get(Prec $)
         Prec=@val-1
         @val
      end
   end
   I={New C init(6)}
in
   {Show {I get(_ $)}}
end



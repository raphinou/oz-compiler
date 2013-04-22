% method calls as expression
local
   C I
in
   class C
      attr
         val
      meth init(V)
         val<-V
      end
      meth get(Prec Val)
         Prec=@val-1
         Val=@val
      end
   end
   I={New C init(6)}
   {Show {I get(_ $)}}
   {Show {I get($ _)}}
end


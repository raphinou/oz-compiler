% Static call with nesting marker
local
   C I A
in
   class A
      meth init(V R)
         R=rec(2*V)
      end
   end
   class C
      attr
         val
      meth init(V)
         val<-A,init(V rec($))
      end
      meth get(Prec Val)
         Prec=@val-1
         Val=@val
      end
   end
   I={New C init(6)}
   { Show {I get(_ $)}}  %12
   {Show {I get($ _)}}   %11
end


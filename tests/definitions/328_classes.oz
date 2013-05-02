% inherintance from a final class
local
   C1 C2 I
in
   class C1
      prop final
      attr
    total:0
      meth init skip end
   end
   class C2 from C1
      meth status($)
    @total
      end
   end



   I ={New C2 init }
   {Show {I status}}
end

% locking property
local
   C I
   T1 T2
in
   class C
      prop locking
      attr
         total:0
      meth init skip end
      meth update(Amount)
         Total
      in
         lock
            Total=@total+Amount
            {Delay {OS.rand} mod 30}
            total:=Total
         end
      end
      meth getTotal($)
         @total
      end
   end
   I ={New C init }
   thread
      for J in 1..100 do
	      {I update(J)}
      end
      T1=unit
   end
   thread
      for J in ~1..~100;~1 do
	      {I update(J)}
      end
      T2=T1
   end
   {Wait T2}
   {Show {I getTotal($)}}
end

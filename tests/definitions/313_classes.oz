% Static calls
local
   A C I
in
   class A
      attr check
      meth init
         check:=ok
      end
   end
   class C from A
      attr
         count:0
      meth init
         A,init
      end
      meth echo(S)
         {Show S}
         count<-@count+1
      end
      meth stats
         {Show @count}
         {Show @check}
      end
   end
   I={New C init}
   {I echo(1)}
   {I echo(a)}
   {I echo(z)}
   {I stats}

end


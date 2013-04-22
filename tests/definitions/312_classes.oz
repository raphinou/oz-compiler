% Attribute assignation with <-
local
   C I
in
   class C
      attr
         count:0
      meth init skip end
      meth echo(S)
         {Show S}
         count<-@count+1
      end
      meth stats
         {Show @count}
      end
   end
   I={New C init}
   {I echo(1)}
   {I echo(a)}
   {I echo(z)}
   {I stats}

end


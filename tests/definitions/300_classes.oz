% Simple class instanciation and object method call
local
   C I
in
   class C
      meth init()
         skip
      end
      meth hello(A1)
         {Show A1}
      end
   end
   I = {New C init()}
   {I hello('Jon')}
end


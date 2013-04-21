% Simple class inheritance
local
   C1 C2 I
in
   class C1
      attr
         type:1
      meth init skip end
   end
   class C2 from C1
      attr
         child:true
      meth show()
         {Show @type}
      end
   end
   I = {New C2 init}
   {I show()}
end

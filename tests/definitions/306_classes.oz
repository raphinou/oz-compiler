% method with no argument and class with not feat or attr.
local
   C I
in
   class C
      meth init skip end
      meth echo(S)
         {Show S}
      end
   end
   I = {New C init}
   {I echo('hellooo')}
end


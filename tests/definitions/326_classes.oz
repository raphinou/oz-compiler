% Method head capture for method with no argument
local
   C I
in


class C
   meth init skip end
   meth test=M
      {Show M}
   end
end
I={New C init}
{I test}
end


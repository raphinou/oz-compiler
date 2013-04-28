% Private method with same variable name as class
local
   C I
in


class C
   meth init skip end
   meth C(M)
      {Show M}
   end
   meth whisper(M)
      {self C(M)}
   end
end
I={New C init}
{I whisper('hello')}
end


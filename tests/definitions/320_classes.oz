% private and dynamic method labels
local
 C L A I
in
   A=echo
   class C
      meth init skip end
      meth !A(M)
         {Show M}
      end
      meth B(M)
         {Show private}
         {Show M}
      end
      meth whisper(M)
         {self B(M)}
      end
   end
   I={New C init}
   {I echo(test)}
   {I whisper(shhhh)}
end


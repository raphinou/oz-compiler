% Private and dynamic method labels
% also tests otherwise method
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
      meth D(M)
         {Show private2}
         {Show M}
      end
      meth whisper(M)
         {self B(M)}
      end
      meth whisper2(M)
         {self D(M)}
      end
      meth otherwise(M)
         {Show otherwise}
         {Show M}
      end
   end
   I={New C init}
   {I echo(test)}
   {I whisper(shhhh)}
   {I whisper2(shhhh)}
   {I bla(hehe)}
end


% Method head captures with same name
local
 C L A I
in
   class C
      meth init skip end
      meth echo(M)=H
         {Show H}
         {Show M}
      end
      meth echoall(M)=H
         {Show 'Multiple args possible'}
         {Show H}
         {Show 'First:'}
         {Show M}
      end
   end
   I={New C init}
   {I echo(test)}
   {I echoall(first second third)}
end


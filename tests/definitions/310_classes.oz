% Anonymous class
local
   C I
in
   C = class $
         meth init skip end
         meth hello(S) {Show 'Hello'#S} end
       end
   I = {New C init}
   {I hello('Cowboy Smith')}
end


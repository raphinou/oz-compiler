% Call a method on self
local
   C I
in
   C = class $
         meth init skip end
         meth echo(S) {Show S} end
         meth hello(S) {self echo('Hello'#S)} end
       end
   I = {New C init}
   {I hello('Mike')}
end


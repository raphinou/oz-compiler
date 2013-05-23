% Key arguments in class methods
local
   C I
in
   class C
      meth init()
         skip
      end
      meth hello(name:Name firstname:Firstname)
         {Show Name#Firstname}
      end
   end
   I = {New C init()}
   {I hello(firstname:'Alan' name:'Turing')}
end

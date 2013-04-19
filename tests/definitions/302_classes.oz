% Default values for method arguments
local
   C I
in
   class C
      meth init()
         skip
      end
      meth hello(name:Name<='Doe' firstname:Firstname)
         {Show Name#Firstname}
      end
      meth greet(Name<='Unknown')
         {Show 'Hello '#Name}
      end
   end
   I = {New C init()}
   {I hello(firstname:'Alan' name:'Turing')}
   {I hello(firstname:'Jon')}
   {I greet('Mozart')}
   {I greet()}
end

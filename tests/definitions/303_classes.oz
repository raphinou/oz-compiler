% Attribute access, including with cell values + accessing non-attribute cells in a method.
local
   C I
in
   class C
      attr
         greeting:'Hello'
         closing:'Bye'
         cellattr:{NewCell 1}
      meth init()
         skip
         %greeting:='Hello'
      end
      meth hello(name:Name<='Doe' firstname:Firstname)
         {Show Name#Firstname}
      end
      meth greet(Name<='Unknown')
         C
      in
         C={NewCell 0}
         {Show @greeting#Name}
         {Show @C}
         {Show @@cellattr}
      end
   end
   I = {New C init()}
   {I hello(firstname:'Alan' name:'Turing')}
   {I hello(firstname:'Jon')}
   {I greet('Mozart')}
   {I greet()}
end

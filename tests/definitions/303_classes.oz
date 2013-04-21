% Attribute access, including with cell values + accessing non-attribute cells in a method.
% Also access attribute which name is accessed via a variable.
local
   C I
in
   class C
      attr
         greeting:'Hello'
         closing:'Bye'
         fr:'Bonjour'
         nl:'Goeiedag'
         en:'Hi'
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
      meth greeti18n(lang:Lang name:Name)
          {Show @Lang#Name}
      end
   end
   I = {New C init()}
   {I hello(firstname:'Alan' name:'Turing')}
   {I hello(firstname:'Jon')}
   {I greet('Mozart')}
   {I greet()}
   {I greeti18n(lang:fr name:'Marie')}
   {I greeti18n(lang:en name:'Jon')}
   {I greeti18n(lang:nl name:'Peter')}
end

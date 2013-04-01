local
   R1=lab(1:a)
   R2=lab(1:a 2:b 3:c)
   R3=rec(1:b)
   R4=2
   R5=none
   R6='string value'
   R7=lab6(1:inner(1:inner2(1:aa 2:b 3:c) 2:b 3:c))
   proc {Check V}
      case V
      of lab(1:A) then
         {Show 'Record Match'}
         {Show A}
         {Show '--------------------'}
      [] rec(1:B) then
         {Show 'Other Record Match'}
         {Show B}
         {Show '--------------------'}
      [] lab(1:a 2:B...) then
         {Show 'OpenRecord Match'}
         {Show B}
         {Show '--------------------'}
      [] lab6(1:inner(1:inner2(1:A ...) 3:C...)) then
         {Show 'Nested OpenRecord match'}
         {Show A}
         {Show C}
         {Show '--------------------'}
      [] 2 then
         {Show 'Integer match'}
         {Show 2}
         {Show '--------------------'}
      [] none then
         {Show 'Atom match'}
         {Show none}
         {Show '--------------------'}
      [] 'string value' then
         {Show 'String Match'}
         {Show '--------------------'}
      else
         {Show 'Else branch'}
         {Show '--------------------'}
      end
   end
in
   {Check R1}
   {Check R2}
   {Check R3}
   {Check R4}
   {Check R5}
   {Check R6}
   {Check R7}
end


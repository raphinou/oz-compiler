local
   R1=3
in
   {Show before}
   case R1
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
   end
   {Show after}

end


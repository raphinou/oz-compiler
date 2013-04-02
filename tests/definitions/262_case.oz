% case expression
local
   R1=lab(1:a)
   R2=lab(1:a 2:b 3:c)
   R3=rec(1:b)
   R4=2
   R5=none
   R6='string value'
   R7=lab6(1:inner(1:inner2(1:aa 2:b 3:c) 2:b 3:c))
   fun {Check V}
      case V
      of lab(1:A) then
         A
      [] rec(1:B) then
         B
      [] lab(1:a 2:B...) then
         B
      [] lab6(1:inner(1:inner2(1:A ...) 3:C...)) then
         A#C
      [] 2 then
         2
      [] none then
         none
      [] 'string value' then
         string
      else
         elseBranch
      end
   end
in
   {Show {Check R1}}
   {Show {Check R2}}
   {Show {Check R3}}
   {Show {Check R4}}
   {Show {Check R5}}
   {Show {Check R6}}
   {Show {Check R7}}
end


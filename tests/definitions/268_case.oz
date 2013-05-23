% Pattern conjunction
local
   R=rec(inner(a b))

   A=a
   B=b
in
   case R
   of rec(Foo=inner(Capt b)) then
      {Show Foo}
      {Show Capt}
   [] rec(!A !B c) then
      {Show error}
   [] rec( b c) then
      {Show error}
   else
      {Show error}
   end

   case R
   of rec(inner(Capt !B)=Foo) then
      {Show Foo}
      {Show Capt}
   [] rec(!A !B c) then
      {Show error}
   [] rec( b c) then
      {Show error}
   else
      {Show error}
   end

   case R
   of rec(inner(a Capt)) then
      {Show Capt}
   [] rec(!A !B c) then
      {Show error}
   [] rec( b c) then
      {Show error}
   else
      {Show error}
   end
end


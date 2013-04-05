% cases with captures and guards
local
   R=lab(a b c)
   R2=rec(inner:R b c)
in
   case R
   of lab(a B ...) andthen B==b then
      {Show openrec}
      {Show B}
   [] lab(a b c) then
      {Show completerec}
   else
      {Show elseBranch}
   end

   case R
   of lab(a B ...) andthen B==c then
      {Show openrec}
      {Show B}
   [] lab(a C B) then % intentionally switched variable names
      {Show completerec}
      {Show C}
      {Show B}
   else
      {Show elseBranch}
   end

   case false
   of lab(a B ...) andthen B==c then
      {Show openrec}
      {Show B}
   [] lab(a C B) then
      {Show completerec}
      {Show C}
      {Show B}
   else
      {Show elseBranch}
   end

   case R2
   of false then
      {Show false}
   [] rec(inner:lab(Capture ...) ...) then
      {Show nested}
      {Show Capture}
   end
end

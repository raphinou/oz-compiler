% Wildcards in record and openrecords patterns
local
   R=lab(a b c d)
in
   %record
   case R
   of lab(a B _ D) then
      {Show D}
      {Show ok}
   else
      {Show ko}
   end

   % openrecord
   case R
   of lab(a _ C ...) then
      {Show C}
      {Show ok}
   else
      {Show ko}
   end
end

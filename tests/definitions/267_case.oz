% Test !Vars in patterns

local
   R=rec(a b c)
   A=a
   B=b
in
   case R
   of rec(a !B c) andthen B==b then
      {Show matchb}  %<--
   [] rec(!A !B c) then
      {Show matchab}
   [] rec(a b c) then
      {Show matchrecord}
   else
      {Show nomatch}
   end

   case R
   of rec(a !A c) andthen B==b then
      {Show matchb}
   [] rec(!A !B c) then
      {Show matchab}   %<--
   [] rec(a b c) then
      {Show matchrecord}
   else
      {Show nomatch}
   end

   case R
   of rec(a !A c) andthen B==b then
      {Show matchb}
   [] rec(!A !A c) then
      {Show matchab}
   [] rec(a b c) then
      {Show matchrecord}    %<--
   else
      {Show nomatch}
   end

   case R
   of rec(a !A c) andthen B==b then
      {Show matchb}
   [] rec(!B !A c) then
      {Show matchab}
   [] rec(a b c) then
      {Show matchrecord}    %<--
   else
      {Show nomatch}
   end

   case R
   of rec(a !A c) andthen B==b then
      {Show matchb}
   [] rec(!B !A c) then
      {Show matchab}
   [] rec(a b d) then
      {Show matchrecord}
   else
      {Show nomatch}    %<--
   end
end

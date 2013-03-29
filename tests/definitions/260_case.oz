local
   R1=lab(1:a)
   R2=lab(1:a 2:b 3:c)
   R3=rec(1:b)
   R4=2
   R5=none
in

   % Match of complete record
   case R1
   of lab(1:A) then
      {Show A}
   [] rec(1:B) then
      {Show B}
   [] lab(1:a 2:B...) then
      {Show B}
      {Show openrec}
   [] 2 then
      {Show 2}
   else
      {Show bigger}
   end

   % Match of open record with capture
   case R2
   of lab(1:A) then
      {Show A}
   [] rec(1:B) then
      {Show B}
   [] lab(1:a 2:B...) then
      {Show B}
      {Show openrec}
   [] 2 then
      {Show 2}
   else
      {Show bigger}
   end

   % Match of another complete record
   case R3
   of lab(1:A) then
      {Show A}
   [] rec(1:B) then
      {Show B}
   [] lab(1:a 2:B...) then
      {Show B}
      {Show openrec}
   [] 2 then
      {Show 2}
   else
      {Show bigger}
   end

   % Match of a const
   case R4
   of lab(1:A) then
      {Show A}
   [] rec(1:B) then
      {Show B}
   [] lab(1:a 2:B...) then
      {Show B}
      {Show openrec}
   [] 2 then
      {Show 2}
   else
      {Show bigger}
   end
end


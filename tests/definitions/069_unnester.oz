% Unnest case values
local
   A=a
   B=b
in
   case A#B
   of a#b then
      {Show A#B}
   else
      {Show ko}
   end
end

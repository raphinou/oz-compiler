% Nesting marker in proc argument, and in if statement
local
   P PB X0 X1 X2 Y
in
   proc {P A ?B ?C}
      B=2*A
      C=4*A
   end
   proc {PB ?R}
      R=true
   end
   Y=local X in X=3 {P 3 X0 $} end
   {Show {P 2 X1 $}}
   {Show if {PB $} then {P 2 X2 $} else  {P 3 X2 $} end}
   {Show Y}
end


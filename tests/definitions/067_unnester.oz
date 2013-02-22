% Unnest the taget of a call
% --SKIP TEST--
local
   proc {MakeAdder X ?P}
      proc {P Y ?R}
         R = X + Y
      end
   end
in
   {Show {{MakeAdder 3} 4}}
end

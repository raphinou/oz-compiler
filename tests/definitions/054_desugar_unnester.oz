% Wildcard _ for argument set by procedure.
local
   P V
in
   proc{P N ?R1 ?R2}
      R1=1000+N
      R2=1000-N
   end
   {Show {P 100 $ _}}
   {Show {P 100 _ $}}
end


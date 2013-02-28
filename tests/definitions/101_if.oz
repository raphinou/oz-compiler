% Complex if statements and expressions, with condition and branches non-elementary
% Also tests comparators < =< > >=
local
   R A=0 B=1 C=2
   F = fun {$ P1 P2}
         P1*P2
       end
in

R=if {F B C}+2>={F A B}*2 then
   t
else
   f
end
{Show R}

% {F A B} = 0
% {F B C} = 2
{Show  if {F A B}>{F B C} then t else f end }
{Show  if {F A B}>={F B C} then t else f end }
{Show  if {F A B}=<{F B C} then t else f end }
{Show  if {F A B}<{F B C} then t else f end }
{Show  if {F A B}\={F B C} then t else f end }
{Show  if {F A B}=={F B C} then t else f end }

{Show  if {F A B}*{F B C}==0 then if A==0 then tt else tf end  else if A==0 then ft else ff end  end }
{Show  if {F A B}*{F B C}==0 then if A==1 then tt else tf end  else if A==0 then ft else ff end  end }
{Show  if {F A B}*{F B C}\=0 then if A==0 then tt else tf end  else if A==0 then ft else ff end  end }
{Show  if {F A B}*{F B C}\=0 then if A==0 then tt else tf end  else if A==1 then ft else ff end  end }
end



% Josephus problem with streams
local
   Pipeline
   Josephus
   Os Os
in
   fun {Pipeline F Xs L H}
      if L=<H then Ys in
         Ys={F Xs L}
         {Pipeline F Ys L+1 H}
      else /* L>H */
         Xs
      end
   end

   proc {Josephus N K}
      fun {Victim Xs I}
         case Xs
         of kill(X S)|Xr then
            if S==1 then {Show I} nil
            elseif X mod K == 0 then
               kill(X+1 S-1)|Xr
            else
               kill(X+1 S)|{Victim Xr I}
            end
         [] nil then
            nil
         end
      end Is Os
   in
      Os={Pipeline
          fun {$ Xs I} thread {Victim Xs I} end end
          Is 1 N}
      Is=kill(1 N)|Os
   end


   {Josephus 40 3}
end


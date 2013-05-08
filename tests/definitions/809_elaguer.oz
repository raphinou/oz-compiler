% Numbers list manipulation
local
   Elaguer
   MNeg
   MAdd
   Zeroes
in
   fun {MNeg Xs}
      case Xs
      of nil then nil
      [] X|Xr then ~X|{MNeg Xr}
      end
   end

   fun {MAdd Xs Ys}
      case Xs#Ys
      of nil#Ys then Ys
      [] Xs#nil then Xs
      [] (X|Xr)#(Y|Yr) then (X+Y)|{MAdd Xr Yr}
      end
   end

   fun {Zeroes Xs}
      case Xs of nil then true
      [] X|Xr then
         if X\=0 then false else {Zeroes Xr} end
      end
   end
   fun {Elaguer Xs}
      if {Zeroes Xs} then nil
      elsecase Xs of X|Xr then
         X|{Elaguer Xr}
      end
   end

   {Show {MNeg [3 4 ~3]}}

   {Show {MAdd [3 5 2] [8 8]}}

   {Show {Elaguer [3 8 4 0 4 0 0]}}

   {Show {Elaguer [0 0 0 0 0]}}

   {Show {Elaguer {MAdd [2 3] {MNeg [2 3]}}}}
end

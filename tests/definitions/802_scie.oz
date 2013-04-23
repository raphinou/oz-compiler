% Scie test
local
   fun {Scie Xs Ys}
      fun {ScieLoop Xs Is}
        case Xs
        of nil then nil
        [] X|Xr then
          case Is
          of nil then
            X*Ys.1|{ScieLoop Xr Ys.2}
          [] I|Ir then
            X*I|{ScieLoop Xr Ir}
          end
        end
      end
   in
      {ScieLoop Xs Ys}
   end
in
   {Show {Scie [1 2 3 4 5 6] [10 100 1000]}}
end

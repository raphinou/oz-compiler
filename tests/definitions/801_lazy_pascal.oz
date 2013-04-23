% Lazy computation of Pascal triangle
local
   ShiftLeft
   ShiftRight
   AddList
   LiftToStream2
   LiftToStream1
   Ps
   in

fun {ShiftLeft L}
   case L of nil then [0]
   []  H|T then H|{ShiftLeft T} end
end

fun {ShiftRight L} 0|L end

fun {AddList L1 L2}
   case L1#L2 of (H1|T1)#(H2|T2) then
      (H1+H2)|{AddList T1 T2}
   [] nil#nil then nil
   end
end

fun {LiftToStream2 F Xs Ys}
   fun lazy {Loop Xs Ys}
      case Xs#Ys of (X|Xr)#(Y|Yr) then {F X Y}|{Loop Xr Yr}
      [] nil#nil then nil
      end
   end
in
   thread {Loop Xs Ys} end
end

fun {LiftToStream1 F Xs}
   fun lazy {Loop Xs}
      case Xs of (X|Xr) then {F X}|{Loop Xr}
      [] nil then nil
      end
   end
in
   thread {Loop Xs} end
end

Ps= [1] | {LiftToStream2 AddList
      {LiftToStream1 ShiftLeft Ps}
      {LiftToStream1 ShiftRight Ps}}
{ForAll {List.take Ps 15} Show}


end

% Lisser code
local
   Sum
   PrendK
   Lisser
   PrendKS
   LisserLarge
in
   fun {Sum L Acc}
      case L of nil then Acc
      [] H|T then {Sum T H+Acc} end
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Solution a la premiere partie

   % Prend les premiers K elements de la liste L
   % Si possible, renvoie la sousliste
   % Si pas possible, renvoie notpossible
   fun {PrendK K L}
      SL={List.take L K}
   in
      if {Length SL}<K then notpossible else SL end
   end

   fun {Lisser K L}
      SL={PrendK K L}
   in
      if SL==notpossible then nil
      else ({Sum SL 0.0}/{IntToFloat K})|{Lisser K L.2} end
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Solution a la seconde partie

   % Prend K elements, avec placement en multiples de S
   % On suppose que c'est toujours possible
   fun {PrendKS K S L}
      if K==1 then [L.1]
      else Reste={List.drop L S} in
         L.1|{PrendKS K-1 S Reste}
      end
   end

   fun {LisserLarge K S L}
      if {Length L}<(K-1)*S+1 then nil
      else SL={PrendKS K S L} in
         ({Sum SL 0.0}/{IntToFloat K})|{LisserLarge K S L.2}
      end
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Exemples avec resultats corrects

   {Show {Lisser 1 [2.0 3.0 4.0]}} % [2.0 3.0 4.0]
   {Show {Lisser 2 [2.0 3.0 4.0]}} % [2.5 3.5]
   {Show {Lisser 3 [2.0 3.0 4.0]}} % [3.0]
   {Show {Lisser 4 [2.0 3.0 4.0]}} % nil

   {Show {Lisser 3 [1.0 1.0 2.0 3.0 5.0 8.0]}} % [1.33 2.0 3.33 5.33]

   {Show {LisserLarge 3 1 [1.0 1.0 2.0 3.0 5.0 8.0]}} % [1.33 2.0 3.33 5.33]
   {Show {LisserLarge 3 2 [1.0 1.0 2.0 3.0 5.0 8.0]}} % [2.67 4.0]
   {Show {LisserLarge 3 3 [1.0 1.0 2.0 3.0 5.0 8.0]}} % nil
end

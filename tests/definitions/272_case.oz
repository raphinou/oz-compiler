% Parameter arguments test for open record with atom features and not integers
local
   GetName
   Name
in
   fun {GetName person(name:Name ...)}
     Name
   end

   Name={GetName person(country:uk firstname:alan name:turing )}
   {Show Name}
end


% Simple try catch expression
local
   V
in

   V=try raise testException end 5
   catch otherException then other
     [] testException then whatWeWant
     end
   {Show V}
end


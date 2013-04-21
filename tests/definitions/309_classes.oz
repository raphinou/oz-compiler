% Wildcard in feature
local
   ApartmentC AptC MyAptC1 Apt1 Apt2 Apt3 Apt4
in
   class ApartmentC from BaseObject
      meth init skip end
   end
   class AptC from ApartmentC
      feat
         streetName: york
         streetNumber:100
         wallColor:white
         floorSurface:wood
   end
   class MyAptC1 from ApartmentC
      feat streetName:f(_)
   end
   Apt1 = {New MyAptC1 init}
   Apt2 = {New MyAptC1 init}
   Apt1.streetName = f(york)
   {Show Apt1.streetName}
   {Show Apt2.streetName}
end
